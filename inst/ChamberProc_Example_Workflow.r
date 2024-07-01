##_____________________________________________________________________________________

###                             CHAMBER MEASUREMENTS WITH PICARRO
##_____________________________________________________________________________________


# Install necessary packages ----------------------------------------------
# install.packages("devtools")
# library(devtools)
# devtools::install_github("bgctw/RespChamberProc")
#devtools::install_github("michaelstrupler/ChamberProc")
# #
# pck <- c("rlang", "changepoint", "nlme", "segmented", "tibble",  "dplyr", "purrr", "RespChamberProc","elevatr","openmeteo")
#
# new_pck <-pck[!pck %in% installed.packages()[, "Package"]]
#           if(length(new_pck)){install.packages(new_pck)}
#
# sapply(pck, require, character.only=TRUE)

# load libraries ----------------------------------------------------------
library(ChamberProc)
library(RespChamberProc)
library(rlang)
library(changepoint)
library(nlme)
library(segmented)
library(tibble)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(data.table)
library(lubridate)
library(tidyr)
library(doSNOW)
library(furrr)
library(elevatr)
library(sf)
library(openmeteo) #retrieves historical weather data from https://open-meteo.com/en/docs/historical-weather-api


# Setup -------------------------------------------------------------------
# ## Set working directory
# ## Set working directory
setwd("/Users/ms/Research Projects/Espana/UCO_Chamber/1_Test_newPackage_ChamberProc")


latDeciDeg <- 37.91514875822371 #enter here latitude in Geographical Coordinates (decimal degrees); crs =4326)
lonDeciDeg <- -4.72405536319233 #enter here longitude in Decimal Degrees

fileName <- "JFAADS2174-20220531-080209-DataLog_User.dat" #set here file name
data_pathname <- "data/" #"Data_Adrian/Agosto/19/"
results_pathname <- ""

#create folder with the name of the measurement archive to save results and plots therein
dir.create(paste0(results_pathname,"results"))

results_dir <- paste0(results_pathname,"results/",str_sub(fileName,end=-18))
dir.create(results_dir)
# fit chambers in parallel inside calcClosedChamberFluxForChunkSpecs
plan(multisession, workers = 4)


# Read and prepare data ---------------------------------------------------

ds0 <- sample_PICARRO_data

#fread(paste0(data_pathname,fileName), sep ="auto")


ds0$TIMESTAMP <- as.POSIXct(paste0(ds0$DATE," ",ds0$TIME), "%Y-%m-%d %H:%M:%S", tz= "UTC")
## The logger is accumulating data from previous field campaigns.
#  Here we subset data from a given field campaign. (Entrar hora del inicio de observationes y fin (convert to UTC))
ds_subset <- subset(ds0, as.numeric(TIMESTAMP) >= as.numeric(RespChamberProc::as.POSIXctUTC("2022-05-31 08:00:00")) )
ds_subset <- subset(ds_subset, as.numeric(TIMESTAMP) <= as.numeric(RespChamberProc::as.POSIXctUTC("2022-05-31 09:30:00" )) )

#remove columns that are not needed for further calculations
ds <- ds_subset %>% select(.,-c(DATE,TIME,FRAC_DAYS_SINCE_JAN1,FRAC_HRS_SINCE_JAN1,JULIAN_DAYS,EPOCH_TIME,ALARM_STATUS,INST_STATUS,CHAMBER_TEMP_sync,CHAMBER_PRESSURE_sync,SWITCH_sync,SOIL_TEMP_sync))


# Correct gases -----------------------------------------------------------

Additional_Weather_Data <- getAdditionalWeatherVariables(latDeciDeg, lonDeciDeg,format(min(ds$TIMESTAMP),"%Y-%m-%d"),format(max(ds$TIMESTAMP),"%Y-%m-%d"))

# For the case of PICARRO IRGA gives dry mole fractions for CO2, N2O, CH4, but not for NH3 and H2O
ds$solenoid_valvesInt<-  ds$solenoid_valves %>% as.integer()
ds$H2Oppt <- ds$H2O*10 # H2O from PICARRO is in %.Needs to be in ppt --> We need to multiply by 10
ds$N2O_dry <- ds$N2O_dry1min
ds$NH3_dry <- 10E-3*corrConcDilution(ds, colConc = "NH3", colVapour = "H2Oppt")  #NH3 from PICARRO is in ppb --> multiply colVapour by 10^-3 to get ppm
ds$H2O_dry <- corrConcDilution(ds, colConc = "H2Oppt", colVapour = "H2Oppt")

## (h, Pa, and AirTemp are obtained directly from freely available Meteo and Elevation data for given coordinates during the measurement time interval with the "getElevationAndAtmosphericPressure" script (see details therein). If desired, it can also be set here manually)
# ds$h = 106 # Córdoba elevation (m above sea level)
# ds$Pa <- 101325*(1 - 2.25577*10^-5*h)^5.25588   # (Pa)
# ds$AirTemp <- 25 #(degrees Celsius)

ds <- ds[!duplicated(ds$TIMESTAMP),] # extract duplicated rows

# interpolate measurement timestamps for whole dataset
##create  continuous timestamp vector (here: interval=1 second)
regular_timesteps <- seq(min(ds$TIMESTAMP),max(ds$TIMESTAMP), by="1 sec")

collar_df <- tibble("TIMESTAMP"=ds$TIMESTAMP,"collar"=ds$solenoid_valvesInt)

ds <-convertToRegularTimesteps(ds,c("CO2_dry", "CH4_dry","H2Oppt", "NH3_dry","N2O_dry"),regular_timesteps)
ds <- left_join(ds,collar_df,by=join_by("TIMESTAMP"=="TIMESTAMP")) %>% fill(.,collar,.direction="down")

nrow(ds_subset)
nrow(ds)

#-- In order to process each measurement cycle independently,
#-- we first determine parts of the time series that are contiguous,
#-- i.e. without gaps and without change of an index variable, here variable collar.


ds$TIMESTAMP_hour <- floor_date(ds$TIMESTAMP, unit = "hour") #create a column with a timestamp roundet to the hour, in order to join with hourly evironmental data

#join Temp_PressureData with ds  (approx does not work for more following NA's, just fill nas with same value as last one (function "fill" from tidyr package))
ds <- left_join(ds,Additional_Weather_Data ,by=join_by("TIMESTAMP_hour"=="DATETIME_hourly")) %>% fill(.,h,AirTemp,Pa,rel_humidity,shortwave_radiation,ET0, .direction = "up")

ds$VPD <- calcVPD( ds$AirTemp, ds$Pa, ds$H2Oppt) ## Here we calculate plant-to-air vapour pressure deficit

## get an overview of the data (for the whole subset)
p_collar <- plot(ds$TIMESTAMP,ds$collar, pch = ".", ylab = "collar (Chamber)",xlab = "Time")
p_collar

### facet plot of time series (for whole subset)
ds_gas_long <- gather(ds, key="gas", value="value", c("CO2_dry","H2Oppt","CH4_dry","NH3_dry","N2O_dry"))
p_gas_facet <- ggplot(ds_gas_long, aes(x=TIMESTAMP, y=value))+
  ggtitle(format(ds$TIMESTAMP,"%d/%m/%Y")[1])+
  geom_point(pch = ".")+
  facet_wrap(~factor(gas,levels=c("CO2_dry","H2Oppt","CH4_dry","NH3_dry","N2O_dry")),ncol=1,scales = "free")

ds_envar_long <- gather(ds, key="envar", value="value", c("AirTemp","Pa","rel_humidity","shortwave_radiation","ET0"))
p_envar_facet <-  ggplot(ds_envar_long, aes(x=TIMESTAMP, y=value))+
  ggtitle(format(ds$TIMESTAMP,"%d/%m/%Y")[1])+
  geom_point(pch = ".")+
  facet_wrap(~factor(envar,levels=c("AirTemp","Pa","rel_humidity","shortwave_radiation","ET0")),ncol=1,scales = "free")

p_gas_facet
p_envar_facet


# Chunk creation ----------------------------------------------------------
#-- In order to process each measurement cycle independently,
#-- we first determine parts of the time series that are contiguous,
#-- i.e. without gaps and without change of an index variable, here variable collar. #indexNA excludes selected index columns (here: collar). gapLength should not be too short, otherwise error
dsChunk_raw <- subsetContiguous(ds, colTime = "TIMESTAMP", colIndex = "collar",
                            gapLength = 12, minNRec = 180, minTime = 180, indexNA = 13)

# thin data (select the thinning interval, here: 8) to make calculations more efficient
dsChunk <- dsChunk_raw %>% group_by(iChunk) %>% slice(seq(1, n(), 5)) %>% ungroup()

mapped_collars <- dsChunk %>% group_by(iChunk) %>% summarise(collar = first(collar)) %>%  head()


## DataFrame collar_spec then needs to specify for each collar id in column collar,
# the colums area (m2) and volume (m3), as well a tlag (s), the lag time between start of the cycle , i.e. the start of the chunk (usually chamber closing time), and the time when the gas reaches the sensor.

chamberVol = 0.6*0.6*0.6		# chamber was a cube of 0.6m length
surfaceArea = 0.6*0.6
collar_spec <- tibble(
  collar = unique(dsChunk$collar),
  depth = 0, #pmax(0,rnorm(length(collar), mean = 0.03, sd = 0.015)),
  area = surfaceArea,
  volume = chamberVol + surfaceArea * depth,
  tlag = NA)
head(collar_spec)


# Time series plots -------------------------------------------------------
# Generate labels for each gas
labels_CO2 <- chunk_labels(dsChunk, CO2_dry, 1.05)
labels_H2O <- chunk_labels(dsChunk, H2Oppt, 1.05)
labels_CH4 <- chunk_labels(dsChunk, CH4_dry, 1.05)
labels_N2O <- chunk_labels(dsChunk, N2O_dry, 1.05)
labels_NH3 <- chunk_labels(dsChunk, NH3_dry, 1.05)

# Generate plots for each gas
p_CO2 <- chunk_plot(dsChunk, CO2_dry, labels_CO2)
p_H2O <- chunk_plot(dsChunk, H2Oppt, labels_H2O)
p_CH4 <- chunk_plot(dsChunk, CH4_dry, labels_CH4)
p_N2O <- chunk_plot(dsChunk, N2O_dry, labels_N2O)
p_NH3 <- chunk_plot(dsChunk, NH3_dry, labels_NH3)

##save the plots
# for (p in c("p_CO2","p_H2O","p_CH4","p_N2O","p_NH3")) {
#   ggsave(filename=paste0(results_dir,"/",str_sub(fileName,end=-18),"_allchunks_",p,".pdf"),get(p),width = 40,height = 30,units = "cm")
# }


# Determine fits for selected chunks and compute the flux --------------------------------------

##select just one chunk
selected_chunk=4
df<-filter(dsChunk,iChunk==selected_chunk)



resFit <- calcClosedChamberFlux(df
                                , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                , colConc = "CO2_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
                                , colTemp = "AirTemp", colPressure = "Pa"		#Temp in degC, Pressure in Pa
                                , volume = 0.4*0.4*0.4, area = 0.4*0.4
                                , minTLag = 60,  maxLag = 120
                                , concSensitivity = 0.01
)


resH2OFit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                   , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                   , colConc = "H2Oppt", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"
                                   , volume = 0.4*0.4*0.4, area = 0.4*0.4
                                   , minTLag = 120,  maxLag = 150,
                                   , concSensitivity = 0.01
)


resCH4Fit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                   , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                   , colConc = "CH4_dry", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"
                                   , volume = 0.4*0.4*0.4, area = 0.4*0.4
                                   , minTLag = 50,  maxLag = 150
                                   , concSensitivity = 0.01
)


resNH3Fit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                   , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                   , colConc = "NH3_dry", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"
                                   , volume = 0.4*0.4*0.4, area = 0.4*0.4
                                   , minTLag = 50,  maxLag = 120
                                   , concSensitivity = 0.01
)


resN2OFit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                   , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                   , colConc = "N2O_dry", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"
                                   , volume = 0.4*0.4*0.4, area = 0.4*0.4
                                   , minTLag = 120,  maxLag = 180
                                   , concSensitivity = 0.01
)

## plot the fits
plotResp(df, resFit, label = paste("Chunk",selected_chunk,sep = " "))

plotResp(df, resH2OFit,colConc = "H2Oppt",ylab="H2O (ppt)",xlab="time (Minute:Second)", label = paste("Chunk",selected_chunk,sep = " "))

plotResp(df, resCH4Fit,colConc = "CH4_dry",ylab="CH4_dry (ppm)",xlab="time (Minute:Second)", label = paste("Chunk",selected_chunk,sep = " "))

plotResp(df, resNH3Fit,colConc = "NH3_dry",ylab="NH3_dry (ppm)",xlab="time (Minute:Second)", label = paste("Chunk",selected_chunk,sep = " "))

plotResp(df, resN2OFit,colConc = "N2O_dry",ylab="N2O_dry (ppm)",xlab="time (Minute:Second)", label = paste("Chunk",selected_chunk,sep = " "))


# Calculate fluxes for all chunks -----------------------------------------
# Computing the fluxes in a field campaign --------------------------------
# -- Function calcClosedChamberFluxForChunks helps you with subsetting the data
# -- and applying function calcClosedChamberFlux to each subset.

collar_spec_CO2 <-mutate(collar_spec, tlag = 60) #One can save processing time and avoid failures in the non-robust breakpoint-detection by specifying a fixed lag-time (may differ across collars) with the collar specification.
collar_spec_H2O <-mutate(collar_spec, tlag = 60)
collar_spec_CH4 <-mutate(collar_spec, tlag = 60)
collar_spec_NH3 <-mutate(collar_spec, tlag = 60)
collar_spec_N2O <-mutate(collar_spec, tlag = 120)



nNode = 8	# number of processors
cl = makeCluster(nNode)
registerDoSNOW(cl)
clusterEvalQ(cl, library(RespChamberProc))


system.time(res <- ddply(dsChunk, .(iChunk), function(dsi){
  collar <- dsi$collar[1]
  iChunk = dsi$iChunk[1]
  print( paste(iChunk, dsi$TIMESTAMP[1], " collar: ",collar) )



res <- calcClosedChamberFluxForChunkSpecs(
      dsi, collar_spec_CO2
      , colTemp = "AirTemp", colPressure = "Pa"
      , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
      , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
      , colConc = "CO2_dry", colTime = "TIMESTAMP"
      , concSensitivity = 0.01
      , minTLag= 30
      , maxLag = 200
      )

  resH2O <- calcClosedChamberFluxForChunkSpecs(
    dsi, collar_spec_H2O
    , colTemp = "AirTemp", colPressure = "Pa"
    , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
    , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
    , colConc = "H2Oppt", colTime = "TIMESTAMP"
    , concSensitivity = 0.01
    , minTLag= 120
    , maxLag = 200
  )
#
  resCH4 <-
    calcClosedChamberFluxForChunkSpecs(
      dsi, collar_spec_CH4
      , colTemp = "AirTemp", colPressure = "Pa"
      , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
      , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
      , colConc = "CH4_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
      , concSensitivity = 0.01
      , minTLag= 60
      , maxLag = 200
    )
#
  resNH3 <- calcClosedChamberFluxForChunkSpecs(
    dsi, collar_spec_NH3
    , colTemp = "AirTemp", colPressure = "Pa"
    , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
    , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
    , colConc = "NH3_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
    , concSensitivity = 0.01
    , minTLag= 60
    , maxLag = 200
  )
#
  resN2O <- calcClosedChamberFluxForChunkSpecs(
    dsi, collar_spec_N2O
    , colTemp = "AirTemp", colPressure = "Pa"
    , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
    , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
    , colConc = "N2O_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
    , concSensitivity = 0.01
    , minTLag= 100
    , maxLag = 200
  )
#
#

  #   get additional environmental variables at the initial time
  to <- ifelse(res$tLag == 0, 1, res$tLag)
  dsiInitial <- dsi[to, , drop = FALSE]

  cbind( data.frame( time=dsiInitial[,"TIMESTAMP"], collar=collar
                     , CO2_flux=res$fluxMedian, CO2_flux_sd=res$sdFlux, Fregress_CO2=res$iFRegress, r2_CO2=res$r2
                     , H2O_flux=resH2O$fluxMedian , H2O_flux_sd=resH2O$sdFlux, Fregress_H2O=resH2O$iFRegress, r2_H2O=resH2O$r2
                     , CH4_flux=resCH4$fluxMedian, CH4_flux_sd=resCH4$sdFlux, Fregress_CH4=resCH4$iFRegress, r2_CH4=resCH4$r2
                     , NH3_flux=resNH3$fluxMedian, NH3_flux_sd=resNH3$sdFlux, Fregress_NH3=resNH3$iFRegress, r2_NH3=resNH3$r2
                     , N2O_flux=resN2O$fluxMedian , N2O_flux_sd=resN2O$sdFlux, Fregress_N2O=resN2O$iFRegress, r2_N2O=resN2O$r2
  )
  , dsiInitial[,c("CO2_dry","CH4_dry","NH3_dry","N2O_dry", "AirTemp","Pa")] )
}
))

res_CO2 <- calcClosedChamberFluxForChunkSpecs(
  dsChunk, collar_spec_CO2
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "CO2_dry", colTime = "TIMESTAMP"
  , concSensitivity = 0.01
)

res_NH3 <- calcClosedChamberFluxForChunkSpecs(
  dsChunk, collar_spec_NH3
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "NH3_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
  , concSensitivity = 0.01
)

res_CH4 <- calcClosedChamberFluxForChunkSpecs(
  dsChunk, collar_spec_CH4
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "CH4_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
  , concSensitivity = 0.01
)

res_N2O <- calcClosedChamberFluxForChunkSpecs(
  dsChunk, collar_spec_N2O
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "N2O_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
  , concSensitivity = 0.01
)

res_H2O <- calcClosedChamberFluxForChunkSpecs(
  dsChunk, collar_spec_H2O
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "H2Oppt", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
  , concSensitivity = 0.01
)


#Plot the results including the fits as facet plots
res_facets_CO2 <- plotCampaignConcSeries(ds=dsChunk,varName = "CO2_dry",dsFits = res_CO2, plotsPerPage = 64L,fileName =paste0(results_dir,"/CO2_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_CO2$plot[[1]])


res_facets_NH3 <- plotCampaignConcSeries(ds=dsChunk,varName = "NH3_dry",dsFits = res_NH3, plotsPerPage = 64L,fileName =paste0(results_dir,"/NH3_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_NH3$plot[[1]])


res_facets_CH4 <- plotCampaignConcSeries(ds=dsChunk,varName = "CH4_dry",dsFits = res_CH4, plotsPerPage = 64L,fileName =paste0(results_dir,"/CH4_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_CH4$plot[[1]])

res_facets_N2O <- plotCampaignConcSeries(ds=dsChunk,varName = "N2O_dry",dsFits = res_N2O, plotsPerPage = 64L,fileName =paste0(results_dir,"/N2O_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_N2O$plot[[1]])


res_facets_H2O <- plotCampaignConcSeries(ds=dsChunk,varName = "H2Oppt",dsFits = res_H2O, plotsPerPage = 64L,fileName =paste0(results_dir,"/H2O_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_H2O$plot[[1]])


#Duration Uncertainty for selected chunk ----------------------------------------------------
# Double chekc plotDurationUncertainty code

a <- plotDurationUncertainty(df, colTime = "TIMESTAMP",colConc = "CO2_dry",colTemp="AirTemp", volume = chamberVol,
                             fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                             ), maxSdFlux = 0.5 #this should be relative to the median (e.g. 10% von median)
                             , nDur=10
                             , durations = seq(collar_spec_CO2$tlag[1],max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),20)
)




source("plotDurationUncertaintyRelSD.R")


resDur <- plotDurationUncertaintyRelSD( df, colConc = "CO2_dry", colTemp="AirTemp", volume = chamberVol,
                                   fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                   )
                                   , maxSdFluxRel = 0.5 #this should be relative to the median (e.g. 10% von median)
                                   , nDur=10
                                   , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),20)
)
resDur$duration

plot( flux ~ duration, resDur$statAll[[1]] )
plot( sdFlux ~ duration, resDur$statAll[[1]] )

resDur_H2O <- plotDurationUncertaintyRelSD( df, colConc = "H2Oppt", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 1
                                       , nDur=10
                                       , durations = seq(120,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),10)
)

plot( flux ~ duration, resDur_H2O$statAll[[1]] )
plot( sdFlux ~ duration, resDur_H2O$statAll[[1]] )

resDur_CH4 <- plotDurationUncertaintyRelSD( df, colConc = "CH4_dry", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 0.5
                                       , nDur=10
                                       , durations = seq(50,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
)

plot( flux ~ duration, resDur_CH4$statAll[[1]] )
plot( sdFlux ~ duration, resDur_CH4$statAll[[1]] )

resDur_NH3 <- plotDurationUncertaintyRelSD( df, colConc = "NH3_dry", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 1
                                       , nDur=10
                                       , durations = seq(60,600,30)
)

plot( flux ~ duration, resDur_NH3$statAll[[1]] )
plot( sdFlux ~ duration, resDur_NH3$statAll[[1]] )

resDur_N2O <- plotDurationUncertaintyRelSD( df, colConc = "N2O_dry", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 0.5
                                       , nDur=10
                                       , durations = seq(120,600,30)
)

plot( flux ~ duration, resDur_N2O$statAll[[1]] )
plot( sdFlux ~ duration, resDur_N2O$statAll[[1]] )




