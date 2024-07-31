##_____________________________________________________________________________________

###                             CHAMBER MEASUREMENTS WITH PICARRO
##_____________________________________________________________________________________


# Install necessary packages ----------------------------------------------
# install.packages("devtools")
# library(devtools)
#devtools::install_github("bgctw/RespChamberProc", ref="durationplo2")
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


latDeciDeg_site1 <- 37.91514875822371 #enter here latitude in Geographical Coordinates (decimal degrees); crs =4326)
lonDeciDeg_site1 <- -4.72405536319233 #enter here longitude in Decimal Degrees

latDeciDeg_site2 <- 37.913511
lonDeciDeg_site2 <- -4.721578

#Data Jesus
fileName <- "JFAADS2174-20220524-074601-DataLog_User.dat" #"JFAADS2174-20220523-110548-DataLog_User.dat" #set here file name
fileName2 <- "JFAADS2174-20220601-102835-DataLog_User.dat"
fileName3 <-  "JFAADS2174-20221129-091921-DataLog_User.dat"#"JFAADS2174-20220601-102835-DataLog_User.dat"
fileName4 <-"JFAADS2174-20221130-091721-DataLog_User.dat"
#fileName5 <-

#Data Adrian
#fileName5 <-"JFAADS2174-20230820-000011-DataLog_User.dat"

data_pathname <-"/Users/ms/Research Projects/Espana/UCO_Chamber/Data_Jesus/" #"data/" #"Data_Adrian/Agosto/19/"
data_pathname2 <- "/Users/ms/Research Projects/Espana/UCO_Chamber/Data_Adrian/Agosto/19/"
results_pathname <- ""

#create folder with the name of the measurement archive to save results and plots therein
dir.create(paste0(results_pathname,"results"))

results_dir <- paste0(results_pathname,"results/publication24")
dir.create(results_dir)





# Read and prepare data ---------------------------------------------------
# read and combine measurement data from multiple datasets
## data Jesus
ds0 <-fread(paste0(data_pathname,fileName), sep ="auto") #sample_PICARRO_data
ds1 <-fread(paste0(data_pathname,fileName2), sep ="auto")
ds2 <-fread(paste0(data_pathname,fileName3), sep ="auto")
ds3 <-fread(paste0(data_pathname,fileName4), sep ="auto")
#s4 <-fread(paste0(data_pathname2,fileName5), sep ="auto")


#data Adrian



#create a timestamp for each dataset
ds0$TIMESTAMP <- as.POSIXct(paste0(ds0$DATE," ",ds0$TIME), "%Y-%m-%d %H:%M:%S", tz= "UTC")
ds1$TIMESTAMP <- as.POSIXct(paste0(ds1$DATE," ",ds1$TIME), "%Y-%m-%d %H:%M:%S", tz= "UTC")
ds2$TIMESTAMP <- as.POSIXct(paste0(ds2$DATE," ",ds2$TIME), "%Y-%m-%d %H:%M:%S", tz= "UTC")
ds3$TIMESTAMP <- as.POSIXct(paste0(ds3$DATE," ",ds3$TIME), "%Y-%m-%d %H:%M:%S", tz= "UTC")
#ds4$TIMESTAMP <- as.POSIXct(paste0(ds4$DATE," ",ds4$TIME), "%Y-%m-%d %H:%M:%S", tz= "UTC")


#subset datasets
ds0_subset <- ds0 %>% filter(between(as.numeric(TIMESTAMP),as.numeric(RespChamberProc::as.POSIXctUTC("2022-05-24 08:00:00")), as.numeric(RespChamberProc::as.POSIXctUTC("2022-05-24 12:00:00" ))))
ds1_subset <- ds1 %>% filter(between(as.numeric(TIMESTAMP),as.numeric(RespChamberProc::as.POSIXctUTC("2022-06-01 10:46:00")), as.numeric(RespChamberProc::as.POSIXctUTC("2022-06-01 12:45:00" ))))
ds2_subset <- ds2 %>% filter(between(as.numeric(TIMESTAMP),as.numeric(RespChamberProc::as.POSIXctUTC("2022-11-29 09:46:00")), as.numeric(RespChamberProc::as.POSIXctUTC("2022-11-29 13:40:00" ))))
ds3_subset <- ds3 %>% filter(between(as.numeric(TIMESTAMP),as.numeric(RespChamberProc::as.POSIXctUTC("2022-11-30 09:36:00")), as.numeric(RespChamberProc::as.POSIXctUTC("2022-11-30 13:30:00" ))))
#ds4_subset <- ds4 %>% filter(between(as.numeric(TIMESTAMP),as.numeric(RespChamberProc::as.POSIXctUTC("2023-08-20 00:00:01")), as.numeric(RespChamberProc::as.POSIXctUTC("2023-08-20 12:00:00" ))))


# Correct gases -----------------------------------------------------------
## all this is done for each dataset subset with the function "process_dataset" (correction for gases and add additional environmental variables)
source("inst/process_dataset.R")

ds0_processed <- process_dataset(ds0_subset,latDeciDeg_site1,lonDeciDeg_site1)
ds1_processed <- process_dataset(ds1_subset,latDeciDeg_site1,lonDeciDeg_site1)
ds2_processed <- process_dataset(ds2_subset,latDeciDeg_site1,lonDeciDeg_site1)
ds3_processed <- process_dataset(ds3_subset,latDeciDeg_site1,lonDeciDeg_site1)
#ds4_processed <- process_dataset(ds4_subset,latDeciDeg_site2,lonDeciDeg_site2)

#merge the loaded datasets and remove columns that are not needed for further calculations
ds_merged <-rbind(ds0_processed,ds1_processed,ds2_processed,ds3_processed) #ds2_processed,ds3_processed
ds <- ds_merged

#save ds_merged
saveRDS(ds_merged,paste0(results_dir,"/ds_merged.Rds"))

#ds_merged_location2 <-rbind(ds0,ds1)


ds <- readRDS(paste0(results_dir,"/ds_merged.Rds"))


## get an overview of the data (for the whole subset)
p_collar <- plot(ds3_processed$TIMESTAMP,ds3_processed$collar, pch = ".", ylab = "collar (Chamber)",xlab = "Time")
p_collar

### facet plot of time series (for selected subset; enter the ds of interest here)
ds_gas_long <- gather(ds3_processed, key="gas", value="value", c("CO2_dry","H2Oppt","CH4_dry","NH3_dry","N2O_dry"))
p_gas_facet <- ggplot(ds_gas_long, aes(x=TIMESTAMP, y=value))+
  ggtitle(format(ds3_processed$TIMESTAMP,"%d/%m/%Y")[1])+
  geom_point(pch = ".")+
  facet_wrap(~factor(gas,levels=c("CO2_dry","H2Oppt","CH4_dry","NH3_dry","N2O_dry")),ncol=1,scales = "free")

ds_envar_long <- gather(ds3_processed, key="envar", value="value", c("AirTemp","Pa","rel_humidity","shortwave_radiation","ET0"))
p_envar_facet <-  ggplot(ds_envar_long, aes(x=TIMESTAMP, y=value))+
  ggtitle(format(ds3_processed$TIMESTAMP,"%d/%m/%Y")[1])+
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
dsChunk <-dsChunk_raw %>% group_by(iChunk) %>% slice(seq(1, n(), 2)) %>% ungroup()

mapped_collars <- dsChunk %>% group_by(iChunk) %>% summarise(collar = first(collar)) %>%  head()

#assign chunks to seasons and grazing states

dsChunk <- dsChunk %>% mutate(season = case_when(
  month(TIMESTAMP) %in% c(5,6,7,8) ~ 'summer',
  month(TIMESTAMP) %in% c(9,10,11) ~ 'autumn',
  TRUE ~ NA_character_  # Assign NA for other months
), grazing_state = case_when(
  day(TIMESTAMP) %in% c(24,29) ~'not grazed',
  day(TIMESTAMP) %in% c(01,30) ~'grazed',
  TRUE ~ NA_character_  # Assign NA for other months
))




## DataFrame collar_spec then needs to specify for each collar id in column collar,
# the colums area (m2) and volume (m3), as well a tlag (s), the lag time between start of the cycle , i.e. the start of the chunk (usually chamber closing time), and the time when the gas reaches the sensor.

chamberVol = 0.475*0.475*0.794		# data jesus
surfaceArea = 0.475*0.475

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
                                , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
                                , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                , colConc = "CO2_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
                                , colTemp = "AirTemp", colPressure = "Pa"		#Temp in degC, Pressure in Pa
                                , volume = chamberVol, area = surfaceArea
                                , minTLag = 60,  maxLag = 120
                                , concSensitivity = 0.05
)


resH2OFit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
                                   , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                   , colConc = "H2Oppt", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"
                                   , volume = chamberVol, area = surfaceArea
                                   , minTLag = 120,  maxLag = 150,
                                   , concSensitivity = 0.05
)


resCH4Fit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
                                   , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                   , colConc = "CH4_dry", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"
                                   , volume = chamberVol, area = surfaceArea
                                   , minTLag = 50,  maxLag = 150
                                   , concSensitivity = 0.05
)


resNH3Fit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
                                   , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                   , colConc = "NH3_dry", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"
                                   , volume = chamberVol, area = surfaceArea
                                   , minTLag = 50,  maxLag = 120
                                   , concSensitivity = 0.05
)


resN2OFit <- calcClosedChamberFlux(df
                                   , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
                                   , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                   , colConc = "N2O_dry", colTime = "TIMESTAMP"
                                   , colTemp = "AirTemp", colPressure = "Pa"
                                   , volume = chamberVol, area = surfaceArea
                                   , minTLag = 120,  maxLag = 180
                                   , concSensitivity = 0.05
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

collar_spec_CO2 <-mutate(collar_spec, tlag = 90) #One can save processing time and avoid failures in the non-robust breakpoint-detection by specifying a fixed lag-time (may differ across collars) with the collar specification.
collar_spec_H2O <-mutate(collar_spec, tlag = 60)
collar_spec_CH4 <-mutate(collar_spec, tlag = 90)
collar_spec_NH3 <-mutate(collar_spec, tlag = 90)
collar_spec_N2O <-mutate(collar_spec, tlag = 90)


dsChunkSummer <- dsChunk %>% filter(season=='summer')
dsChunkAutumn <- dsChunk %>% filter(season=='autumn')

res_CO2_Summer <- calcClosedChamberFluxForChunkSpecs(
  dsChunkSummer, collar_spec_CO2
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "CO2_dry", colTime = "TIMESTAMP"
  , concSensitivity = 0.05
)

res_CO2_Autumn <- calcClosedChamberFluxForChunkSpecs(
  dsChunkAutumn, collar_spec_CO2
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "CO2_dry", colTime = "TIMESTAMP"
  , concSensitivity = 0.05
)



res_CO2 <- calcClosedChamberFluxForChunkSpecs(
  dsChunk, collar_spec_CO2
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "CO2_dry", colTime = "TIMESTAMP"
  , concSensitivity = 0.05
)


res_NH3 <- calcClosedChamberFluxForChunkSpecs(
  dsChunk, collar_spec_NH3
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "NH3_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
  , concSensitivity = 0.05
)

res_CH4 <- calcClosedChamberFluxForChunkSpecs(
  dsChunk, collar_spec_CH4
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "CH4_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
  , concSensitivity = 0.05
)

res_N2O <- calcClosedChamberFluxForChunkSpecs(
  dsChunk, collar_spec_N2O
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "N2O_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
  , concSensitivity = 0.05
)

res_H2O <- calcClosedChamberFluxForChunkSpecs(
  dsChunk, collar_spec_H2O
  , colTemp = "AirTemp", colPressure = "Pa"
  , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp)
  , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
  , colConc = "H2Oppt", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
  , concSensitivity = 0.05
)


# Only select chunks that have a r2<=0.8
res_CO2_r2smaller0_8 <- res_CO2 %>% filter(r2>=0.8)
res_NH3_r2smaller0_8 <- res_NH3 %>% filter(r2>=0.8)
res_H2O_r2smaller0_8 <- res_H2O %>% filter(r2>=0.8)
res_CH4_r2smaller0_8 <- res_CH4 %>% filter(r2>=0.8)
res_N2O_r2smaller0_8 <- res_N2O %>% filter(r2>=0.8)




#for each Gas, identify which chunks are bad and exclude them
dsChunk[dsChunk$iChunk %in% res_CO2_r2smaller0_8$iChunk,]
dsChunk[dsChunk$iChunk %in% res_NH3_r2smaller0_8$iChunk,]
dsChunk[dsChunk$iChunk %in% res_H2O_r2smaller0_8$iChunk,]
dsChunk[dsChunk$iChunk %in% res_CH4_r2smaller0_8$iChunk,]
dsChunk[dsChunk$iChunk %in% res_N2O_r2smaller0_8$iChunk,]



#Plot the results including the fits as facet plots
# res_facets_CO2 <- plotCampaignConcSeries(ds=dsChunk,varName = "CO2_dry",dsFits = res_CO2, plotsPerPage = 64L,fileName =paste0(results_dir,"/CO2_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
# print( res_facets_CO2$plot[[1]])

res_facets_CO2_summer <- plotCampaignConcSeries(ds=dsChunkSummer,varName = "CO2_dry",dsFits = res_CO2_Summer, plotsPerPage = 64L,fileName =paste0(results_dir,"/CO2_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_CO2_summer$plot[[1]])

res_facets_CO2_autumn <- plotCampaignConcSeries(ds=dsChunkAutumn,varName = "CO2_dry",dsFits = res_CO2_Autumn, plotsPerPage = 64L,fileName =paste0(results_dir,"/CO2_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_CO2_autumn$plot[[1]])

res_facets_CO2_r2_0_8 <- plotCampaignConcSeries(ds=dsChunk[dsChunk$iChunk %in% res_CO2_r2smaller0_8$iChunk,],varName = "CO2_dry",dsFits = res_CO2_r2smaller0_8, plotsPerPage = 64L,fileName =paste0(results_dir,"/CO2_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_CO2_r2_0_8$plot[[1]])



# res_facets_NH3 <- plotCampaignConcSeries(ds=dsChunk,varName = "NH3_dry",dsFits = res_NH3, plotsPerPage = 64L,fileName =paste0(results_dir,"/NH3_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
# print( res_facets_NH3$plot[[1]])

res_facets_NH3_r2_0_8 <- plotCampaignConcSeries(ds=dsChunk[dsChunk$iChunk %in% res_NH3_r2smaller0_8$iChunk,],varName = "NH3_dry",dsFits = res_NH3_r2smaller0_8, plotsPerPage = 64L,fileName =paste0(results_dir,"/NH3_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_NH3_r2_0_8$plot[[1]])

# res_facets_CH4 <- plotCampaignConcSeries(ds=dsChunk,varName = "CH4_dry",dsFits = res_CH4, plotsPerPage = 64L,fileName =paste0(results_dir,"/CH4_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
# print( res_facets_CH4$plot[[1]])

res_facets_CH4_r2_0_8 <- plotCampaignConcSeries(ds=dsChunk[dsChunk$iChunk %in% res_CH4_r2smaller0_8$iChunk,],varName = "CH4_dry",dsFits = res_CH4_r2smaller0_8, plotsPerPage = 64L,fileName =paste0(results_dir,"/CH4_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_CH4_r2_0_8$plot[[1]])



res_facets_N2O <- plotCampaignConcSeries(ds=dsChunk,varName = "N2O_dry",dsFits = res_N2O, plotsPerPage = 64L,fileName =paste0(results_dir,"/N2O_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_N2O$plot[[1]])

res_facets_N2O_r2_0_8 <- plotCampaignConcSeries(ds=dsChunk[dsChunk$iChunk %in% res_N2O_r2smaller0_8$iChunk,],varName = "N2O_dry",dsFits = res_N2O_r2smaller0_8, plotsPerPage = 64L,fileName =paste0(results_dir,"/N2O_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_N2O_r2_0_8$plot[[1]])


# res_facets_H2O <- plotCampaignConcSeries(ds=dsChunk,varName = "H2Oppt",dsFits = res_H2O, plotsPerPage = 64L,fileName =paste0(results_dir,"/H2O_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
# print( res_facets_H2O$plot[[1]])

res_facets_H2O_r2_0_8 <- plotCampaignConcSeries(ds=dsChunk[dsChunk$iChunk %in% res_H2O_r2smaller0_8$iChunk,],varName = "H2Oppt",dsFits = res_H2O_r2smaller0_8, plotsPerPage = 64L,fileName =paste0(results_dir,"/H2O_fit_facets.pdf")) #fileName =paste0(results_dir,"/CO2_fit_facets.pdf") )
print( res_facets_H2O_r2_0_8$plot[[1]])




# find optimal model fit for each gas -------------------------------------

res_CO2 <- res_CO2 %>% mutate(gas = "CO2")
res_CH4 <- res_CH4 %>% mutate(gas = "CH4")
res_H2O <- res_H2O %>% mutate(gas = "H2O")
res_N2O <- res_N2O %>% mutate(gas = "N2O")
res_NH3 <- res_NH3 %>% mutate(gas = "NH3")



# Combine the data frames
combined_res <- bind_rows(res_CO2, res_CH4, res_H2O,res_N2O,res_NH3)
combined_res <- combined_res %>% mutate(iFRegress = factor(iFRegress),CV=abs(sdFlux/fluxMedian))


#select only good values
combined_res <- combined_res %>% filter(r2>=0.8)

CV_comparison_gas <- combined_res %>% group_by(gas) %>% summarize(mean_CV=mean(CV))

#save results as RDS
saveRDS(combined_res %>% select(-c(times,model)),file=paste0(results_dir,"/results.rds"),compress = T)
#save results as .csv
write.csv(combined_res %>% select(-c(times,model)),file=paste0(results_dir,"/results.txt"))


#Research question:if measurement for gas x is good for one chunk, does that mean that for gas y and for gas z it is also good?
a <- combined_res %>% select(iChunk,r2,gas) %>%  group_by(gas)

# Convert to wide format
r2_data <- a %>%
  pivot_wider(names_from = gas, values_from = r2)

ggplot(r2_data)+
  geom_point(aes(CO2,CH4),color="blue")+
  geom_point(aes(CO2,H2O),color="red")+
  geom_point(aes(CO2,NH3),color="green")+
  labs(y="r2 CH4(blue)/H2O(red)/NH3(green)",x="r2 CO2")

plot(r2_data$CO2,r2_data$CH4)


# Plot the histograms of the used Models with facet wrap
ggplot(combined_res, aes(x = iFRegress, fill = gas)) +
  geom_bar(alpha = 0.7) +
  facet_wrap(~ gas, scales = "free_y") +
  scale_x_discrete(name = "Model") +
  scale_y_continuous(name = "Count", breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "Histograms of CO2, CH4, H2O, NH3,and N2O") +
  theme_minimal()+
  theme(legend.position = "none")

# Plot Flux vs. Window Duration (WD)



# #Duration Uncertainty for selected chunk ----------------------------------------------------

findStabilizationPoint <- function(x, y) {
  # Fit a smooth curve to the data using loess
  fit <- loess(y ~ x)

  # Predict y values using the fitted model
  y_fit <- predict(fit, x)

  # Calculate the first derivative using finite differences
  dy_dx <- diff(y_fit) / diff(x)

  # Calculate the 25th quantile of the absolute derivative values
  quantile_25 <- quantile(abs(dy_dx), 0.25)

  # Find the index where the derivative is closest to the 25th quantile
  stabilize_point_index <- which.min(abs(abs(dy_dx) - quantile_25))

  # Adjust index for comparison since diff reduces length by 1
  stabilize_x <- x[stabilize_point_index + 1]
  stabilize_y <- y_fit[stabilize_point_index + 1]

  # Plot the original data and the fitted curve
  plot(x, y, main = "Curve Stabilization Point", xlab = "Duration", ylab = "Flux or Sd or CV", pch = 16, col = "blue")
  lines(x, y_fit, col = "red", lwd = 2)

  # Mark the stabilization point
  points(stabilize_x, stabilize_y, col = "green", pch = 19, cex = 1.5)
  text(stabilize_x, stabilize_y, labels = paste("Stabilizes at x =", round(stabilize_x, 2)), pos = 4)

  # Optionally plot the derivative to visualize stabilization
  plot(x[-length(x)], dy_dx, type = "l", main = "First Derivative", xlab = "X", ylab = "dy/dx", col = "purple")
  abline(h = 0, col = "gray", lty = 2)
  points(stabilize_x, dy_dx[stabilize_point_index], col = "green", pch = 19)
  text(stabilize_x, dy_dx[stabilize_point_index], labels = paste("Stabilizes at x =", round(stabilize_x, 2)), pos = 4)

  return(tibble(Duration_stab = stabilize_x, CV_stab = stabilize_y))
}


##select just one chunk
selected_chunk=4 #error when selected chunk==7: error in `gls()`:! false convergence (8) if exponential model or polynomial is used
df<-filter(dsChunk,iChunk==selected_chunk)
#
resDur <- plotDurationUncertaintyRelSD( df, colConc = "CO2_dry", colTemp="AirTemp", volume = chamberVol,
                                   fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh)
                                   , maxSdFluxRel = 1
                                   , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),20)
)

#
plot( flux ~ duration, resDur$statAll[[1]] )
#plot( abs(sdFlux/fluxMedian) ~ duration, resDur$statAll[[1]])
plot( sdFlux ~ duration, resDur$statAll[[1]] )

# Find optimal window duration (findStabilizationPoint Function)
resDur_CV <- resDur$statAll[[1]]$sdFlux / resDur$statAll[[1]]$fluxMedian
resDur_duration <- resDur$statAll[[1]]$duration
findStabilizationPoint(resDur_duration,  resDur$statAll[[1]]$flux )


resDur_H2O <- plotDurationUncertaintyRelSD( df, colConc = "H2Oppt", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp
                                       )
                                       , maxSdFluxRel = 1
                                       , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
)
plot( flux ~ duration, resDur_H2O$statAll[[1]] )
plot( sdFlux ~ duration, resDur_H2O$statAll[[1]] )

# Find optimal window duration (findStabilizationPoint Function)
resDur_H2O_CV <- resDur_H2O$statAll[[1]]$sdFlux / resDur_H2O$statAll[[1]]$fluxMedian
resDur_H2O_duration <- resDur_H2O$statAll[[1]]$duration
findStabilizationPoint(resDur_H2O_duration, resDur_H2O$statAll[[1]]$flux)

#
resDur_CH4 <- plotDurationUncertaintyRelSD( df, colConc = "CH4_dry", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 1
                                       , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
)

plot( flux ~ duration, resDur_CH4$statAll[[1]] )
plot( sdFlux ~ duration, resDur_CH4$statAll[[1]] )

# Find optimal window duration (findStabilizationPoint Function)
resDur_CH4_CV <- resDur_CH4$statAll[[1]]$sdFlux / resDur_CH4$statAll[[1]]$fluxMedian
resDur_CH4_duration <- resDur_CH4$statAll[[1]]$duration
findStabilizationPoint(resDur_CH4_duration, resDur_CH4$statAll[[1]]$flux)


#
resDur_NH3 <- plotDurationUncertaintyRelSD( df, colConc = "NH3_dry", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 1
                                       , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
)

plot( flux ~ duration, resDur_NH3$statAll[[1]] )
plot( sdFlux ~ duration, resDur_NH3$statAll[[1]] )

# Find optimal window duration (findStabilizationPoint Function)
resDur_NH3_CV <- resDur_NH3$statAll[[1]]$sdFlux / resDur_NH3$statAll[[1]]$fluxMedian
resDur_NH3_duration <- resDur_NH3$statAll[[1]]$duration
findStabilizationPoint(resDur_NH3_duration, resDur_NH3$statAll[[1]]$flux)

#
resDur_N2O <- plotDurationUncertaintyRelSD( df, colConc = "N2O_dry", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 0.5
                                       , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
)

plot( flux ~ duration, resDur_N2O$statAll[[1]] )
plot( sdFlux ~ duration, resDur_N2O$statAll[[1]] )

# Find optimal window duration (findStabilizationPoint Function)
resDur_N2O_CV <- resDur_N2O$statAll[[1]]$sdFlux / resDur_N2O$statAll[[1]]$fluxMedian
resDur_N2O_duration <- resDur_N2O$statAll[[1]]$duration
findStabilizationPoint(resDur_N2O_duration, resDur_N2O$statAll[[1]]$flux)


#
#

# Create histograms WDo ---------------------------------------------------
## first create a function "PlotDurationUncertaintyRelSD" for all chunks and load it
# source("inst/plotDurationUncertaintyRelSDforChunks.R")
# #
# resDurChunks <- plotDurationUncertaintyRelSDforChunks( dsChunk, collar_spec_CO2
#                                         , colTime= "TIMESTAMP",  colTemp="AirTemp", colPressure="Pa"
#                                         , colConc = "CO2_dry", volume = chamberVol
#                                         , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
#                                         , concSensitivity = 0.05
#                                         , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
#                                         , maxSdFluxRel = 0.5 #this should be relative to the median (e.g. 10% von median)
#                                         , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
# )
#
# resDurChunks$duration




# get optimum measurement window duration for each chunk --------------------------------------------

# resWDur <- list()
# for (v in unique(dsChunk$iChunk)){
#   dfi <- dsChunk %>% dplyr::filter(iChunk==v)
#
#   WDur <- plotDurationUncertaintyRelSD( dfi, plot=FALSE, colConc = "CO2_dry", colTemp="AirTemp", volume = chamberVol,
#                                           fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp
#                                           )
#                                           , maxSdFluxRel = 1 #this should be relative to the median (e.g. 10% von median)
#                                           , durations = seq(60,max(as.numeric(dfi$TIMESTAMP) - as.numeric(dfi$TIMESTAMP[1])),20)
#   )
#
#   resWDur[[v]] <- WDur
# }

#replacement of for-loop above:
##unique_chunks <- unique(dsChunk$iChunk)

plan(multisession, workers = 6)

unique_good_chunks <- unique(res_CO2_r2smaller0_8$iChunk)

resWDur <- unique_good_chunks %>%
  map(function(v) {
    dfi <- dsChunk[dsChunk$iChunk %in% res_CO2_r2smaller0_8$iChunk,] %>% filter(iChunk == v)

    plotDurationUncertainty(
      dfi, colConc = "CO2_dry", colTemp = "AirTemp", volume = chamberVol,
      durations = seq(60, max(as.numeric(dfi$TIMESTAMP) - as.numeric(dfi$TIMESTAMP[1])), 30)
    )
  }) %>%
  set_names(unique_good_chunks)

#save results as RDS
#saveRDS(resWDur,file=paste0(results_dir,"/results_WDur_NH3.rds"),compress = T)

#zz <- readRDS(paste0(results_dir,"/results_WDur_NH3.rds"))

# Apply the function findStabilizationPoints to each data frame in the list
WDur_opt <- lapply(resWDur, function(df) {
  y <- df$statAll[[1]]$sdFlux / df$statAll[[1]]$fluxMedian
  x <- df$statAll[[1]]$duration
  findStabilizationPoint(x, y)
})

# Create a tibble with the chunk names and the respective optimal window durations
# Create a tibble with the names of each list item
WDur_opt_tibble <- tibble(
  name = names(WDur_opt),
  Duration_stab = sapply(WDur_opt, function(res) res$Duration_stab),
  CV_stab = sapply(WDur_opt, function(res) res$CV_stab)
)

# ggplot() +
#   geom_histogram(aes(WDur_tibble$duration), binwidth = 20, color = "black", fill= "grey") +
#   labs(title = "gas name", x = "WD optimum",
#        y = "Frequency")
#
# ggplot(WDur_tibble, aes(duration)) +
#   geom_histogram(aes(y = ..density..), color = "#000000", fill = "#0099F8") +
#   geom_density(color = "#000000", fill = "#F85700", alpha = 0.6)


# Plot With coloured quantiles
# Compute median and quartiles
median_val <- median(WDur_opt_tibble$Duration_stab)
quantile_25 <- quantile(WDur_opt_tibble$Duration_stab, 0.25)
quantile_75 <- quantile(WDur_opt_tibble$Duration_stab, 0.75)

# Total number of observations
total_counts <- nrow(WDur_opt_tibble$Duration_stab)

# Bin width
binwidth <- 2

# Maximum count for the histogram
max_count <- max(hist(WDur_opt_tibble$Duration_stab, plot = FALSE, breaks = seq(min(WDur_opt_tibble$Duration_stab), max(WDur_opt_tibble$Duration_stab), by = binwidth))$counts)

# Create a dataframe for quantiles (used for legend)
vline_data <- data.frame(
  x = c(median_val, quantile_25, quantile_75),
  label = c("Median", "1st Quartile", "3rd Quartile"),
  color = c("red", "blue", "blue")
)

# Create the plot
ggplot(WDur_opt_tibble, aes(x = Duration_stab)) +
  theme_minimal() +
  geom_histogram(aes(y = after_stat(density)), fill = "grey", color = NA, binwidth = binwidth) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(data = vline_data, aes(xintercept = x, color = label), linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("Median" = "red", "1st Quartile" = "blue", "3rd Quartile" = "blue")) +
  scale_x_continuous(
    breaks = seq(min(WDur_opt_tibble$Duration_stab), max(WDur_opt_tibble$Duration_stab), by = 20),
    name = "Duration"
  ) +
  scale_y_continuous(
    name = "Density",
    sec.axis = sec_axis(~ . * total_counts * binwidth,
                        name = "Counts",
                        breaks = seq(0, max_count, by = 2))
  ) +
  labs(
    title = "Histogram and density plot with median and quartiles",
    subtitle = "",
    color = NULL,  # This removes the title from the legend
    x = "Duration"
  )+
  theme(
    legend.title = element_blank(),  # Ensure the legend title is blank
    legend.position.inside = c(1, 1),       # Position legend in the upper right
    legend.justification = c(1, 1)   # Justify the legend to the upper right corner
  )


