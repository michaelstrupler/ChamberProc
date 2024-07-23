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

fileName <- "JFAADS2174-20220531-080209-DataLog_User.dat" #"JFAADS2174-20220523-110548-DataLog_User.dat" #set here file name
fileName2 <- "JFAADS2174-20220524-074601-DataLog_User.dat"
data_pathname <-"/Users/ms/Research Projects/Espana/UCO_Chamber/Data_Jesus/" #"data/" #"Data_Adrian/Agosto/19/"
data_pathname2 <- "/Users/ms/Research Projects/Espana/UCO_Chamber/Data_Adrian/"
results_pathname <- ""

#create folder with the name of the measurement archive to save results and plots therein
dir.create(paste0(results_pathname,"results"))

results_dir <- paste0(results_pathname,"results/publication24"))
dir.create(results_dir)
# fit chambers in parallel inside calcClosedChamberFluxForChunkSpecs
plan(multisession, workers = 4)


# Read and prepare data ---------------------------------------------------
# read and combine measurement data from multiple datasets

ds0 <-fread(paste0(data_pathname,fileName), sep ="auto") #sample_PICARRO_data
ds1 <-fread(paste0(data_pathname,fileName2), sep ="auto")
#ds2 <-fread(paste0(data_pathname,fileName3), sep ="auto")


#create a timestamp for each dataset
ds0$TIMESTAMP <- as.POSIXct(paste0(ds0$DATE," ",ds0$TIME), "%Y-%m-%d %H:%M:%S", tz= "UTC")
ds1$TIMESTAMP <- as.POSIXct(paste0(ds1$DATE," ",ds1$TIME), "%Y-%m-%d %H:%M:%S", tz= "UTC")
#ds2$TIMESTAMP <- as.POSIXct(paste0(ds2$DATE," ",ds2$TIME), "%Y-%m-%d %H:%M:%S", tz= "UTC")


#subset datasets
ds0_subset <- ds0 %>% filter(between(as.numeric(TIMESTAMP),as.numeric(RespChamberProc::as.POSIXctUTC("2022-05-31 08:00:00")), as.numeric(RespChamberProc::as.POSIXctUTC("2022-05-31 12:00:00" ))))
ds1_subset <- ds1 %>% filter(between(as.numeric(TIMESTAMP),as.numeric(RespChamberProc::as.POSIXctUTC("2022-05-24 08:00:00")), as.numeric(RespChamberProc::as.POSIXctUTC("2022-05-24 12:00:00" ))))

# Correct gases -----------------------------------------------------------
## all this is done for each dataset subset with the function "process_dataset" (correction for gases and add additional environmental variables)
source("inst/process_dataset.R")

ds0_processed <- process_dataset(ds0_subset,latDeciDeg,lonDeciDeg)
ds1_processed <- process_dataset(ds1_subset,latDeciDeg,lonDeciDeg)

#merge the loaded datasets and remove columns that are not needed for further calculations
ds_merged_location1 <-rbind(ds0_processed,ds1_processed)
ds <- ds_merged_location1
#ds_merged_location2 <-rbind(ds0,ds1)




## get an overview of the data (for the whole subset)
p_collar <- plot(ds$TIMESTAMP,ds$collar, pch = ".", ylab = "collar (Chamber)",xlab = "Time")
p_collar

### facet plot of time series (for selected subset; select the ds here)
ds_gas_long <- gather(ds0_processed, key="gas", value="value", c("CO2_dry","H2Oppt","CH4_dry","NH3_dry","N2O_dry"))
p_gas_facet <- ggplot(ds_gas_long, aes(x=TIMESTAMP, y=value))+
  ggtitle(format(ds$TIMESTAMP,"%d/%m/%Y")[1])+
  geom_point(pch = ".")+
  facet_wrap(~factor(gas,levels=c("CO2_dry","H2Oppt","CH4_dry","NH3_dry","N2O_dry")),ncol=1,scales = "free")

ds_envar_long <- gather(ds0_processed, key="envar", value="value", c("AirTemp","Pa","rel_humidity","shortwave_radiation","ET0"))
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
dsChunk <- dsChunk_raw %>% group_by(iChunk) %>% slice(seq(1, n(), 2)) %>% ungroup()

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

collar_spec_CO2 <-mutate(collar_spec, tlag = 60) #One can save processing time and avoid failures in the non-robust breakpoint-detection by specifying a fixed lag-time (may differ across collars) with the collar specification.
collar_spec_H2O <-mutate(collar_spec, tlag = 80)
collar_spec_CH4 <-mutate(collar_spec, tlag = 60)
collar_spec_NH3 <-mutate(collar_spec, tlag = 60)
collar_spec_N2O <-mutate(collar_spec, tlag = 95)





# nNode = 8	# number of processors
# cl = makeCluster(nNode)
# registerDoSNOW(cl)
# clusterEvalQ(cl, library(RespChamberProc))
#
#
# system.time(res <- ddply(dsChunk, .(iChunk), function(dsi){
#   collar <- dsi$collar[1]
#   iChunk = dsi$iChunk[1]
#   print( paste(iChunk, dsi$TIMESTAMP[1], " collar: ",collar) )
#
#
#
# res <- calcClosedChamberFluxForChunkSpecs(
#       dsi, collar_spec_CO2
#       , colTemp = "AirTemp", colPressure = "Pa"
#       , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
#       , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
#       , colConc = "CO2_dry", colTime = "TIMESTAMP"
#       , concSensitivity = 0.01
#       , minTLag= 30
#       , maxLag = 200
#       )
#
#   resH2O <- calcClosedChamberFluxForChunkSpecs(
#     dsi, collar_spec_H2O
#     , colTemp = "AirTemp", colPressure = "Pa"
#     , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
#     , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
#     , colConc = "H2Oppt", colTime = "TIMESTAMP"
#     , concSensitivity = 0.01
#     , minTLag= 120
#     , maxLag = 200
#   )
# #
#   resCH4 <-
#     calcClosedChamberFluxForChunkSpecs(
#       dsi, collar_spec_CH4
#       , colTemp = "AirTemp", colPressure = "Pa"
#       , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
#       , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
#       , colConc = "CH4_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
#       , concSensitivity = 0.01
#       , minTLag= 60
#       , maxLag = 200
#     )
# #
#   resNH3 <- calcClosedChamberFluxForChunkSpecs(
#     dsi, collar_spec_NH3
#     , colTemp = "AirTemp", colPressure = "Pa"
#     , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
#     , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
#     , colConc = "NH3_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
#     , concSensitivity = 0.01
#     , minTLag= 60
#     , maxLag = 200
#   )
# #
#   resN2O <- calcClosedChamberFluxForChunkSpecs(
#     dsi, collar_spec_N2O
#     , colTemp = "AirTemp", colPressure = "Pa"
#     , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
#     , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
#     , colConc = "N2O_dry", colTime = "TIMESTAMP"	# colum names conc ~ timeInSeconds
#     , concSensitivity = 0.01
#     , minTLag= 100
#     , maxLag = 200
#   )
# #
# #
#
#   #   get additional environmental variables at the initial time
#   to <- ifelse(res$tLag == 0, 1, res$tLag)
#   dsiInitial <- dsi[to, , drop = FALSE]
#
#   cbind( data.frame( time=dsiInitial[,"TIMESTAMP"], collar=collar
#                      , CO2_flux=res$fluxMedian, CO2_flux_sd=res$sdFlux, Fregress_CO2=res$iFRegress, r2_CO2=res$r2
#                      , H2O_flux=resH2O$fluxMedian , H2O_flux_sd=resH2O$sdFlux, Fregress_H2O=resH2O$iFRegress, r2_H2O=resH2O$r2
#                      , CH4_flux=resCH4$fluxMedian, CH4_flux_sd=resCH4$sdFlux, Fregress_CH4=resCH4$iFRegress, r2_CH4=resCH4$r2
#                      , NH3_flux=resNH3$fluxMedian, NH3_flux_sd=resNH3$sdFlux, Fregress_NH3=resNH3$iFRegress, r2_NH3=resNH3$r2
#                      , N2O_flux=resN2O$fluxMedian , N2O_flux_sd=resN2O$sdFlux, Fregress_N2O=resN2O$iFRegress, r2_N2O=resN2O$r2
#   )
#   , dsiInitial[,c("CO2_dry","CH4_dry","NH3_dry","N2O_dry", "AirTemp","Pa")] )
# }
# ))

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






#Tests variation in lag time and duration for a single chunk
## Function to test optimal time windows and tlags
# source("/Users/ms/Research Projects/Espana/UCO_Chamber/1_Test_newPackage_ChamberProc/fluxDurationTests.R")
#
#
# # Define parameters for the function
# start_tlag <- 60
# end_tlag <- 200
# interval_tlag <- 20
# start_duration <- 60
# end_duration <- 160
# interval_duration <- 20
# colConc <- "CO2_dry" #CH4
#
# #for all chunks
# ### create a list with tLags and WD_optimums for each chunk
# WD_O_list <-list()
#
# for (i in mapped_collars$iChunk) {
#
#   df<-filter(dsChunk,iChunk==5) #i
# resTwindowLag<- fluxDurationTest(df, start_tlag, end_tlag, interval_tlag, start_duration, end_duration, interval_duration, colConc)
#
# # Access the returned objects
# fluxTibble <- resTwindowLag$fluxTibble %>% mutate(CV=sdFlux/flux)
# fluxTibble_pivot <- resTwindowLag$fluxTibble_pivot
# summary_stats <- resTwindowLag$summary_stats
#
# fluxTibble_pivot
# summary_stats
#
# # Figure sdFlux vs Window Duration (WD)
# ggplot(data=fluxTibble, aes(x = duration, y = sdFlux, size = tLag)) +
#   geom_point(shape=1) +
#   labs(title = "Scatterplot of Duration vs sd Flux",
#        x = "Duration",
#        y = "sd Flux",
#        size = "Tlag") +
#   theme_minimal()
#
# # Figure sdFlux/flux = CV vs Window Duration (WD)
# ggplot(data=fluxTibble, aes(x = duration, y = CV, size = tLag)) +
#   geom_point(shape=1) +
#   labs(title = "Scatterplot of Duration vs sdFlux/flux (CV)",
#        x = "Duration",
#        y = "CV",
#        size = "Tlag") +
#   theme_minimal()
#
# ## Figure with facet plots for each gas
# ggplot(data=fluxTibble, aes(x = duration, y = sdFlux)) +
#   geom_point() +
#   facet_wrap(~ tLag, scales = "fixed") +
#   labs(x = "WD", y = "sdFlux", title = "sd Flux vs WD for different tLags ")
#
#
# ggplot(data=fluxTibble, aes(x = duration, y = CV)) +
#   geom_point() +
#   facet_wrap(~ tLag, scales = "fixed") +
#   labs(x = "WD", y = "CV flux", title = "sd Flux / flux (=CV) vs WD for different tLags ")

# create a Figure with a distribution (or histogram) of optimal Window Durations for all measurements of a gas to identify range of optimal duration
## first define condition sd < X  or CV < X in order to define WDoptimum
### find for each tlag the WD that first meets the criterium CV <0.5
# WD_O <- fluxTibble  %>% group_by(tLag) %>% filter(abs(CV)<=0.5) %>% summarize(WD_optimum = min(duration, na.rm = TRUE))
#
# WD_O_list[[i]] <- WD_O
# }











# find optimal model fit for each gas -------------------------------------

res_CO2 <- res_CO2 %>% mutate(gas = "CO2")
res_CH4 <- res_CH4 %>% mutate(gas = "CH4")
res_H2O <- res_H2O %>% mutate(gas = "H2O")
res_N2O <- res_N2O %>% mutate(gas = "N2O")
res_NH3 <- res_NH3 %>% mutate(gas = "NH3")



# Combine the data frames
combined_res <- bind_rows(res_CO2, res_CH4, res_H2O,res_N2O,res_NH3)
combined_res <- combined_res %>% mutate(iFRegress = factor(iFRegress))

#save results as RDS
saveRDS(combined_res %>% select(-c(times,model)),file=paste0(results_dir,"/results.rds"),compress = T)
#save results as .csv
write.csv(combined_res %>% select(-c(times,model)),file=paste0(results_dir,"/results.txt"))

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

##select just one chunk
selected_chunk=4 #error when selected chunk==7: error in `gls()`:! false convergence (8) if exponential model or polynomial is used
df<-filter(dsChunk,iChunk==selected_chunk)
#
resDur <- plotDurationUncertaintyRelSD( df, colConc = "CO2_dry", colTemp="AirTemp", volume = chamberVol,
                                   fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh)
                                   , maxSdFluxRel = 1 #this should be relative to the median (e.g. 10% von median)
                                   , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),20)
)
resDur$duration
#
plot( flux ~ duration, resDur$statAll[[1]] )
plot( abs(sdFlux/fluxMedian) ~ duration, resDur$statAll[[1]])
plot( sdFlux ~ duration, resDur$statAll[[1]] )
#

resDur_H2O <- plotDurationUncertaintyRelSD( df, colConc = "H2Oppt", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 1
                                       , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
)
plot( flux ~ duration, resDur_H2O$statAll[[1]] )
plot( abs(sdFlux/fluxMedian) ~ duration, resDur_H2O$statAll[[1]])
plot( sdFlux ~ duration, resDur_H2O$statAll[[1]] )
#
resDur_CH4 <- plotDurationUncertaintyRelSD( df, colConc = "CH4_dry", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 1
                                       , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
)

plot( flux ~ duration, resDur_CH4$statAll[[1]] )
plot( abs(sdFlux/fluxMedian) ~ duration, resDur_CH4$statAll[[1]])
plot( sdFlux ~ duration, resDur_CH4$statAll[[1]] )
#
resDur_NH3 <- plotDurationUncertaintyRelSD( df, colConc = "NH3_dry", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 1
                                       , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
)

plot( flux ~ duration, resDur_NH3$statAll[[1]] )
plot( abs(sdFlux/fluxMedian) ~ duration, resDur_NH3$statAll[[1]])
plot( sdFlux ~ duration, resDur_NH3$statAll[[1]] )
#
resDur_N2O <- plotDurationUncertaintyRelSD( df, colConc = "N2O_dry", colTemp="AirTemp", volume = chamberVol,
                                       fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare
                                       )
                                       , maxSdFluxRel = 0.5
                                       , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
)

plot( flux ~ duration, resDur_N2O$statAll[[1]] )
plot( abs(sdFlux/fluxMedian) ~ duration, resDur_N2O$statAll[[1]])
plot( sdFlux ~ duration, resDur_N2O$statAll[[1]] )

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


resWDur <- list()
for (v in unique(dsChunk$iChunk)){
  dfi <- dsChunk %>% dplyr::filter(iChunk==v)

  WDur <- plotDurationUncertaintyRelSD( dfi, colConc = "CO2_dry", colTemp="AirTemp", volume = chamberVol,
                                          fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp
                                          )
                                          , maxSdFluxRel = 1 #this should be relative to the median (e.g. 10% von median)
                                          , durations = seq(60,max(as.numeric(dfi$TIMESTAMP) - as.numeric(dfi$TIMESTAMP[1])),20)
  )

  resWDur[[v]] <- WDur
}


## extract the chunk name and duration from the list:
# function to extract duration and list name
extract_duration <- function(tbl_name, tbl) {
  duration <- tbl$duration
  list_name <- as.integer(tbl_name)
  tibble(list_name = list_name, duration = duration)
}

# Iterate over each tibble in `resWDur`, extracting duration and list name
WDur_tibble <- map_df(names(resWDur), ~ extract_duration(.x, resWDur[[.x]]))

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
median_val <- median(WDur_tibble$duration)
quantile_25 <- quantile(WDur_tibble$duration, 0.25)
quantile_75 <- quantile(WDur_tibble$duration, 0.75)

# Total number of observations
total_counts <- nrow(WDur_tibble)

# Bin width
binwidth <- 2

# Maximum count for the histogram
max_count <- max(hist(WDur_tibble$duration, plot = FALSE, breaks = seq(min(WDur_tibble$duration), max(WDur_tibble$duration), by = binwidth))$counts)

# Create a dataframe for quantiles (used for legend)
vline_data <- data.frame(
  x = c(median_val, quantile_25, quantile_75),
  label = c("Median", "1st Quartile", "3rd Quartile"),
  color = c("red", "blue", "blue")
)

# Create the plot
ggplot(WDur_tibble, aes(x = duration)) +
  theme_minimal() +
  geom_histogram(aes(y = after_stat(density)), fill = "grey", color = NA, binwidth = binwidth) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  geom_vline(data = vline_data, aes(xintercept = x, color = label), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Median" = "red", "1st Quartile" = "blue", "3rd Quartile" = "blue")) +
  scale_x_continuous(
    breaks = seq(min(WDur_tibble$duration), max(WDur_tibble$duration), by = 20),
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
    subtitle = "CO2",
    color = NULL,  # This removes the title from the legend
    x = "Duration"
  )+
  theme(
    legend.title = element_blank(),  # Ensure the legend title is blank
    legend.position.inside = c(1, 1),       # Position legend in the upper right
    legend.justification = c(1, 1)   # Justify the legend to the upper right corner
  )
