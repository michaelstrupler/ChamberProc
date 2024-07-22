
#read data
ds <- readRDS(url("https://github.com/michaelstrupler/ChamberProc/blob/master/inst/genData/issue8_duration.rds?raw=TRUE"),"rb")

dsChunk<- subsetContiguous(ds, colTime = "TIMESTAMP", colIndex = "collar",
                                gapLength = 12, minNRec = 180, minTime = 180, indexNA = 13)

mapped_collars <- dsChunk %>% group_by(iChunk) %>% summarise(collar = first(collar)) %>%  head()


# Define collar specs
chamberVol = 0.6*0.6*0.6		# chamber was a cube of 0.6m length
surfaceArea = 0.6*0.6

collar_spec <- tibble(
  collar = unique(dsChunk$collar),
  depth = 0, #pmax(0,rnorm(length(collar), mean = 0.03, sd = 0.015)),
  area = surfaceArea,
  volume = chamberVol + surfaceArea * depth,
  tlag = NA)
head(collar_spec)


collar_spec_CO2 <-mutate(collar_spec, tlag = 60)


# Function PlotDurationUncertaintyRelSDforChunks" ---------------------------------------------------
plotDurationUncertaintyRelSDforChunks <- function(
    ### plot the increase of uncertainty with decreasing measurement duration
  ds
  , colTime = "TIMESTAMP"	##<< column name of time [s]
  , fRegress = c(exp = regressFluxExp, lin = regressFluxLinear, tanh = regressFluxTanh)	##<<
  ## list of functions to yield
  ## a single flux estimate, see details of \code{\link{calcClosedChamberFlux}}
  , ...	            ##<< further arguments to \code{\link{calcClosedChamberFlux}}
  , durations = seq( max(65,resFit0$tLag), max(times0), length.out = nDur + 1)		##<<
  ## durations to check. Default is equally spaced between tLag and maximum duration
  , nDur = 5		    ##<< number of durations to check
  , maxSdFluxRel = 1	  ##<< maximum allowed Ratio of standard deviation of flux to flux
){
  times <- ds[[colTime]]
  times0 <- as.numeric(times) - as.numeric(times[1])
  resFit0 <- calcClosedChamberFluxForChunkSpecs(ds, colTime = colTime, fRegress = fRegress, ...)
  #resFit0
  duration <- durations[1]
  nDur <- length(durations)
  #plot( CO2_dry ~ times0, ds)
  resFits0 <- suppressWarnings(
    bind_rows(map_df( durations[-c(nDur + 1) ], function(duration){
      dss <- subset(ds, times0 <= duration )
      times0s <- times0[times0 <= duration]
      resFit <- calcClosedChamberFlux(dss, useFixedTLag = resFit0$tLag
                                      ,colTime = colTime, fRegress = fRegress[resFit0$iFRegress],...)
      #plot( CO2_dry ~ times0s, dss)
      #lines( fitted(resFit$model) ~ times0s[times0s >= resFit0$tLag], col = "red")
      #bind_cols(select_(resFit,~flux,~sdFlux), duration = max(times0s) )
      #select_(resFit,~flux,~sdFlux)
    })))
  resFits <- suppressWarnings(bind_rows(resFits0, resFit0)
                              %>% mutate(duration = c(durations,max(times0))))
  iMinTime <- if (min(abs(resFits$sdFlux/resFits$flux), na.rm = TRUE) <= maxSdFluxRel ) {
    min(which( abs(resFits$sdFlux/resFits$flux) <= maxSdFluxRel ))
  } else nrow(resFits)
  minDuration <- resFits[iMinTime,]
  ##details<<
  ## Produces a plot with standard deviation of the flux estimate versus
  ## the duration of the measurment.
  ## The lines correspond to the given maxium acceptable standard deviation to flux ratio
  ## and the duration that matches this criterion.
  plot( abs(sdFlux/fluxMedian) ~ duration, resFits, xlab = "Duration of measurement (s)"
        , ylab = "abs(sdflux/fluxMedian)")
  abline(h = maxSdFluxRel, col = "grey", lty = "dashed" )
  abline(v = minDuration["duration"], col = "grey", lty = "dashed" )
  ##value<< tibble result of \code{\link{calcClosedChamberFlux}} for
  ## the minimum duration, with additional component
  ans <- mutate( resFits[iMinTime,]
                 , statAll = list(resFits)	##<< tibble: each row a fit for a given duration
  )
  ans
}



# Application of the fucntion "plotDurationUncertaintyRelSDforChunks" to the sample dataset
resDurChunks <- plotDurationUncertaintyRelSDforChunks( dsChunk, collar_spec_CO2
                                                       , colTime= "TIMESTAMP",  colTemp="AirTemp", colPressure="Pa"
                                                       , colConc = "CO2_dry", volume = chamberVol
                                                       , fRegress = c(lin = regressFluxLinear, tanh = regressFluxTanh, exp = regressFluxExp, poly= regressFluxSquare)
                                                       , concSensitivity = 0.01
                                                       , debugInfo = list(omitEstimateLeverage = FALSE)	# faster
                                                       , maxSdFluxRel = 0.5 #this should be relative to the median (e.g. 10% von median)
                                                       , durations = seq(60,max(as.numeric(df$TIMESTAMP) - as.numeric(df$TIMESTAMP[1])),30)
)

resDurChunks$duration


