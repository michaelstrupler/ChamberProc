#' Get Additional Environmental Variables
#'
#' @description This function retrieves additional environmental variables for specific chamber setups where certain data might be missing.
#' The function uses the packages 'openmeteo' for atmospheric  and 'elevatr' for elevation data. Note that weather variables (hourly intervals) can only be retrieved for dates more than 5 days old.
#'
#' @param latDeciDeg Numeric. Latitude in decimal degrees, e.g., 37.915.
#' @param lonDeciDeg Numeric. Longitude in decimal degrees, e.g., -4.72.
#'
#' @param starttime Character. Start date in '%Y-%m-%d' format, e.g., '2022-05-01'.
#' @param endtime Character. End date in '%Y-%m-%d' format, e.g., '2022-05-02'.
#'
#' @return A tibble containing the following environmental variables:
#'   - `elevation` (*h*): Elevation at the sample location. (m a.s.l)
#'   - `AirTemp`: Air temperature at 2m above ground. (°C)
#'   - `Pa`: Atmospheric pressure at the sample location elevation. (Pa)
#'   - `rel_humidity`: Relative humidity at 2m above ground. (%)
#'   - `shortwave_radiation`: Shortwave radiation (W/m²)
#'   - `ET0`: Reference Evapotranspiration of a well-watered grass field (mm)
#'
#' @export
#'
#' @examples
#' Additional_Weather_Data <- getAdditionalWeatherVariables(latDeciDeg = 37.915, lonDeciDeg = -4.72, starttime = '2022-05-01', endtime = '2022-05-02')
#'
#' @references
#' - Zippenfenig, P. (2023). Open-Meteo.com Weather API [Computer software]. Zenodo. https://doi.org/10.5281/ZENODO.7970649
#' - Hersbach, H., Bell, B., Berrisford, P., Biavati, G., Horányi, A., Muñoz Sabater, J., Nicolas, J., Peubey, C., Radu, R., Rozum, I., Schepers, D., Simmons, A., Soci, C., Dee, D., & Thépaut, J.-N. (2023). ERA5 hourly data on single levels from 1940 to present [Data set]. ECMWF. https://doi.org/10.24381/cds.adbb2d47
#' - Muñoz Sabater, J. (2019). ERA5-Land hourly data from 2001 to present [Data set]. ECMWF. https://doi.org/10.24381/CDS.E2161BAC
#' - Schimanke, S., Ridal, M., Le Moigne, P., Berggren, L., Undén, P., Randriamampianina, R., Andrea, U., Bazile, E., Bertelsen, A., Brousseau, P., Dahlgren, P., Edvinsson, L., El Said, A., Glinton, M., Hopsch, S., Isaksson, L., Mladek, R., Olsson, E., Verrelle, A., & Wang, Z.Q. (2021). CERRA sub-daily regional reanalysis data for Europe on single levels from 1984 to present [Data set]. ECMWF. https://doi.org/10.24381/CDS.622A565A


#start- and endtime needs to be in ""%Y-%m-%d" format, e.g. "2022-05-01"
getAdditionalWeatherVariables <- function(latDeciDeg,lonDeciDeg,starttime,endtime){



  ## Create a data frame with the coordinates (Geographical Coordinates (decimal degrees); crs =4326)
  sampling_location<- data.frame(
    lat = latDeciDeg, #37.91514875822371
    lon = lonDeciDeg #-4.72405536319233,
  )


  ## Convert the data frame to an sf object
  sampling_location_sf <- st_as_sf(sampling_location, coords = c("lon", "lat"), crs = 4326)
  sampling_location_elevation<- get_elev_point(locations=sampling_location_sf, src = "aws") #gets elevation data from Amazon Web Service Terrain Tiles (Mapzen terrain tiles is a composite of elevation data of varying resolutions from multiple open data sources including SRTM, ETOPO1, and other higher resolution sources for some parts of the world.)


  ## get historical hourly weather variables from the Open-Meteo API
  temperature <- weather_history(location=c(sampling_location$lat,sampling_location$lon),start=starttime,end=endtime,hourly="temperature_2m",timezone = "UTC") #Temperature 2m above ground
  atmospheric_pressure_msl <- weather_history(location=c(sampling_location$lat,sampling_location$lon),start=starttime,end=endtime,hourly="pressure_msl",timezone = "UTC") # multiply by 100 to convert from hPa to Pa

  relative_humidity <- weather_history(location=c(sampling_location$lat,sampling_location$lon),start=starttime,end=endtime,hourly="relative_humidity_2m",timezone = "UTC") # unit %
  sw_radiation <- weather_history(location=c(sampling_location$lat,sampling_location$lon),start=starttime,end=endtime,hourly="shortwave_radiation",timezone = "UTC") # unit: W/m^2
  ref_evapotranspiration <- weather_history(location=c(sampling_location$lat,sampling_location$lon),start=starttime,end=endtime,hourly="et0_fao_evapotranspiration",timezone = "UTC") # unit:mm

  ## Convert atmospheric pressure from sea level to pressure at altitude and convert to Pa
  scale_height <- 8400 # meters
  atmospheric_pressure <- atmospheric_pressure_msl %>% mutate(.,hourly_pressure_location_Pa=100*hourly_pressure_msl*exp(-sampling_location_elevation$elevation / scale_height))

  ## Return the results
  return(tibble(DATETIME_hourly=temperature$datetime
                ,h=sampling_location_elevation$elevation
                ,AirTemp=temperature$hourly_temperature_2m
                ,Pa = atmospheric_pressure$hourly_pressure_location_Pa
                ,rel_humidity=relative_humidity$hourly_relative_humidity_2m
                ,shortwave_radiation=sw_radiation$hourly_shortwave_radiation
                ,ET0=ref_evapotranspiration$hourly_et0_fao_evapotranspiration))
}


