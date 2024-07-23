# Define the function that processes each dataset
process_dataset <- function(ds, latDeciDeg, lonDeciDeg) {

  # Obtain additional weather data
  Additional_Weather_Data <- getAdditionalWeatherVariables(latDeciDeg, lonDeciDeg,
                                                           format(min(ds$TIMESTAMP), "%Y-%m-%d"),
                                                           format(max(ds$TIMESTAMP), "%Y-%m-%d"))

  # Convert solenoid valves to integer
  ds$solenoid_valvesInt <- as.integer(ds$solenoid_valves)

  # Convert H2O to ppt
  ds$H2Oppt <- ds$H2O * 10

  # Direct assignment for N2O dry
  ds$N2O_dry <- ds$N2O_dry1min

  # Correct concentration for NH3 and H2O
  ds$NH3_dry <- 10E-3 * corrConcDilution(ds, colConc = "NH3", colVapour = "H2Oppt")
  ds$H2O_dry <- corrConcDilution(ds, colConc = "H2Oppt", colVapour = "H2Oppt")

  # Remove duplicated timestamps
  ds <- ds[!duplicated(ds$TIMESTAMP),]

  # Create continuous timestamp vector with 1-second intervals
  regular_timesteps <- seq(min(ds$TIMESTAMP), max(ds$TIMESTAMP), by = "1 sec")

  # Create a data frame for collar information
  collar_df <- tibble("TIMESTAMP" = ds$TIMESTAMP, "collar" = ds$solenoid_valvesInt)

  # Convert dataset to regular timesteps
  ds <- convertToRegularTimesteps(ds, c("CO2_dry", "CH4_dry", "H2Oppt", "NH3_dry", "N2O_dry"), regular_timesteps)

  # Join the collar information and fill missing values
  ds <- left_join(ds, collar_df, by = join_by("TIMESTAMP" == "TIMESTAMP")) %>%
    fill(collar, .direction = "down")

  # Create a column with timestamp rounded to the hour
  ds$TIMESTAMP_hour <- floor_date(ds$TIMESTAMP, unit = "hour")

  # Join with additional weather data and fill missing values
  ds <- left_join(ds, Additional_Weather_Data, by = join_by("TIMESTAMP_hour" == "DATETIME_hourly")) %>%
    fill(h, AirTemp, Pa, rel_humidity, shortwave_radiation, ET0, .direction = "up")

  # Calculate Vapour Pressure Deficit (VPD)
  ds$VPD <- calcVPD(ds$AirTemp, ds$Pa, ds$H2Oppt)

  return(ds)
}
