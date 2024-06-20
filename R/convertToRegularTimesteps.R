#' Interpolate measurement data to equidistant timesteps
#'
#' @description Interpolate measurement data from irregular timesteps to equidistant timesteps
#'
#' @param df A dataframe containing the measurement data and time stamps
#' @param var_names Character. The names of the variables that need to be interpolated to regular time steps
#' @param regular_timesteps A continuous timestamp vector
#'
#' @return A dataframe with equidistant timesteps
#' @export
#'
#' @examples
#' ds <- dataset
#' regular_timesteps <- seq(min(ds$TIMESTAMP),max(ds$TIMESTAMP), by="1 sec")
#' ds <-convertToRegularTimesteps(ds,c("CO2_dry", "H2O"),regular_timesteps)

convertToRegularTimesteps <-function(df, var_names, regular_timesteps) {
  interpolated_data <- lapply(var_names, function(var) {
    interpolated_values <- approx(df$TIMESTAMP, df[[var]], xout = regular_timesteps)$y
    return(interpolated_values)
  })
  interpolated_data <- cbind(TIMESTAMP = regular_timesteps, as.data.frame(interpolated_data))
  colnames(interpolated_data)[-1] <- var_names #[-1 excludes the name of the first column (TIMESTAMP), as already included in cbind]
  return(interpolated_data)
}
