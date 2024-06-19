## Interpolate measurement data to regular timesteps (interval can be defined in object "regular_timesteps") using approx().
convertToRegularTimesteps <-function(df, var_names, regular_timesteps) {
  interpolated_data <- lapply(var_names, function(var) {
    interpolated_values <- approx(df$TIMESTAMP, df[[var]], xout = regular_timesteps)$y
    return(interpolated_values)
  })
  interpolated_data <- cbind(TIMESTAMP = regular_timesteps, as.data.frame(interpolated_data))
  colnames(interpolated_data)[-1] <- var_names #[-1 excludes the name of the first column (TIMESTAMP), as already included in cbind]
  return(interpolated_data)
}
