#' Generate labels for chunk facet plots
#'
#' @param dsChunk A dataframe containing all measurement data grouped by chunks for all gases.
#' @param gas_column Numeric. The column in the dsChunk with gas measurement data (e.g., 'CO2_dry').
#' @param y_multiplier Numeric. This multiplier is used for label placement.
#'
#' @return A list of labels for each chunk.
#' @export
#'
#' @examples
#' labels_CO2 <- chunk_labels(dsChunk, CO2_dry, 1.05)
chunk_labels <- function(dsChunk, gas_column, y_multiplier) {
  dsChunk %>%
    dplyr::group_by(iChunk) %>%
    dplyr::summarise(
      collar = first(collar),  # Use the Collar value for each iChunk
      x = max(TIMESTAMP),      # Use max TIMESTAMP for x position
      y = max({{ gas_column }}) * y_multiplier  # Use slightly above max gas value for y position
    )
}

#' Generate facet plot with all chunks for selected gases. Please first create the labels for each gas using 'chunk_labels()'.
#'
#' @param dsChunk A dataframe containing all measurement data grouped by chunks for all gases.
#' @param gas_column Numeric. The column in the dsChunk with gas measurement data (e.g., 'CO2_dry').
#' @param labels A list of labels for each chunk.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' Chunk_Facet_Plot_CO2 <- chunk_plot(dsChunk, CO2_dry, labels_CO2)
chunk_plot <- function(dsChunk, gas_column, labels) {
  ggplot(dsChunk, aes(x = TIMESTAMP, y = {{ gas_column }})) +
    geom_point(pch='.') +
    facet_wrap(~ iChunk, scales = "free") +
    geom_text(data = labels, aes(x = x, y = y, label = paste("Collar", collar, sep = " ")),
              hjust = 1, vjust = 1, inherit.aes = FALSE)
}
