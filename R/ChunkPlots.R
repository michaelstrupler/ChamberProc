# Function to generate labels
chunk_labels <- function(dsChunk, gas_column, y_multiplier) {
  dsChunk %>%
    group_by(iChunk) %>%
    summarise(
      Collar = first(collar),  # Use the Collar value for each iChunk
      x = max(TIMESTAMP),      # Use max TIMESTAMP for x position
      y = max({{ gas_column }}) * y_multiplier  # Use slightly above max gas value for y position
    )
}

# Function to generate plots
chunk_plot <- function(dsChunk, gas_column, labels, y_col) {
  ggplot(dsChunk, aes(x = TIMESTAMP, y = {{ gas_column }})) +
    geom_point(pch='.') +
    facet_wrap(~ iChunk, scales = "free") +
    geom_text(data = labels, aes(x = x, y = y, label = paste("Collar", collar, sep = " ")),
              hjust = 1, vjust = 1, inherit.aes = FALSE)
}
