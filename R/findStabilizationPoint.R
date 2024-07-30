#' Find the stabilization Point of a curve. First, a curve (loess) is fitted to the input data and its first derivative calculated, using finite differences. Then, the index, where the derivative is close to zero is found.
#'
#' @param x  A vector with durations
#' @param y  A vector with the Fluxes, coefficients of variations or standard deviations
#'
#' @return A tibble with the Duration and Flux or SD or CV value where the curve stabilizes
#' @export
#'
#' @examples
#' y <- resDur$statAll[[1]]$sdFlux / resDur$statAll[[1]]$fluxMedian
#' x <- resDur$statAll[[1]]$duration
#' result <- find_stabilization_point(x, y)
#'
findStabilizationPoint <- function(x, y) {
  # Fit a smooth curve to the data using loess
  fit <- loess(y ~ x)

  # Predict y values using the fitted model
  y_fit <- predict(fit, x)

  # Calculate the first derivative using finite differences
  dy_dx <- diff(y_fit) / diff(x)

  # Identify the point where the derivative is close to zero
  stabilize_point_index <- which.min(abs(dy_dx))

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

