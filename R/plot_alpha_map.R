#' Plot estimated alpha(x) values as a spatial map
#'
#' @param x Numeric vector of x-coordinates
#' @param y Numeric vector of y-coordinates
#' @param alpha Numeric vector of alpha(x) values (same length as x and y)
#' @param grid Logical, whether to interpolate alpha over a regular grid (default = TRUE)
#' @param resolution Numeric, grid resolution if grid = TRUE
#' @param title Plot title (optional)
#' @return A ggplot2 object
#' @export
plot_alpha_map <- function(x, y, alpha, grid = TRUE, resolution = 100, title = "Estimated α(x)") {
  if (length(x) != length(y) || length(x) != length(alpha)) {
    stop("x, y, and alpha must have the same length.")
  }
  
  library(ggplot2)
  library(akima)  # for interpolation

  df <- data.frame(x = x, y = y, alpha = alpha)

  if (grid) {
    interp_alpha <- with(df, interp(x, y, alpha,
                                    xo = seq(min(x), max(x), length = resolution),
                                    yo = seq(min(y), max(y), length = resolution),
                                    linear = TRUE))
    interp_df <- as.data.frame(as.table(interp_alpha$z))
    names(interp_df) <- c("x_idx", "y_idx", "alpha")
    interp_df$x <- interp_alpha$x[interp_df$x_idx]
    interp_df$y <- interp_alpha$y[interp_df$y_idx]
    interp_df <- interp_df[!is.na(interp_df$alpha), ]
    
    p <- ggplot(interp_df, aes(x, y, fill = alpha)) +
      geom_raster(interpolate = TRUE) +
      scale_fill_viridis_c(option = "C", name = "α(x)") +
      labs(title = title, x = "x", y = "y") +
      coord_fixed() +
      theme_minimal()
  } else {
    p <- ggplot(df, aes(x, y, color = alpha)) +
      geom_point(size = 2) +
      scale_color_viridis_c(option = "C", name = "α(x)") +
      labs(title = title, x = "x", y = "y") +
      coord_fixed() +
      theme_minimal()
  }

  return(p)
}

