#' Estimate local singularity exponents alpha(x) using wavelet decomposition
#'
#' @param x Numeric vector of locations (1D), or x-coordinates if 2D
#' @param y Numeric vector of observations (must be same length as x)
#' @param scales Numeric vector of dyadic wavelet scales (e.g., c(2, 4, 8, 16))
#' @param wavelet Character. Type of wavelet (default = "haar")
#' @return A numeric vector of estimated alpha(x) values
#' @export
estimate_alpha <- function(x, y, scales = c(2, 4, 8, 16), wavelet = "haar") {
  if (length(x) != length(y)) stop("x and y must be the same length.")

  # Sort data by x (important for spatial alignment)
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  
  n <- length(y)
  log_a <- log(scales)
  log_W <- matrix(NA, nrow = n, ncol = length(scales))
  
  # Install required package if not present
  if (!requireNamespace("waveslim", quietly = TRUE)) {
    stop("Please install the 'waveslim' package for wavelet transforms.")
  }

  for (i in seq_along(scales)) {
    a <- scales[i]

    # Simple local averaging window (moving difference)
    # Approximate wavelet transform: local difference over scale a
    for (j in seq_len(n)) {
      left <- max(1, j - a)
      right <- min(n, j + a)
      window <- y[left:right]
      
      if (length(window) > 2) {
        local_wave <- diff(range(window))  # local fluctuation proxy
        log_W[j, i] <- log(abs(local_wave) + 1e-6)  # avoid log(0)
      } else {
        log_W[j, i] <- NA
      }
    }
  }

  # For each location, fit log |W(a,x)| ~ log(a)
  alpha_hat <- rep(NA, n)
  for (j in seq_len(n)) {
    yj <- log_W[j, ]
    if (sum(!is.na(yj)) >= 2) {
      fit <- lm(yj ~ log_a)
      alpha_hat[j] <- coef(fit)[2] - 0.5  # alpha = slope - 0.5
    }
  }

  return(alpha_hat)
}

