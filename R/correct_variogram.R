#' Correct an empirical variogram using local singularity exponents
#'
#' @param variogram_data A data.frame containing empirical variogram values (with columns: dist, gamma)
#' @param alpha_map A numeric vector or data.frame of estimated alpha(x) values for each location
#' @param coords A data.frame or matrix of coordinates corresponding to the alpha_map
#' @param epsilon Numeric. Minimum scale (e.g. resolution)
#' @param epsilon_max Numeric. Maximum scale used in alpha estimation
#' @return A data.frame with corrected variogram values
#' @export
correct_variogram <- function(variogram_data, alpha_map, coords, epsilon = 1, epsilon_max = 10) {
  if (missing(variogram_data) || missing(alpha_map) || missing(coords)) {
    stop("Please provide variogram_data, alpha_map, and coords.")
  }
  
  if (!all(c("dist", "gamma") %in% colnames(variogram_data))) {
    stop("variogram_data must contain 'dist' and 'gamma' columns.")
  }

  # Construct spatial distance matrix between all coords
  dist_matrix <- as.matrix(dist(coords))

  # Compute delta_alpha(x, h) — difference in alpha for each lag
  delta_alpha_list <- list()
  
  for (i in 1:nrow(variogram_data)) {
    h <- variogram_data$dist[i]
    
    # Find all pairs at approx. distance h
    pairs <- which(abs(dist_matrix - h) < (h * 0.05), arr.ind = TRUE)
    
    if (nrow(pairs) == 0) {
      delta_alpha_list[[i]] <- 0
      next
    }
    
    # Compute average delta_alpha for those pairs
    deltas <- alpha_map[pairs[, 1]] - alpha_map[pairs[, 2]]
    delta_alpha_list[[i]] <- mean(deltas, na.rm = TRUE)
  }

  delta_alpha_vec <- unlist(delta_alpha_list)
  
  # Apply correction factor
  correction_factor <- (epsilon / epsilon_max) ^ delta_alpha_vec
  gamma_corrected <- variogram_data$gamma * correction_factor

  # Return corrected variogram
  result <- data.frame(
    dist = variogram_data$dist,
    gamma = gamma_corrected
  )
  
  return(result)
}

