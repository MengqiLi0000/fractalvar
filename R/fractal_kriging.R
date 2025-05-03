#' Perform kriging using a fractal-corrected variogram
#'
#' @param data A data.frame with columns x, y, value (observed points)
#' @param alpha_map A numeric vector of estimated alpha(x) at data locations
#' @param newdata A data.frame with prediction locations (x, y)
#' @param epsilon Minimum scale used in alpha estimation
#' @param epsilon_max Maximum scale used in alpha estimation
#' @param model_type Type of variogram model to fit (e.g. "Exp", "Sph", "Gau")
#'
#' @return A SpatialPointsDataFrame or data.frame with kriging predictions
#' @export
fractal_kriging <- function(data, alpha_map, newdata,
                            epsilon = 1, epsilon_max = 10,
                            model_type = "Exp") {
  library(gstat)
  library(sp)
  
  if (!all(c("x", "y", "value") %in% names(data))) stop("data must have x, y, and value columns.")
  if (!all(c("x", "y") %in% names(newdata))) stop("newdata must have x and y columns.")

  # Convert to spatial
  coordinates(data) <- ~x + y
  coordinates(newdata) <- ~x + y

  # Empirical variogram
  emp_vario <- variogram(value ~ 1, data)

  # Apply correction
  coords_mat <- coordinates(data)
  vario_corrected <- correct_variogram(emp_vario, alpha_map, coords_mat,
                                       epsilon = epsilon, epsilon_max = epsilon_max)

  # Fit model to corrected variogram
  corrected_variogram <- emp_vario
  corrected_variogram$gamma <- vario_corrected$gamma

  vgm_fit <- fit.variogram(corrected_variogram, model = vgm(psill = 1, model = model_type, nugget = 0.1, range = 10))

  # Kriging
  kriged <- krige(value ~ 1, data, newdata, model = vgm_fit)

  return(kriged)
}

