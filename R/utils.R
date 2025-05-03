# utils.R
# Utility functions used across fractalize

#' Normalize a numeric vector to [0, 1]
#'
#' @param x Numeric vector
#' @return Normalized vector
normalize <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

#' Compute pairwise Euclidean distances between points
#'
#' @param coords A matrix or data.frame with two columns (x, y)
#' @return A symmetric distance matrix
distance_matrix <- function(coords) {
  as.matrix(dist(coords))
}

#' Center and scale a numeric vector (mean = 0)
#'
#' @param x Numeric vector
#' @return Centered and scaled vector
center_scale <- function(x) {
  if (sd(x, na.rm = TRUE) == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

#' Clamp values to within a specified range
#'
#' @param x Numeric vector
#' @param min_val Minimum value
#' @param max_val Maximum value
#' @return Clamped numeric vector
clamp <- function(x, min_val = 0, max_val = 1) {
  pmin(pmax(x, min_val), max_val)
}

#' Replace NA values in a vector with a specified value
#'
#' @param x Numeric vector
#' @param value Value to replace NA with
#' @return Cleaned vector
fill_na <- function(x, value = 0) {
  x[is.na(x)] <- value
  x
}

