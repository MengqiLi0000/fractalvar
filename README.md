# ```fractalvar``` (Preliminary Results--To Be Updated)

![Apr 30, 2025, 02_09_48 PM](https://github.com/user-attachments/assets/8c45ada8-8ba1-4130-b583-4c6f578d544b)
This is for my working project on fractal-corrected variogram estimation
The `fractalvar` R package provides tools for estimating **location-specific singularity exponents** and applying **fractal corrections to variograms** in spatial data. This enables second-order spatial models to adapt to **local variation in roughness and scaling**, improving interpolation and model fit in heterogeneous environments.

The method extends the classical variogram using multifractal theory and wavelet-based estimation. It models **multi-scale spatial dependence** without introducing zones or increasing model complexity.

# What You Can Do

✅ Find out how rough or smooth your data is at each location  
✅ Fix your variogram to reflect these local differences  
✅ Make better interpolation maps using this corrected model  
✅ See how roughness changes across space with a simple plot  

# Example

```r
library(fractalvar)

# Load example data
data("synthetic_1d")  # has x, y, value

# Estimate fractal alpha at each location
alpha <- estimate_alpha(index = 1:length(value), series = value)

# Fix the variogram using these alpha values
empirical <- gstat::variogram(value ~ 1, data.frame(x = x, y = y, value = value))
coords <- cbind(x, y)
corrected <- correct_variogram(empirical, alpha, coords)

# Interpolate using the corrected model
grid <- expand.grid(x = seq(0, 1, length.out = 100), y = seq(0, 1, length.out = 100))
result <- fractal_kriging(data.frame(x, y, value), alpha, newdata = grid)

# See how roughness (alpha) looks across space
plot_alpha_map(x, y, alpha)
