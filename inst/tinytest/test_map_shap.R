library(terra)
predictors_agg = rast(system.file("raster/predictors_agg.tif", package = "spatialexplain"))
data("regr_exp", package = "spatialexplain")
my_maxcell = 20

set.seed(2024-11-13)
regr_psp_shap = map_shap(regr_exp, predictors_agg, maxcell = my_maxcell, type = "shap")

# check dimensions
expect_equal(terra::nlyr(regr_psp_shap), 6)

# check values
expect_equivalent(terra::global(regr_psp_shap, "mean", na.rm = TRUE)[[1]][[6]],
                  -0.2943066, tolerance = 0.0001)
