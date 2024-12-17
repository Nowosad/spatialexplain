library(terra)
predictors_agg = rast(system.file("raster/predictors_agg.tif", package = "spatialexplain"))
data("regr_exp", package = "spatialexplain")
my_maxcell = 20

set.seed(2024-11-13)
regr_psp_os = map_oscillations(regr_exp, predictors_agg, maxcell = my_maxcell, type = "oscillations")
regr_psp_osu = map_oscillations(regr_exp, predictors_agg, maxcell = my_maxcell, type = "oscillations_uni")
regr_psp_ose = map_oscillations(regr_exp, predictors_agg, maxcell = my_maxcell, type = "oscillations_emp")

# check dimensions
expect_equal(terra::nlyr(regr_psp_os), 6)
expect_equal(terra::nlyr(regr_psp_osu), 6)
expect_equal(terra::nlyr(regr_psp_ose), 6)

# check values
expect_equivalent(terra::global(regr_psp_os, "mean", na.rm = TRUE)[[1]][[6]],
                  2.147025, tolerance = 0.0001)
expect_equivalent(terra::global(regr_psp_osu, "mean", na.rm = TRUE)[[1]][[6]],
                  2.375516, tolerance = 0.0001)
expect_equivalent(terra::global(regr_psp_ose, "mean", na.rm = TRUE)[[1]][[6]],
                  2.146239, tolerance = 0.0001)

# check classification model
data("clas_exp", package = "spatialexplain")
clas_psp_bd = map_oscillations(clas_exp, predictors_agg, maxcell = my_maxcell)
expect_equal(terra::nlyr(clas_psp_bd), 6)
