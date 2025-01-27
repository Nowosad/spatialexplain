library(terra)
predictors_agg = rast(system.file("raster/predictors_agg.tif", package = "spatialexplain"))
data("regr_exp", package = "spatialexplain")
my_maxcell = 20

regr_psp_bd = map_breakdown(regr_exp, predictors_agg, maxcell = my_maxcell)

# check dimensions
expect_equal(terra::nlyr(regr_psp_bd), 7)

# check values
expect_equivalent(terra::global(regr_psp_bd, "mean", na.rm = TRUE)[[1]][[1]],
                  15.10157, tolerance = 0.0001)

# check missing features
expect_error(map_breakdown(regr_exp, predictors_agg, maxcell = my_maxcell, type = "break_down_interactions"))

# check classification model
data("clas_exp", package = "spatialexplain")
clas_psp_bd = map_breakdown(clas_exp, predictors_agg, maxcell = my_maxcell)
expect_equal(terra::nlyr(clas_psp_bd), 7)
