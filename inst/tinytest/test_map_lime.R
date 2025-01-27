library(terra)
predictors_agg = rast(system.file("raster/predictors_agg.tif", package = "spatialexplain"))
data("regr_exp", package = "spatialexplain")
my_maxcell = 20

regr_pss_lime1 = map_lime(regr_exp, predictors_agg, maxcell = my_maxcell)
# regr_pss_lime2 = map_lime(regr_exp, predictors_agg, maxcell = my_maxcell, type = "iml")
# regr_pss_lime3 = map_lime(regr_exp, predictors_agg, maxcell = my_maxcell, type = "lime")

# check dimensions
# expect_equal(terra::ncell(regr_pss_lime1), terra::ncell(regr_pss_lime2))
expect_equal(terra::nlyr(regr_pss_lime1), 8)
# expect_equal(terra::nlyr(regr_pss_lime2), 6)

# check values
expect_equivalent(terra::global(regr_pss_lime1, "mean", na.rm = TRUE)[[1]][[1]],
                  15.10157, tolerance = 0.0001)
expect_equivalent(terra::global(regr_pss_lime1, "mean", na.rm = TRUE)[[1]][[8]],
                  -0.7605135, tolerance = 0.0001)
# expect_equivalent(terra::global(regr_pss_lime2, "mean", na.rm = TRUE)[[1]][[6]],
#                   4.259218, tolerance = 0.0001)

# check missing features
expect_error(map_breakdown(regr_exp, predictors_agg, maxcell = my_maxcell, type = "lime"))

# check classification model
# data("clas_exp", package = "spatialexplain")
# clas_pss_lime1 = map_lime(clas_exp, predictors_agg, maxcell = my_maxcell, type = "iml")
# expect_equal(terra::nlyr(clas_pss_lime1), 7)
