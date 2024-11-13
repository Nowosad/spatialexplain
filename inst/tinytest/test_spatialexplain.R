library(terra)
predictors_agg = rast(system.file("raster/predictors_agg.tif", package = "spatialexplain"))
data("regr_exp", package = "spatialexplain")
my_maxcell = 20

regr_psp_bd = predict_spatial_parts(regr_exp, predictors_agg, maxcell = my_maxcell)
set.seed(2024-11-13)
regr_psp_shap = predict_spatial_parts(regr_exp, predictors_agg, maxcell = my_maxcell, type = "shap")
regr_psp_os = predict_spatial_parts(regr_exp, predictors_agg, maxcell = my_maxcell, type = "oscillations")
regr_psp_osu = predict_spatial_parts(regr_exp, predictors_agg, maxcell = my_maxcell, type = "oscillations_uni")
regr_psp_ose = predict_spatial_parts(regr_exp, predictors_agg, maxcell = my_maxcell, type = "oscillations_emp")

# check dimensions
expect_equal(terra::ncell(regr_psp_bd), terra::ncell(regr_psp_shap))
expect_equal(terra::nlyr(regr_psp_bd), 7)
expect_equal(terra::nlyr(regr_psp_shap), 6)
expect_equal(terra::nlyr(regr_psp_os), 6)
expect_equal(terra::nlyr(regr_psp_osu), 6)
expect_equal(terra::nlyr(regr_psp_ose), 6)

# check values
expect_equivalent(terra::global(regr_psp_bd, "mean", na.rm = TRUE)[[1]][[1]],
                  15.10157, tolerance = 0.0001)
expect_equivalent(terra::global(regr_psp_shap, "mean", na.rm = TRUE)[[1]][[6]],
                  -0.2943066, tolerance = 0.0001)
expect_equivalent(terra::global(regr_psp_os, "mean", na.rm = TRUE)[[1]][[6]],
                  2.147025, tolerance = 0.0001)
expect_equivalent(terra::global(regr_psp_osu, "mean", na.rm = TRUE)[[1]][[6]],
                  2.375516, tolerance = 0.0001)
expect_equivalent(terra::global(regr_psp_ose, "mean", na.rm = TRUE)[[1]][[6]],
                  2.146239, tolerance = 0.0001)

# check missing features
expect_error(predict_spatial_parts(regr_exp, predictors_agg, maxcell = my_maxcell, type = "break_down_interactions"))
