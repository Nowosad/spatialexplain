#' Predict oscillations
#'
#' This function calculates the attributions of the model
#' (Oscillations explanations) for each observation in the raster.
#' Model predictions are decomposed into parts that are attributed for particular variables,
#' and then the attributions are assigned to the corresponding cells in the raster.
#' See the documentation of the [`DALEX::predict_parts()`] function for more details.
#'
#' @param explainer a model to be explained, preprocessed by the [`DALEX::explain()`] function
#' @param raster_obs a SpatRaster object with the observations to be explained (predictors used in the model)
#' @param maxcell the maximum number of cells in the raster. If the number of cells in the raster is greater than `maxcell`, the function will sample `maxcell` cells from the raster. By default 1000
#' @param ... other parameters that will be passed to [`iBreakDown::break_down()`]
#' @param N the maximum number of observations used for calculation of attributions. By default 500
#' @param type the type of variable attributions. Either `oscillations_uni` (default), `oscillations_emp`, `oscillations`
#'
#' @return A SpatRaster object with the same dimensions as `raster_obs`. The number of layers in it is equal to the number of variables in the model.
#' @export
#'
#' @references Lundberg, S. (2017). A unified approach to interpreting model predictions. arXiv preprint arXiv:1705.07874.
#' @references Staniak, M., & Biecek, P. (2018). Explanations of model predictions with live and breakDown packages. arXiv preprint arXiv:1804.01955.
#' @references Biecek, P., & Burzykowski, T. (2021). Explanatory model analysis: explore, explain, and examine predictive models. Chapman and Hall/CRC.
#'
#' @examples
#' library(terra)
#' predictors_agg = rast(system.file("raster/predictors_agg.tif", package = "spatialexplain"))
#' data("regr_exp", package = "spatialexplain")
#' regr_psp_bd = map_oscillations(regr_exp, predictors_agg, maxcell = 100)
#' plot(regr_psp_bd)
map_oscillations = function(explainer, raster_obs, maxcell = 1000, ...,
                            N = 500, type = "oscillations_uni"){
  if (terra::ncell(raster_obs) > 1.1 * maxcell) {
    raster_obs = terra::spatSample(raster_obs, maxcell, method = "regular",
                                   as.raster = TRUE, warn = FALSE)
  }
  x_df = as.data.frame(raster_obs, na.rm = FALSE)
  result = x_df
  result[] = NA
  for (i in seq_len(nrow(x_df))){
    if (stats::complete.cases(x_df[i, ])){
      pp = DALEX::predict_parts(explainer, new_observation = x_df[i, ], ..., N = N, type = type)
      pp_df = data.frame(contribution = pp$oscillations,
                         variable_name = pp$`_vname_`,
                         label = NA)
      pp_df = stats::reshape(pp_df, idvar = "label", timevar = "variable_name", direction = "wide")
      names(pp_df) = gsub("contribution.", "", names(pp_df))
      result[i, ] = pp_df[, names(result)]
    } else {
      result[i, ] = NA_real_
    }
  }
  r_result = terra::rast(raster_obs, nlyrs = ncol(result))
  terra::values(r_result) = result
  names(r_result) = names(result)
  return(r_result)
}
