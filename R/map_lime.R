#' Predict LIME
#'
#' For each observation in the raster, the function returns an explanation of the model prediction using selected implementation of the LIME (Local Interpretable Model-agnostic Explanations) method.
#'
#' @param explainer a model to be explained, preprocessed by the [`DALEX::explain()`] function
#' @param raster_obs a raster object with the observations to be explained (predictors used in the model)
#' @param maxcell the maximum number of cells in the raster. If the number of cells in the raster is greater than `maxcell`, the function will sample `maxcell` cells from the raster. By default 1000
#' @param ... additional parameters passed to the [`DALEXtra::predict_surrogate()`] function
#' @param type the type of the LIME method implementation. Possible values are: `"localModel"` (default), `"iml"`, and `"lime"`
#'
#' @return A raster object with the same dimensions as `raster_obs`. The number of layers may vary depending on the type of the LIME method implementation
#' @export
#'
#' @examples
#' library(terra)
#' predictors_agg = rast(system.file("raster/predictors_agg.tif", package = "spatialexplain"))
#' data("regr_exp", package = "spatialexplain")
#' regr_pss_lm = map_lime(regr_exp, predictors_agg, maxcell = 20)
#' plot(regr_pss_lm)
map_lime = function(explainer, raster_obs, maxcell = 1000, ..., type = "localModel"){
  if (!requireNamespace("DALEXtra", quietly = TRUE)) {
    stop("The DALEXtra package is required for this function. Please install it and try again.", call. = FALSE)
  }
  if (terra::ncell(raster_obs) > 1.1 * maxcell) {
    raster_obs = terra::spatSample(raster_obs, maxcell, method = "regular",
                                   as.raster = TRUE, warn = FALSE)
  }
  x_df = as.data.frame(raster_obs, na.rm = FALSE)
  if (type %in% c("localModel")){
    result = cbind(model_mean = NA, intercept = NA, x_df)
  } else if (type %in% c("iml")) {
    result = x_df
  } else if (type == "lime"){
    result = x_df
    model_type.dalex_explainer = DALEXtra::model_type.dalex_explainer
    predict_model.dalex_explainer = DALEXtra::predict_model.dalex_explainer
  }
  result[] = NA
  for (i in seq_len(nrow(x_df))){
    if (stats::complete.cases(x_df[i, ])){
      ps = DALEXtra::predict_surrogate(explainer, new_observation = x_df[i, ], ..., type = type) #...,
      if (type == "localModel"){
        ps_df = data.frame(contribution = ps$estimated,
                           variable_name = ps$original_variable,
                           label = ps$model)
        ps_df$variable_name[1:2] = c("model_mean", "intercept")
      } else if (type == "iml"){
        ps_df = data.frame(contribution = ps$results$effect,
                           variable_name = ps$results$feature,
                           label = "iml")
      } else if (type == "lime"){
        ps_df = data.frame(contribution = ps$feature_weight,
                           variable_name = ps$feature,
                           label = ps$model_type)
      }
      ps_df = stats::reshape(ps_df, idvar = "label", timevar = "variable_name", direction = "wide")
      names(ps_df) = gsub("contribution.", "", names(ps_df))
      result[i, which(names(result) %in% colnames(ps_df))] = ps_df[, stats::na.omit(match(names(result), colnames(ps_df)))]
    } else {
      result[i, ] = NA_real_
    }
  }
  r_result = terra::rast(raster_obs, nlyrs = ncol(result)) # should we remove empty layers?
  terra::values(r_result) = result
  names(r_result) = names(result)
  return(r_result)
}
