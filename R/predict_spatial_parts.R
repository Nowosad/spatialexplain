#' Predict spatial parts
#'
#' This function calculates the attributions of the model for each observation in the raster.
#'
#' @param explainer a model to be explained, preprocessed by the [`DALEX::explain()`] function
#' @param raster_obs
#' @param maxcell
#' @param ... other parameters that will be passed to [`iBreakDown::break_down()`]
#' @param N the maximum number of observations used for calculation of attributions. By default NULL (use all) or 500 (for oscillations)
#' @param type the type of variable attributions. Either `shap`, `oscillations`, `oscillations_uni`, `oscillations_emp`, `break_down` or `break_down_interactions`
#'
#' @return A raster object with the same dimensions as `raster_obs`. The number of layers equal to the number of variables in the model or the number of variables in the model plus one if `type` is not `shap`.
#' @export
#'
#' @examples
predict_spatial_parts = function(explainer, raster_obs, maxcell = 1000, ...,
                                 N = if (substr(type, 1, 4) == "osci") 500 else NULL,
                                 type = "break_down"){
  if (terra::ncell(raster_obs) > 1.1 * maxcell) {
    raster_obs = terra::spatSample(raster_obs, maxcell, method = "regular",
                                   as.raster = TRUE, warn = FALSE)
  }
  x_df = as.data.frame(raster_obs, na.rm = FALSE)
  if (type == "shap"){
    result = x_df
  } else {
    result = cbind(intercept = NA, x_df, prediction = NA)
  }
  for (i in seq_len(nrow(x_df))){
    if (stats::complete.cases(x_df[i, ])){
      pp = DALEX::predict_parts(explainer, new_observation = x_df[i, ], type = type)
      if (type == "shap"){
        pp_mean_contribution = tapply(pp$contribution, pp$variable, mean, na.rm = TRUE)
        pp_df = data.frame(contribution = pp_mean_contribution,
                           variable_name = unique(pp$variable_name),
                           label = unique(pp$label))
      } else {
        pp_df = data.frame(contribution = pp$contribution,
                           variable_name = pp$variable_name,
                           label = pp$label)
      }
      pp_df$variable_name = ifelse(pp_df$variable_name == "", "prediction", pp_df$variable_name)
      pp_df = stats::reshape(pp_df, idvar = "label", timevar = "variable_name", direction = "wide")
      names(pp_df) = gsub("contribution.", "", names(pp_df))
      # pp_df = tidyr::pivot_wider(pp_df, names_from = variable_name, values_from = contribution)
      if ("prediction" %in% colnames(pp_df)){
        result[i, ] = pp_df[which.max(pp_df$prediction), names(result)]
      } else {
        result[i, ] = pp_df[, names(result)]
      }
    } else {
      result[i, ] = NA_real_
    }
  }
  r_result = terra::rast(raster_obs, nlyrs = ncol(result))
  terra::values(r_result) = result
  names(r_result) = names(result)
  # plot(r_result)
  return(r_result)
}
