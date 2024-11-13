library(spatialexplain)
library(terra)

predictors = rast("/vsicurl/https://github.com/Nowosad/IIIRqueR_workshop_materials/raw/refs/heads/main/data/predictors.tif")
plot(predictors, axes = FALSE)

data("regr_exp", package = "spatialexplain")
regr_exp

raster_obs = predictors
explainer = regr_exp
maxcell = 20

if (terra::ncell(raster_obs) > 1.1 * maxcell) {
  raster_obs = terra::spatSample(raster_obs, maxcell, method = "regular",
                                 as.raster = TRUE, warn = FALSE)
}
x_df = as.data.frame(raster_obs, na.rm = FALSE)


library(DALEXtra)
library(lime)
lime_johnny = predict_surrogate(explainer = explainer,
                                 new_observation = x_df[i, ],
                                 n_features = 3,
                                 n_permutations = 1000,
                                 type = "localModel")


if (type %in% c("shap", "oscillations", "oscillations_uni", "oscillations_emp")){
  result = x_df
} else if (type %in% c("break_down")) {
  result = cbind(intercept = NA, x_df)
} else if (type == "break_down_interactions"){
  stop("'break_down_interactions' are not yet implemented. Please contact us if you need this feature",
       call. = FALSE)
}


for (i in seq_len(nrow(x_df))){
  if (stats::complete.cases(x_df[i, ])){
    pp = DALEX::predict_parts(explainer, new_observation = x_df[i, ], ..., N = N, type = type)
    if (type == "shap"){
      pp_mean_contribution = tapply(pp$contribution, pp$variable, mean, na.rm = TRUE)
      pp_df = data.frame(contribution = pp_mean_contribution,
                         variable_name = unique(pp$variable_name),
                         label = unique(pp$label))
    } else if (type %in% c("oscillations", "oscillations_uni", "oscillations_emp")) {
      pp_df = data.frame(contribution = pp$oscillations,
                         variable_name = pp$`_vname_`,
                         label = NA)
    } else {
      pp_df = data.frame(contribution = pp$contribution,
                         variable_name = pp$variable_name,
                         label = pp$label)
    }
    # pp_df$variable_name = ifelse(pp_df$variable_name == "", "prediction", pp_df$variable_name)
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
return(r_result)
