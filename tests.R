library(sf)
library(terra)
library(rpart)
library(DALEX)
library(DALEXtra)

temp_train = read_sf("/vsicurl/https://github.com/Nowosad/IIIRqueR_workshop_materials/raw/refs/heads/main/data/temp_train.gpkg")
plot(temp_train)

predictors = rast("/vsicurl/https://github.com/Nowosad/IIIRqueR_workshop_materials/raw/refs/heads/main/data/predictors.tif")
plot(predictors, axes = FALSE)

temp = extract(predictors, temp_train, ID = FALSE)
temp_train = cbind(temp_train, temp)
head(temp_train)

grid_raster = rast(predictors)
grid_raster

rpart_model = rpart(temp ~ ., data = st_drop_geometry(temp_train))

regr_exp = explain(rpart_model,
                   data = st_drop_geometry(temp_train)[-1],
                   y = temp_train$temp)

devtools::document()
devtools::load_all()
regr_pps1 = predict_spatial_parts(regr_exp, predictors, maxcell = 20)
plot(regr_pps1)

regr_pps2 = predict_spatial_parts(regr_exp, predictors, maxcell = 20, type = "shap")
plot(regr_pps2)

raster_obs = predictors
explainer = regr_exp
maxcell = 20

if (terra::ncell(raster_obs) > 1.1 * maxcell) {
  raster_obs = terra::spatSample(raster_obs, maxcell, method = "regular",
                                 as.raster = TRUE, warn = FALSE)
}
x_df = as.data.frame(raster_obs, na.rm = FALSE)
if (type == "shap"){
  result = x_df
} else {
  result = cbind(intercept = NA, x_df, pred = NA)
}
for (i in seq_len(nrow(x_df))){
  if (complete.cases(x_df[i, ])){
    pp = DALEX::predict_parts(explainer, new_observation = x_df[i, ], type = "break_down_interactions")
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
    pp_df = tidyr::pivot_wider(pp_df, names_from = variable_name, values_from = contribution)
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
plot(r_result)


mean =
mean
pp
