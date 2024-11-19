library(spatialexplain)
library(sf)
library(terra)
library(rpart)
library(DALEX)

temp_train = read_sf("/vsicurl/https://github.com/Nowosad/IIIRqueR_workshop_materials/raw/refs/heads/main/data/temp_train.gpkg")
predictors = rast("/vsicurl/https://github.com/Nowosad/IIIRqueR_workshop_materials/raw/refs/heads/main/data/predictors.tif")
temp = extract(predictors, temp_train, ID = FALSE)
temp_train = cbind(temp_train, temp)
temp_train$temp = cut(temp_train$temp,
                      breaks = c(-Inf, 17, Inf), labels = c("cold", "hot"))
rpart_model_clas = rpart(temp ~ ., data = st_drop_geometry(temp_train))
clas_exp = explain(rpart_model_clas,
                   data = st_drop_geometry(temp_train)[-1],
                   y = temp_train$temp)
usethis::use_data(clas_exp, overwrite = TRUE)
