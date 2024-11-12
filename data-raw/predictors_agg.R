library(terra)
predictors = rast("/vsicurl/https://github.com/Nowosad/IIIRqueR_workshop_materials/raw/refs/heads/main/data/predictors.tif")
predictors_agg = aggregate(predictors, 8)

plot(predictors)
plot(predictors_agg)

writeRaster(predictors_agg, "inst/raster/predictors_agg.tif", overwrite = TRUE)
