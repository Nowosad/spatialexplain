# Predict LIME

For each observation in the raster, the function returns an explanation
of the model prediction using selected implementation of the LIME (Local
Interpretable Model-agnostic Explanations) method.

## Usage

``` r
map_lime(explainer, raster_obs, maxcell = 1000, ..., type = "localModel")
```

## Arguments

- explainer:

  a model to be explained, preprocessed by the
  [`DALEX::explain()`](https://modeloriented.github.io/DALEX/reference/explain.html)
  function

- raster_obs:

  a SpatRaster object with the observations to be explained (predictors
  used in the model)

- maxcell:

  the maximum number of cells in the raster. If the number of cells in
  the raster is greater than `maxcell`, the function will sample
  `maxcell` cells from the raster. By default 1000

- ...:

  additional parameters passed to the
  [`DALEXtra::predict_surrogate()`](https://ModelOriented.github.io/DALEXtra/reference/predict_surrogate.html)
  function

- type:

  the type of the LIME method implementation. Possible values are:
  `"localModel"` (default), `"iml"`, and `"lime"`

## Value

A SpatRaster object with the same dimensions as `raster_obs`. The number
of layers it has may vary depending on the type of the LIME method
implementation

## Examples

``` r
library(terra)
predictors_agg = rast(system.file("raster/predictors_agg.tif", package = "spatialexplain"))
data("regr_exp", package = "spatialexplain")
regr_pss_lm = map_lime(regr_exp, predictors_agg, maxcell = 20)
plot(regr_pss_lm)
```
