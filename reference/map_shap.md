# Predict SHAP

This function calculates the attributions of the model (SHAP
explanations) for each observation in the raster. Model predictions are
decomposed into parts that are attributed for particular variables, and
then the attributions are assigned to the corresponding cells in the
raster. See the documentation of the
[`DALEX::predict_parts()`](https://modeloriented.github.io/DALEX/reference/predict_parts.html)
function for more details.

## Usage

``` r
map_shap(explainer, raster_obs, maxcell = 1000, ..., N = NULL, type = "shap")
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

  other parameters that will be passed to
  [`iBreakDown::break_down()`](https://ModelOriented.github.io/iBreakDown/reference/break_down.html)

- N:

  the maximum number of observations used for calculation of
  attributions. By default NULL (use all)

- type:

  the type of variable attributions. Only `"shap"` is implemented at the
  moment.

## Value

A SpatRaster object with the same dimensions as `raster_obs`. The number
of layers in it is equal to the number of variables in the model.

## References

Lundberg, S. (2017). A unified approach to interpreting model
predictions. arXiv preprint arXiv:1705.07874.

Staniak, M., & Biecek, P. (2018). Explanations of model predictions with
live and breakDown packages. arXiv preprint arXiv:1804.01955.

Biecek, P., & Burzykowski, T. (2021). Explanatory model analysis:
explore, explain, and examine predictive models. Chapman and Hall/CRC.

## Examples

``` r
library(terra)
predictors_agg = rast(system.file("raster/predictors_agg.tif", package = "spatialexplain"))
data("regr_exp", package = "spatialexplain")
regr_psp_bd = map_shap(regr_exp, predictors_agg, maxcell = 100)
plot(regr_psp_bd)
```
