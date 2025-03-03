% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_lime.R
\name{map_lime}
\alias{map_lime}
\title{Predict LIME}
\usage{
map_lime(explainer, raster_obs, maxcell = 1000, ..., type = "localModel")
}
\arguments{
\item{explainer}{a model to be explained, preprocessed by the \code{\link[DALEX:explain]{DALEX::explain()}} function}

\item{raster_obs}{a SpatRaster object with the observations to be explained (predictors used in the model)}

\item{maxcell}{the maximum number of cells in the raster. If the number of cells in the raster is greater than \code{maxcell}, the function will sample \code{maxcell} cells from the raster. By default 1000}

\item{...}{additional parameters passed to the \code{\link[DALEXtra:predict_surrogate]{DALEXtra::predict_surrogate()}} function}

\item{type}{the type of the LIME method implementation. Possible values are: \code{"localModel"} (default), \code{"iml"}, and \code{"lime"}}
}
\value{
A SpatRaster object with the same dimensions as \code{raster_obs}. The number of layers it has may vary depending on the type of the LIME method implementation
}
\description{
For each observation in the raster, the function returns an explanation of the model prediction using selected implementation of the LIME (Local Interpretable Model-agnostic Explanations) method.
}
\examples{
library(terra)
predictors_agg = rast(system.file("raster/predictors_agg.tif", package = "spatialexplain"))
data("regr_exp", package = "spatialexplain")
regr_pss_lm = map_lime(regr_exp, predictors_agg, maxcell = 20)
plot(regr_pss_lm)
}
