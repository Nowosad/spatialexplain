
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spatialexplain

<!-- badges: start -->

[![R-CMD-check](https://github.com/Nowosad/spatialexplain/workflows/R-CMD-check/badge.svg)](https://github.com/Nowosad/spatialexplain/actions/)
[![Codecov test
coverage](https://codecov.io/gh/Nowosad/spatialexplain/graph/badge.svg)](https://app.codecov.io/gh/Nowosad/spatialexplain)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of **spatialexplain** is to provide model agnostic tools for
exploring and explaining spatial machine learning models. It is based on
the [**DALEX**](https://modeloriented.github.io/DALEX/) package and uses
the **terra** package for spatial raster data handling.

## Installation

You can install the development version of **spatialexplain** from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Nowosad/spatialexplain")
```

<!-- ## Example -->

## Documentation

A set of vignettes that show how to use the package to explain spatial
machine learning models:

1.  [An introduction to spatial
    explanations](https://nowosad.github.io/spatialexplain/articles/An-introduction-to-spatial-explanations.html):
    this one is recommended as a starting point
2.  [Spatial
    break-down](https://nowosad.github.io/spatialexplain/articles/Spatial-break-down.html):
    example of the Break Down method usage
3.  [Spatial
    oscillations](https://nowosad.github.io/spatialexplain/articles/Spatial-oscillations.html):
    example of the Oscillations method usage
4.  [Spatial
    SHAP](https://nowosad.github.io/spatialexplain/articles/Spatial-SHAP.html):
    example of the SHAP method usage
5.  [Spatial
    LIME](https://nowosad.github.io/spatialexplain/articles/Spatial-LIME.html):
    example of the LIME method usage

## Contribution

Contributions to this package are welcome - let us know if you have any
suggestions or spotted a bug. The preferred method of contribution is
through a GitHub pull request. Feel also free to contact us by creating
[an issue](https://github.com/nowosad/spatialexplain/issues).
