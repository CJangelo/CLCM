
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CLCM - Confirmatory Latent Class Models

<!-- badges: start -->
<!-- badges: end -->

CLCM, an R package for estimating confirmatory latent class models with
clinical data types.

## Description

Estimate confirmatory latent class models for a variety of item response
types that are encountered in the clinical field. One or two timepoints
are supported. Latent regression estimation can be performed, allowing
for comparisons of longitudinal latent class assignments (e.g.,
treatment success/failure) across observed groups (e.g., treatment arms
in clinical trials). Fit statistics (C2, AIC/BIC) are available as well.

## Installation

You can install the released version of CLCM from github:

``` r
install.packages('devtools')
library(devtools)
devtools::install_github("CJangelo/CLCM")
```

The goal is to release this package on CRAN - I am looking for
volunteers to help me with testing required to support a CRAN
submission.

## Examples

For concise examples of how these models can be applied to clinical
data, see the Vignettes under `Articles` at the top of this page.

## License

This R package is free and open source software (License: GPL (&gt;=
3)).
