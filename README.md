
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{rnawebapp}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{rnawebapp}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
rnawebapp::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-03-25 10:23:52 AWST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.2) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> Warning in readLines(old_path): incomplete final line found on
#> '/Users/00104561/.R/Makevars'
#> ── R CMD check results ─────────────────────────────── rnawebapp 0.0.0.9000 ────
#> Duration: 15s
#> 
#> ❯ checking DESCRIPTION meta-information ... NOTE
#>   Malformed Description field: should contain one or more complete sentences.
#>   Non-standard license specification:
#>     What license is it under?
#>   Standardizable: FALSE
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```
