# predped

<!-- badges: start -->
[![R-CMD-check](https://github.com/m4ma/m4ma/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ndpvh/predped/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/m4ma/m4ma/branch/development/graph/badge.svg?token=PWCVRIDAH7)](https://codecov.io/gh/ndpvh/predped)
<!-- badges: end -->

An R package that builds on the m4ma package to estimate and simulate walking behavior.

## Dependencies
```R
install.packages(c('cppRouting', 'TSP', 'pdftools', 'magick'))
remotes::install_github('m4ma/m4ma')
```