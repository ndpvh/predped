# predped

<!-- badges: start -->
[![build](https://github.com/ndpvh/predped/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ndpvh/predped/actions/workflows/R-CMD-check.yaml)
[![coverage](https://github.com/ndpvh/predped/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/ndpvh/predped/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

An R package that builds on the m4ma package to estimate and simulate walking behavior.

## Dependencies
```R
install.packages(c('cppRouting', 'TSP', 'pdftools', 'magick'))
remotes::install_github('m4ma/m4ma')
```