# Transform trace to time-series

Transform trace to time-series

## Usage

``` r
time_series(trace, time_step = 0.5, cpp = TRUE)
```

## Arguments

- trace:

  List of objects of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)

- time_step:

  Numeric denoting the time between each iteration. Defaults to `0.5`
  (the same as in [`simulate`](https://rdrr.io/r/stats/simulate.html)).

- cpp:

  Logical denoting whether to use the Rcpp (`TRUE`) or R (`FALSE`)
  version of this function. Defaults to `TRUE`.

## Examples

``` r
# This is my example
```
