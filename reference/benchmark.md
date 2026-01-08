# Benchmark the package

Function that allows users to benchmark predped on their own device.
Times are measured through repeated assessments with
[`Sys.time()`](https://rdrr.io/r/base/Sys.time.html), providing us with
a measure of the user-based time for the functions. Importantly, this is
different from pure execution time of the function, which may be less
than the user time.

## Usage

``` r
benchmark(
  x = NULL,
  iterations = 100,
  summarize = TRUE,
  digits = 2,
  progress = TRUE
)
```

## Arguments

- x:

  Character or character vector containing the names of the functions to
  include in the benchmark. Functions should be part of the package.
  Defaults to `NULL`, denoting that all functions will be tested.

- iterations:

  Integer denoting the number of times to repeat each function's
  benchmark case. The higher this number, the longer the benchmark will
  take but also the more accurate the distribution of times will be per
  function. Defaults to `100`.

- summarize:

  Logical denoting whether to summarize the results of the benchmark. If
  `TRUE`, the function will output an HTML file containing the
  summarized results of the benchmark (mean, standard deviation, and 2.5
  will be a list of lists containing the raw execution times per
  function and per benchmark case for the function. Defaults to `TRUE`.

- digits:

  Integer denoting the decimal points to which to round to. Ignored if
  `summarize = FALSE`. Defaults to `2`.

- progress:

  Logical denoting whether to add a progress bar for the benchmarks.
  Defaults to `TRUE`.

## Value

Either a list of lists containing the raw execution times per benchmark
case per function (if `summarize = FALSE`) or an HTML file summarizing
the results of the benchmark (if `summarize = TRUE`)

## Examples

``` r
if (FALSE) { # \dontrun{
# Run benchmark for in_object with 100 iterations
benchmark(
    x = "in_object",
    iterations = 100
)

# Run benchmark for all functions with 10 iterations
benchmark(
   iterations = 10
)
} # }
```
