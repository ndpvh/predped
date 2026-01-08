# Compute the utilities with all utility variables known

This function uses the values of the relevant variables used as input in
the utility functions to derive the utility for each of the different
moving options.

## Usage

``` r
# S4 method for class 'data.frame'
utility(object, parameters, cpp = TRUE)
```

## Arguments

- object:

  Dataframe containing all of the needed information to compute the
  utilities. Typically output of the `compute_utility_variables`
  function.

- parameters:

  Dataframe containing the parameters of the agent. Should conform to
  the naming conventions mentioned in
  [`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md).

- cpp:

  Logical denoting whether to use the Rcpp version of the function
  (`TRUE`) or the R version (`FALSE`). Defaults to `TRUE`.

## Value

Numeric vector denoting the (dis)utility of moving to each of the cells.

## See also

[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
`update-agent`, [`update`](https://rdrr.io/r/stats/update.html),
`utility-agent`, `compute_utility_variables`,
[`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md),
[`update_position`](https://github.com/ndpvh/predped/reference/update_position.md)
