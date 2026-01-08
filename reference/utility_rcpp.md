# Utility

This function is the Rcpp equivalent of `utility-agent`. It takes in a
dataframe containing all of the relevant values for computing the
utility, as well as a dataframe containing the parameters. Heavily
depends on the `m4ma` package.

## Usage

``` r
utility_rcpp(data, parameters)
```

## Arguments

- data:

  Dataframe containing all of the needed information to compute the
  utilities. Typically output of the `compute_utility_variables`
  function.

- parameters:

  Dataframe containing the parameters of the agent. Should conform to
  the naming conventions mentioned in
  [`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md).

## Value

Numeric vector denoting the (dis)utility of moving to each of the
potential cells.

## See also

[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
`update-agent`, [`update`](https://rdrr.io/r/stats/update.html),
`utility-agent`, `compute_utility_variables`,
[`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md),
[`update_position`](https://github.com/ndpvh/predped/reference/update_position.md)
