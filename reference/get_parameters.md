# Get parameters

Define a parameter list containing means, covariances, and bounds based
on either provided user-input or on a provided file. Serves as an
internal function.

## Usage

``` r
get_parameters(
  filename = NULL,
  sep = ",",
  archetype = "BaselineEuropean",
  mean = NULL,
  Sigma = NULL,
  bounds = NULL
)
```

## Arguments

- filename:

  Character denoting the path to a file containing parameters. Defaults
  to `NULL`, triggering reading in the csv-files that come with predped.

- sep:

  Character denoting the separator in case `x` is a delimited file.
  Defaults to `","`.

- archetype:

  String denoting the archetype to be used for the covariance matrix.
  Ignored if `Sigma` is provided. Defaults to `"BaselineEuropean"`.

- mean:

  Dataframe containing the means for each of the parameters for a given
  agent. Defaults to `NULL`, triggering reading in the data instead.

- Sigma:

  Either a covariance matrix that defines the individual differences on
  each of the parameters (when `transform_covariance = FALSE`), or a
  matrix containing standard deviations for each of the parameters on
  its diagonal and correlations between the parameters on its
  off-diagonal (when `transform_covariance = TRUE`; see
  [`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md)).
  Default covariance matrices exist for each of the archetypes in
  [`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md)
  and thus changes with the value of `archetype`. Defaults to `NULL`,
  triggering reading in the data (but only if
  `individual_differences = TRUE`).

- bounds:

  Named numeric matrix containing the bounds for each of the parameters.
  Usually provided in the parameter-list under name `"params_bounds"`.
  Defaults to `NULL`, triggering reading in the data (but only if
  `individual_differences = TRUE`).

## Value

Named list containing slots `"mean"`, `"Sigma"`, and `"bounds"`.
