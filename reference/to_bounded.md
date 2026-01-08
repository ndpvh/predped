# Transform to positive axis

Use the real (normal, unbounded) definition of the parameters and
transform them to a positive (probit, bounded) scale.

## Usage

``` r
to_bounded(parameters, bounds)
```

## Arguments

- parameters:

  Dataframe or named list containing values for the parameters of M4MA.

- bounds:

  Named numeric matrix containing the bounds for the parameters of M4MA.

## Value

Dataframe or named list containing the transformed parameters

## See also

[`generate_parameters`](https://github.com/ndpvh/predped/reference/generate_parameters.md),
[`plot_distribution`](https://github.com/ndpvh/predped/reference/plot_distribution.md),
[`to_unbounded`](https://github.com/ndpvh/predped/reference/to_unbounded.md),
[`to_covariance`](https://github.com/ndpvh/predped/reference/to_covariance.md)
