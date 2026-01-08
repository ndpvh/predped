# Transform to real axis

Use the probit (positive, bounded) definition of the parameters and
transform them to a real (normal, unbounded) scale.

## Usage

``` r
to_unbounded(parameters, bounds)
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
[`to_bounded`](https://github.com/ndpvh/predped/reference/to_bounded.md),
[`to_covariance`](https://github.com/ndpvh/predped/reference/to_covariance.md)
