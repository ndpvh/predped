# Transform user-provided matrix to covariance matrix

Use the inputted matrix to construct the covariance matrix for the
parameters.

## Usage

``` r
to_covariance(X)
```

## Arguments

- X:

  A d x d matrix containing the standard deviations of the parameters on
  the diagonal and the correlations between parameters off the diagonal.

## Value

Covariance matrix

## See also

[`generate_parameters`](https://github.com/ndpvh/predped/reference/generate_parameters.md),
[`plot_distribution`](https://github.com/ndpvh/predped/reference/plot_distribution.md),
[`to_bounded`](https://github.com/ndpvh/predped/reference/to_bounded.md),
[`to_unbounded`](https://github.com/ndpvh/predped/reference/to_unbounded.md)
