# Plot prior parameter distributions

Wrapper of the
[`generate_parameters`](https://github.com/ndpvh/predped/reference/generate_parameters.md)
function that uses the provided arguments to visualize the distribution
of parameter values under the current specifications.

## Usage

``` r
plot_distribution(...)
```

## Arguments

- ...:

  Arguments provided to the
  [`generate_parameters`](https://github.com/ndpvh/predped/reference/generate_parameters.md).

## Value

Plotted histograms of the prior distribution of parameters under the
provided specifications.

## See also

[`generate_parameters`](https://github.com/ndpvh/predped/reference/generate_parameters.md),
[`load_parameters`](https://github.com/ndpvh/predped/reference/load_parameters.md),
[`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md),
[`to_bounded`](https://github.com/ndpvh/predped/reference/to_bounded.md),
[`to_covariance`](https://github.com/ndpvh/predped/reference/to_covariance.md),
[`to_unbounded`](https://github.com/ndpvh/predped/reference/to_unbounded.md)
