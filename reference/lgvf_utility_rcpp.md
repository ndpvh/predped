# Logarithmic Group-Attracted Visual Field Utility (LGVF)

Rcpp alternative to the `lgvf_utility` function.

## Usage

``` r
lgvf_utility_rcpp(
  a_lgvf,
  b_lgvf,
  e_lgvf,
  group_member_data,
  vf_limit = 135 * M_PI/180
)
```

## Arguments

- a_lgvf:

  Numeric denoting the exponent (shape) of the utility function.

- b_lgvf:

  Numeric denoting the slope (weight) of the utility function.

- e_lgvf:

  Numeric denoting the optimal comfortable distance (epsilon) to
  maintain.

- group_member_data:

  List containing distances and relative angles to members.

- vf_limit:

  Numeric denoting the visual field limit (default 135 degrees in
  radians).

## Value

Numeric vector containing the LGVF utility for each cell.

## See also

[`get_group_member_data_rcpp`](https://github.com/ndpvh/predped/reference/get_group_member_data_rcpp.md),
`utility-agent`
