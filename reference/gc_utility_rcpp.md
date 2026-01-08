# Group centroid utility

Rcpp alternative for the group centroid utility function.

## Usage

``` r
gc_utility_rcpp(
  a_group_centroid,
  b_group_centroid,
  radius,
  cell_distances,
  stop_utility,
  number_agents
)
```

## Arguments

- a_group_centroid:

  Numeric denoting the power to which to take the utility.

- b_group_centroid:

  Numeric denoting the slope of the utility function.

- radius:

  Numeric denoting the radius of the agent.

- cell_distances:

  Numeric vector denoting the distance of each cell in the `centers` to
  the predicted group centroid.

- stop_utility:

  Numeric denoting the utility of stopping. Is used to ensure the agents
  do not freeze when they are too far away from each other.

- number_agents:

  Numeric denoting the number of ingroup members.

## Value

Numeric vector containing the group-centroid-related utility for each
cell.

## See also

[`distance_group_centroid`](https://github.com/ndpvh/predped/reference/distance_group_centroid.md),
[`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md),
`utility-agent`
