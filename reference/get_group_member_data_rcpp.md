# Get Distances and Angles to Group Members

Rcpp version of
[`get_group_member_data`](https://github.com/ndpvh/predped/reference/get_group_member_data.md).

## Usage

``` r
get_group_member_data_rcpp(
  agent_idx,
  agent_group,
  position,
  orientation,
  predictions,
  centers
)
```

## Arguments

- agent_idx:

  Numeric denoting the position of the agent in the predictions.

- agent_group:

  Numeric vector with the group membership of all pedestrians.

- position:

  Numeric vector denoting the current position of the agent.

- orientation:

  Numeric denoting the current orientation of the agent.

- predictions:

  Numeric matrix with shape N x 2 containing predicted positions.

- centers:

  Numerical matrix containing the coordinates at each candidate cell.

## Value

A list containing the distances, relative angles and number of group
members.

## See also

[`lgvf_utility`](https://github.com/ndpvh/predped/reference/lgvf_utility.md),
`utility-agent`
