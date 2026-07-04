# Get Distances and Angles to Group Members

Finds the predicted positions of the group members and calculates the
distances and relative angles from all candidate cells to each member.
This functions as the alternative to distance_group_centroid and
get_angles for the lgvf_utility.

## Usage

``` r
get_group_member_data(
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
