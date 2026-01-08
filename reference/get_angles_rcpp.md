# Angle between agent and group members

Finds the angle at which the group members are located compared to the
agent. Uses the predicted positions of the group members for this.

## Usage

``` r
get_angles_rcpp(
  agent_idx,
  agent_group,
  position,
  orientation,
  predictions,
  centers,
  any_member = TRUE
)
```

## Arguments

- agent_idx:

  Numeric denoting the position of the agent in the prediction matrix
  `p_pred`.

- agent_group:

  Numeric vector with the group membership of all pedestrians.

- position:

  Numeric vector denoting the current position of the agent.

- orientation:

  Numeric denoting the current orientation of the agent.

- predictions:

  Numeric matrix with shape N x 2 containing predicted positions of all
  pedestrians that belong to the social group of the agent.

- centers:

  Numerical matrix containing the coordinates at each position the
  object can be moved to. Should have one row for each cell.

- any_member:

  Logical denoting whether to consider the angles of all group members
  (`TRUE`) – effectively saying that it doesn't matter which group
  member the agent can see, as long as they can see one – or whether to
  only consider the nearest group member (`FALSE`). Defaults to `TRUE`.

## Value

Numeric vector containing the relative angle of the group member(s)
compared to the orientation of the agent within a given cell in
`centers`.

## See also

`utility-agent`
[`vf_utility_continuous`](https://github.com/ndpvh/predped/reference/vf_utility_continuous.md)
[`vf_utility_discrete`](https://github.com/ndpvh/predped/reference/vf_utility_discrete.md)
