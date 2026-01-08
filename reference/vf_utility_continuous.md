# Continuous visual field utility

The idea of this utility function is to let the angle at which you see
your group members play a role in the utility. Here, we distinguish
between using a cosine for the maximization – leading to maximum utility
whenever an agent is directly looking at a group member and to minimum
utility whenever the group member is directly behind the agent – and the
sine – leading to maximum utility whenever the group members are
directly besides the agent and to minimum utility whenever the agent is
either directly behind the group member or the group member behind the
agent.

## Usage

``` r
vf_utility_continuous(b_vf, rel_angles, fx = cos)
```

## Arguments

- b_vf:

  Numeric denoting the slope of the utility function.

- rel_angles:

  Numeric vector containing the relative angle from each cell center to
  the predicted positions of the group members. Typically output of
  [`get_angles`](https://github.com/ndpvh/predped/reference/get_angles.md).

- fx:

  Trigonometric function applied to the relative angles defining what
  their effect is (scaled by `b_vf`). Defaults to `cos`, saying that the
  maximal utility stems from directly looking at a person (orientation
  0). Alternative could be `sin`, which maximizes the utility of looking
  at a person from right next to them (orientation 90).

## Value

Numeric vector containing the utility attributed to keeping the group
members within your visual field. Returns 0's if the agent does not have
any additional group members.

## See also

[`get_angles`](https://github.com/ndpvh/predped/reference/get_angles.md),
`utility-agent`,
[`vf_utility_discrete`](https://github.com/ndpvh/predped/reference/vf_utility_discrete.md)
