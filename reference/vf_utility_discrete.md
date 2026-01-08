# Discrete visual field utility

The idea of this utility function is that it doesn't matter at which
angle you see a group member within the visual field, as long as you see
them. This translates to a discrete added disutility whenever the group
member falls inside the non-visual zone behind the agent.

## Usage

``` r
vf_utility_discrete(b_vf, rel_angles)
```

## Arguments

- b_vf:

  Numeric denoting the slope of the utility function.

- rel_angles:

  Numeric vector containing the relative angle from each cell center to
  the predicted positions of the group members. Typically output of
  [`get_angles`](https://github.com/ndpvh/predped/reference/get_angles.md).

## Value

Numeric vector containing the utility attributed to keeping the group
members within your visual field. Returns 0's if the agent does not have
any additional group members.

## See also

[`get_angles`](https://github.com/ndpvh/predped/reference/get_angles.md),
`utility-agent`,
[`vf_utility_continuous`](https://github.com/ndpvh/predped/reference/vf_utility_continuous.md)
