# Logarithmic Group-Attracted Visual Field Utility (LGVF)

Unifies the previous social utility functions WB, GC and VF into one
utility function. Applies a logarithmic penalty based on distances to
group members, and adds an additional penalty if that member is outside
the extended visual field.

## Usage

``` r
lgvf_utility(
  a_lgvf,
  b_lgvf,
  e_lgvf,
  group_member_data,
  vf_limit = 135 * pi/180
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

  Named list containing the number of pedestrians as an integer (under
  `"nped"`), a list containing a numeric vector of distances of each
  cell center to each group member (under `"distances"`), and a list of
  numeric vectors containing the relative angle at which the group
  member would find itself relative to the orientation of agent when
  moving to a particular cell center.

- vf_limit:

  Numeric denoting the visual field limit (default 135 degrees in
  radians).

## Value

Numeric vector containing the LGVF utility for each cell.

## See also

[`get_group_member_data`](https://github.com/ndpvh/predped/reference/get_group_member_data.md),
`utility-agent`

## Examples

``` r
# TO BE WRITTEN
```
