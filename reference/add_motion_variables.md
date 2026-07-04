# Add motion variables to data

This function adds several motion variables to an already existing
dataset. These motion variables are then used by `predped` to compute
utilities, allowing for estimations in the long run. The variables that
are added are speed, orientation, and the cell to which a person moved
(as defined by the M4MA).

## Usage

``` r
add_motion_variables(
  data,
  velocities = c(1.5, 1, 0.5),
  orientations = c(72.5, 50, 32.5, 20, 10, 0, -72.5, -50, -32.5, -20, -10),
  time_step = NULL,
  standing_start = 0.25,
  initial_conditions = FALSE,
  a_turning = 2,
  b_turning = 0.2,
  fx = function(x) mean(x, na.rm = TRUE)
)
```

## Arguments

- data:

  Instance of a data.frame containing the data you want to transform.

- velocities:

  Numeric vector denoting the changes in speeds as assumed by the M4MA.
  Defaults to `1.5` (acceleration), `1`, and `0.5` (deceleration).

- orientations:

  Numeric vector denoting the changes in orientation as assumed by the
  M4MA. Defaults to `72.5`, `50`, `32.5`, `20`, `10`, `0`, `350`, `340`,
  `327.5`, `310`, `287.5` (note that the larger angles are actually the
  negative symmetric versions of the smaller angles).

- time_step:

  Numeric denoting the time between each iteration. Defaults to `NULL`,
  in which case the time step is derived by the
  [`get_time_step`](https://github.com/ndpvh/predped/reference/get_time_step.md)
  function with summarizing function `fx`.

- standing_start:

  Numeric denoting the speed below which the cell is set to `0`
  (stopped). Matches the `standing_start` parameter in
  [`update_position`](https://github.com/ndpvh/predped/reference/update_position.md).
  Defaults to `0.25`.

- initial_conditions:

  Logical denoting whether the added columns should include the initial
  conditions (that is, speed, orientation, and position at the previous
  time point) alongside their current alternatives. Useful when one
  wants to compute the values of the utility-related variables from the
  data. Defaults to `FALSE`.

- b_turning, a_turning:

  Numeric denoting the values of the parameters \\b\\ and \\a\\ for the
  relationship between orientation and velocity. For more information,
  see the documentation of
  [`compute_centers`](https://github.com/ndpvh/predped/reference/compute_centers.md).
  Alternatively can also be a named numeric vector, where each value in
  the vector denotes the values of the parameters for each agent in the
  data.frame.Defaults to `NULL`, meaning that this relationship takes on
  the default values of `predped`.

- fx:

  A summarizing function that should be used to derive the time step if
  it is not provided through the `time_step` argument.

## Value

A data.frame with predped-relevant motion variables derived from the
provided data, including the speeds and orientations, cells that the
agents moved to, the status of the agents, and the original positions,
ids, and goals that were included in the original dataset.

## Details

The provided dataset should at least have the following columns: - `x`,
`y`: Coordinates at which a person was standing at a given time -
`time`: A continuous variable that denotes the time at which the
measurement took place. - `id`: The identifier given to the person whose
position was measured. - `goal_id`: The identifier given to the goal the
person had to move towards while their position was being measured. -
`goal_x`, `goal_y`: The position of the goal the person had to move to
while their position was being measured.

## Examples

``` r
# Create a minimal working example dataset from scratch
data <- data.frame(
  "time" = seq(0, 10, 1),
  "id" = rep("participant", 11),
  "x" = cos(seq(0, 2 * pi, length.out = 11)),
  "y" = sin(seq(0, 2 * pi, length.out = 11)),
  "goal_id" = rep("goal", 11),
  "goal_x" = rep(0, 11),
  "goal_y" = rep(0, 11)
)

# Add the motion variables to this dataset
add_motion_variables(data)
#>                iteration time          id         x             y goal_id
#> participant.3          3    3 participant  0.309017  9.510565e-01    goal
#> participant.4          4    4 participant -0.309017  9.510565e-01    goal
#> participant.5          5    5 participant -0.809017  5.877853e-01    goal
#> participant.6          6    6 participant -1.000000  1.224647e-16    goal
#> participant.7          7    7 participant -0.809017 -5.877853e-01    goal
#> participant.8          8    8 participant -0.309017 -9.510565e-01    goal
#> participant.9          9    9 participant  0.309017 -9.510565e-01    goal
#> participant.10        10   10 participant  0.809017 -5.877853e-01    goal
#> participant.11        11   11 participant  1.000000 -2.449294e-16    goal
#>                goal_x goal_y    speed orientation status cell
#> participant.3       0      0 0.618034         144   move   14
#> participant.4       0      0 0.618034         180   move   14
#> participant.5       0      0 0.618034         216   move   14
#> participant.6       0      0 0.618034         252   move   14
#> participant.7       0      0 0.618034         288   move   14
#> participant.8       0      0 0.618034         324   move   14
#> participant.9       0      0 0.618034           0   move   14
#> participant.10      0      0 0.618034          36   move   14
#> participant.11      0      0 0.618034          72   move   14

```
