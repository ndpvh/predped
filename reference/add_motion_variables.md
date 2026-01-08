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
  time_step = 0.5,
  threshold = qlnorm(0.25, -2.95, 0.64)/time_step,
  initial_conditions = FALSE
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

  Numeric denoting the time between each iteration. Defaults to `0.5`
  (the same as in [`simulate`](https://rdrr.io/r/stats/simulate.html)).

- threshold:

  Numeric denoting under which observed value for speed the cell to
  which an agent has moved should be put to \`0\`. Defaults to a value
  based on the observed measurement error in our system.

- initial_conditions:

  Logical denoting whether the added columns should include the initial
  conditions (that is, speed, orientation, and position at the previous
  time point) alongside their current alternatives. Useful when one
  wants to compute the values of the utility-related variables from the
  data. Defaults to `FALSE`.

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
# This is my example
```
