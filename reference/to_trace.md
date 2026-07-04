# Transform data to a trace

This function does the opposite of
[`unpack_trace`](https://github.com/ndpvh/predped/reference/unpack_trace.md).
It takes in a data.frame and return a trace according to `predped`s
requirements. The data.frame should at least have the column names "x",
"y", "time", and "id", containing the coordinates, times at which the
data were gathered (in seconds), and the id-number of the person whose
data it is. Additionally, data.frame needs information on the goals that
agents were trying to achieve at each timepoint, of which their
positions should be saved under "goal_x" and "goal_y", and their id to
"goal_id".

## Usage

``` r
to_trace(
  data,
  background,
  b_turning = NULL,
  a_turning = NULL,
  velocities = c(1.5, 1, 0.5),
  orientations = c(72.5, 50, 32.5, 20, 10, 0, -10, -20, -32.5, -50, -72.5),
  time_step = NULL,
  standing_start = 0.25,
  stay_stopped = TRUE,
  fx = function(x) mean(x, na.rm = TRUE),
  cpp = TRUE,
  ...
)
```

## Arguments

- data:

  Instance of a data.frame containing the data you want to transform.

- background:

  Instance of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
  containing the setting in which the data were gathered.

- b_turning, a_turning:

  Numeric denoting the values of the parameters \\b\\ and \\a\\ for the
  relationship between orientation and velocity. For more information,
  see the documentation of
  [`compute_centers`](https://github.com/ndpvh/predped/reference/compute_centers.md).
  Alternatively can also be a named numeric vector, where each value in
  the vector denotes the values of the parameters for each agent in the
  data.frame.Defaults to `NULL`, meaning that this relationship takes on
  the default values of `predped`.

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

- stay_stopped:

  Logical denoting whether agents will predict others that are currently
  not moving to remain immobile in the next iteration. Is needed to
  compute the utility variables accurately. Defaults to `TRUE`.

- fx:

  A summarizing function that should be used to derive the time step if
  it is not provided through the `time_step` argument.

- cpp:

  Logical denoting whether to use the Rcpp (`TRUE`) or R (`FALSE`)
  version of this function. Defaults to `TRUE`.

- ...:

  Arguments passed to
  [`find_path`](https://github.com/ndpvh/predped/reference/find_path.md).

## Value

Object of the
[`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md).

## Examples

``` r
# This is my example
```
