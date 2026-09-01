# Compute utility variables

This function uses the current state of the environment to determine the
values of a whole range of variables that are used within the utility
functions.

This function uses the current state of the environment to determine the
values of a whole range of variables that are used within the utility
functions.

## Usage

``` r
# S4 method for class 'agent'
compute_utility_variables(
  object,
  state,
  background,
  agent_specifications,
  centers,
  check,
  cpp = TRUE
)

# S4 method for class 'data.frame'
compute_utility_variables(
  object,
  background,
  b_turning = NULL,
  a_turning = NULL,
  time_step = NULL,
  standing_start = 0.25,
  fx = function(x) mean(x, na.rm = TRUE),
  ...
)
```

## Arguments

- object:

  Object of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- state:

  Object of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

- background:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- agent_specifications:

  List created by the
  [`create_agent_specifications`](https://github.com/ndpvh/predped/reference/create_agent_specifications.md)
  function. Contains all information of all agents within the current
  `state` and allows for the communication between the `predped`
  simulation functions and the `m4ma` utility functions.

- centers:

  Numerical matrix containing the coordinates at each position the
  object can be moved to. Should have one row for each cell.

- check:

  Logical matrix of dimensions 11 x 3 denoting whether an agent can move
  to a given cell (`TRUE`) or not (`FALSE`).

- cpp:

  Logical denoting whether to use the Rcpp alternative (`TRUE`) or the R
  alternative of this function (`FALSE`). Defaults to `TRUE`.

- b_turning, a_turning:

  Numeric denoting the values of the parameters \\b\\ and \\a\\ for the
  relationship between orientation and velocity. For more information,
  see the documentation of
  [`compute_centers`](https://github.com/ndpvh/predped/reference/compute_centers.md).
  Alternatively can also be a named numeric vector, where each value in
  the vector denotes the values of the parameters for each agent in the
  data.frame.Defaults to `NULL`, meaning that this relationship takes on
  the default values of `predped`.

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

- fx:

  A summarizing function that should be used to derive the time step if
  it is not provided through the `time_step` argument.

- ...:

  Additional arguments passed on to
  [`to_trace`](https://github.com/ndpvh/predped/reference/to_trace.md)
  and
  [`unpack_trace`](https://github.com/ndpvh/predped/reference/unpack_trace.md)

## Value

Data.frame containing all of the needed variables to be able to compute
the values of the utility functions.

Data.frame containing all of the needed variables to be able to compute
the values of the utility functions.

## See also

[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
`update-agent`, [`update`](https://rdrr.io/r/stats/update.html),
[`update_position`](https://github.com/ndpvh/predped/reference/update_position.md),
[`update`](https://rdrr.io/r/stats/update.html)
