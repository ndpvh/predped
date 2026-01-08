# Update the Position of an Agent

Update the Position of an Agent

## Usage

``` r
update_position(
  agent,
  state,
  background,
  agent_specifications,
  velocities = matrix(rep(c(1.5, 1, 0.5), each = 11), ncol = 3),
  orientations = matrix(rep(c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 327.5, 310, 287.5),
    times = 3), ncol = 3),
  standing_start = 0.1,
  time_step = 0.5,
  report = TRUE,
  print_iteration = TRUE,
  cpp = TRUE
)
```

## Arguments

- agent:

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

- velocities:

  Numeric matrix containing the change in speed for an agent whenever
  they move to the respective cell of this matrix. Is used to create the
  cell positions that the agent might move to. Defaults to a matrix in
  which the columns contain `1.5` (acceleration), `1`, and `0.5`.

- orientations:

  Numeric matrix containing the change in direction for an agent
  whenever they move to the respective cell of this matrix. Is used to
  create the cell positions that the agent might move to. Defaults to a
  matrix in which the rows contain `72.5`, `50`, `32.5`, `20`, `10`,
  `0`, `350`, `340`, `327.5`, `310`, `287.5` (note that the larger
  angles are actually the negative symmetric versions of the smaller
  angles).

- standing_start:

  Numeric denoting the factor of their preferred speed that agents move
  when they just came from standing still. Defaults to `0.1`.

- time_step:

  Numeric denoting the number of seconds each discrete step in time
  should mimic. Defaults to `0.5`, or half a second.

- report:

  Logical denoting whether to report whenever an agent is reorienting.
  Defaults to `FALSE`, and is usually not needed as feedback.

- print_iteration:

  Logical denoting whether to report each simulated iteration. Defaults
  to `FALSE`, but can be switched off if desired.

- cpp:

  Logical denoting whether to use the Rcpp alternatives for several of
  the lower-level functions (`TRUE`) or whether to use the R
  alternatives instead (`FALSE`). Defaults to `TRUE`.

## Value

Object of the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## See also

[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
`update-agent`, [`update`](https://rdrr.io/r/stats/update.html),
[`update_goal`](https://github.com/ndpvh/predped/reference/update_goal.md)
