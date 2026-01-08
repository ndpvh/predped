# Look for the best direction to head in

This functions scans the environment and computes the utility of each
possible direction. Used in
[`update_position`](https://github.com/ndpvh/predped/reference/update_position.md)
when the agent has to reorient itself.

## Usage

``` r
best_angle(
  agent,
  state,
  background,
  agent_specifications,
  velocities,
  orientations,
  time_step = 0.5,
  step = 45,
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

  List containing the specifications of all agents in the state. Is
  provided here as an argument to increase computational speed, as you
  only have to define these specifications once (instead of multiple
  times) per iteration. This list is furthermore needed to make the
  connection with the `m4ma` package.

- velocities:

  Numerical matrix that contains the change in velocity per cell that
  the agent might move to.

- orientations:

  Numerical matrix that contains the change in orientation per cell that
  the agent might move to.

- time_step:

  Numeric denoting the time step in seconds. Defaults to `0.5`

- step:

  Numeric denoting the change in angle for looking around in degrees.
  Defaults to `45`, meaning that the agent looks around exhaustively in
  8 different directions (360 / 45) by default.

- cpp:

  Logical denoting whether to use the R or Rcpp version of the function.
  Defaults to `TRUE`.

## Value

Numeric denoting the angle or direction that had the highest utility (in
degrees).

## See also

[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
[`update`](https://rdrr.io/r/stats/update.html), `utility-agent`
