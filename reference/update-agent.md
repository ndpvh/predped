# Update Agent

Update the current agent by first updating its goal status (i.e.,
checking how and whether an agent can start interacting with a goal, has
to reorient, etc) and then updating its position (i.e., using the
utility functions to determine where the agent will move next).

## Usage

``` r
# S4 method for class 'agent'
update(
  object,
  state,
  background,
  agent_specifications,
  seen,
  velocities = matrix(rep(c(1.5, 1, 0.5), each = 11), ncol = 3),
  orientations = matrix(rep(c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 327.5, 310, 287.5),
    times = 3), ncol = 3),
  close_enough = 2,
  space_between = 1.25,
  precomputed_edges = NULL,
  many_nodes = !is.null(precomputed_edges),
  adaptive_goal_sorting = TRUE,
  standing_start = 0.1,
  time_step = 0.5,
  report = FALSE,
  print_iteration = FALSE,
  cpp = TRUE
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

- seen:

  Logical indicating whether the agent can see the path point to which
  they are currently moving.

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

- close_enough:

  Numeric denoting how close (in radii) the agent needs to be to an
  object in order to interact with it. Defaults to `2`, meaning the
  agent can interact with objects at `2 * radius(agent)` distance away.

- space_between:

  Numeric denoting the space that should be left between an object and
  the created path points for the agents (in radii). Defaults to `2.5`,
  meaning a space of `2.5 * radius(agent)` is left between an object and
  the path points agents use in their strategy.

- precomputed_edges:

  Output of
  [`compute_edges`](https://github.com/ndpvh/predped/reference/compute_edges.md)
  containing the nodes and edges the agent can use to plan its path.
  Defauls to `NULL`, triggering the creation of these edges whenever
  they are needed.

- many_nodes:

  Logical denoting whether to use the minimal number of nodes or to use
  many more (see
  [`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md)).
  Ignored if `precomputed_edges` is provided. Defaults to `FALSE`.

- adaptive_goal_sorting:

  Logical denoting whether agents have the ability to change the order
  of their goals adaptively throughout the simulation. Defaults to
  `TRUE`.

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
[`update`](https://rdrr.io/r/stats/update.html),
[`update_goal`](https://github.com/ndpvh/predped/reference/update_goal.md),
[`update_position`](https://github.com/ndpvh/predped/reference/update_position.md)
