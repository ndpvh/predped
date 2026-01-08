# Constructor for the [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)

Constructor for the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)

## Usage

``` r
# S4 method for class 'agent'
initialize(
  .Object,
  center,
  radius,
  id = character(0),
  speed = 0.1,
  orientation = 0,
  current_goal = NULL,
  goals = list(),
  group = 0,
  status = "move",
  waiting_counter = 0,
  cell = 0,
  parameters = data.frame(),
  color = "black",
  cell_centers = matrix(0, nrow = 33, ncol = 2),
  utility_variables = data.frame()
)
```

## Arguments

- .Object:

  For this class, should be left unspecified (see Example).

- center:

  Numerical vector of two elements denoting the current position of the
  agent (x and y coordinate).

- radius:

  Numeric denoting the size of agent. As agents are circular, this is
  equal to the radius of the agent.

- id:

  Character that serves as an identifier for the agent. Defaults to an
  empty character, triggering the random generation of an id.

- speed:

  Numeric denoting the current speed of the agent (in m/sec). Defaults
  to `0.1`.

- orientation:

  Numeric denoting the orientation of the agent (in degrees). Defaults
  to `0`.

- current_goal:

  Object of class
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
  which represents the current goal of the agent. Defaults to a
  placeholder of class class
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

- goals:

  List of goals the agent has to achieve. The current goal is not
  included in this list. Defaults to an empty list.

- group:

  Numeric indicating the group to which the agent belongs. Influences
  the behavior of the agent through the social utility functions.
  Defaults to `0`.

- status:

  Character denoting the current status of the agent, or what they will
  be doing to make the next decision. Is one of `"move"` (the agent will
  move to another cell), `"plan"` (the agent will plan the route to
  their next goal), `"reroute"` (the agent will replan the route of
  their current goal), `"reorient"` (the agent will check other movement
  options), `"completing goal"` (the agent will interact with their
  goal), `"exit"` (the agent will exit the space), or `"wait"` (the
  agent will wait for another agent to finish their goal and step out of
  the way). Defaults to `"move"`.

- waiting_counter:

  Numeric defining how long the agent will remain in the status "wait"
  once triggered. At the start of the waiting period is set to be equal
  to the number of iterations the other agent still has to interact with
  their goal plus 2. Defaults to `0`

- cell:

  Numeric denoting the cell the agent will move to. Defaults to `0`.

- parameters:

  Dataframe containing the values of the parameters for the agent.
  Should contain all parameters relevant for the utility functions (use
  `predped::draw_parameters(1)` for an example). Defaults to a random
  set of parameters from the default parameter list in `predped`.

- color:

  Character denoting the color in which the agent should be plotted.

- cell_centers:

  Numeric matrix denoting the positions that the agent may move to at a
  particular iteration. Should usually not be provided or altered by the
  user.

- utility_variables:

  Data.frame precomputed containing values for all the variables
  relevant to compute the utilities. Should usually not be provided or
  altered by the user.

## Value

Object of the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`cell`](https://github.com/ndpvh/predped/reference/cell.md),
[`current_goal`](https://github.com/ndpvh/predped/reference/current_goal.md),
[`goals`](https://github.com/ndpvh/predped/reference/goals.md),
[`group`](https://github.com/ndpvh/predped/reference/group.md),
[`id`](https://github.com/ndpvh/predped/reference/id.md),
[`orientation`](https://github.com/ndpvh/predped/reference/orientation.md),
[`parameters`](https://github.com/ndpvh/predped/reference/parameters.md)
[`position`](https://github.com/ndpvh/predped/reference/position.md),
[`size`](https://github.com/ndpvh/predped/reference/size.md),
[`speed`](https://github.com/ndpvh/predped/reference/speed.md),
[`status`](https://github.com/ndpvh/predped/reference/status.md),
[`waiting_counter`](https://github.com/ndpvh/predped/reference/waiting_counter.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), radius = 0.25)

# Access the two slots that were specified
my_agent@center
#> An object of class "coordinate"
#> [1] 0 0
my_agent@radius
#> [1] 0.25
```
