# An S4 Class to Represent Agents.

Defines the `agent` class, which contains all characteristics of an
agent. Some of these characteristics are time-independent (e.g.,
parameters and set of goals) while others change during each time step
in the simulation or estimation (e.g., speed and orientation).

## Slots

- `id`:

  Character that serves as an identifier for the agent.

- `center`:

  Numerical vector of two elements denoting the current position of the
  agent (x and y coordinate).

- `radius`:

  Numeric denoting the size of agent. As agents are circular, this is
  equal to the radius of the agent.

- `speed`:

  Numeric denoting the current speed of the agent (in m/sec).

- `orientation`:

  Numeric denoting the orientation of the agent (in degrees)

- `group`:

  Numeric indicating the group to which the agent belongs. Influences
  the behavior of the agent through the social utility functions.

- `status`:

  Character denoting the current status of the agent, or what they will
  be doing to make the next decision. Is one of "move" (the agent will
  move to another cell), "plan" (the agent will plan the route to their
  next goal), "reroute" (the agent will replan the route of their
  current goal), "reorient" (the agent will check other movement
  options), "completing goal" (the agent will interact with their goal),
  "exit" (the agent will exit the space), or "wait" (the agent will wait
  for another agent to finish their goal and step out of the way).

- `waiting_counter`:

  Numeric defining how long the agent will remain in the status "wait"
  once triggered. At the start of the waiting period is set to be equal
  to the number of iterations the other agent still has to interact with
  their goal plus 2.

- `cell`:

  Numeric denoting the cell the agent will move to.

- `current_goal`:

  Object of class
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
  which represents the current goal of the agent.

- `goals`:

  List of goals the agent has to achieve. The current goal is not
  included in this list.

- `parameters`:

  Dataframe containing the values of the parameters for the agent.
  Should contain all parameters relevant for the utility functions (use
  `predped::draw_parameters(1)` for an example).

- `color`:

  Character denoting the color in which the agent should be plotted.

- `cell_centers`:

  Numeric matrix denoting the positions that the agent may move to at a
  particular iteration.

- `utility_variables`:

  Data.frame precomputed containing values for all the variables
  relevant to compute the utilities.

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
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
[`waiting_counter`](https://github.com/ndpvh/predped/reference/waiting_counter.md),
`initialize-agent`
