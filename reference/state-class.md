# An S4 Class to Represent the State

An S4 Class to Represent the State

## Slots

- `iteration`:

  Numeric denoting the iteration number.

- `setting`:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- `agents`:

  List containing objects of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)
  representing the agents that are currently walking around in the
  `setting`.

- `potential_agents`:

  List containing objects of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)
  representing agents that are waiting to enter the `setting`.

- `iteration_variables`:

  Dataframe containing values for variables that control the simulation
  under the hood, such as `max_agents`.

- `variables`:

  List of user-specified variables that can be used to control the
  simulation, see the `fx` argument of
  [`simulate`](https://rdrr.io/r/stats/simulate.html)

## See also

[`agents`](https://github.com/ndpvh/predped/reference/agents.md),
`initialize-state`,
[`iteration`](https://github.com/ndpvh/predped/reference/iteration.md),
[`potential_agents`](https://github.com/ndpvh/predped/reference/potential_agents.md),
[`setting`](https://github.com/ndpvh/predped/reference/setting.md)
