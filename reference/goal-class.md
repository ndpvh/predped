# An S4 Class to Represent Goals

Defines the `goal` class, which contains all characteristics of a goal
that the agents can pursue.

## Slots

- `id`:

  Character that serves as an identifier for the goal.

- `position`:

  Numerical vector denoting the position of the goal.

- `path`:

  Numerical matrix of size n x 2 denoting the different intermediate
  path points between an agent and a goal. In effect defines which path
  the agent will take to move towards the goal.

- `busy`:

  Logical denoting whether the goal is currently being interacted with.

- `done`:

  Logical denoting whether a goal has been completed.

- `counter`:

  Numeric denoting the number of time steps the agent should interact
  with the goal before the goal has been completed.

## See also

[`busy`](https://github.com/ndpvh/predped/reference/busy.md),
[`counter`](https://github.com/ndpvh/predped/reference/counter.md),
[`done`](https://github.com/ndpvh/predped/reference/done.md),
[`id`](https://github.com/ndpvh/predped/reference/id.md),
[`path`](https://github.com/ndpvh/predped/reference/path.md),
[`position`](https://github.com/ndpvh/predped/reference/position.md),
`initialize-goal`
