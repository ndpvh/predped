# Create initial condition

Creates a list of agents that can be used as an initial condition to the
simulation. Agents are placed at random locations in the room. Main
advantage of this function is that you don't have to wait for a room to
fill up with the required number of agents, but rather impose that the
room is already filled with this number of agents.

## Usage

``` r
create_initial_condition(
  agent_number,
  model,
  goal_number = function(n) rnorm(n, 10, 2),
  group_size = matrix(1, nrow = 1, ncol = 2),
  space_between = 1.25,
  precomputed_edges = NULL,
  cpp = TRUE,
  ...
)
```

## Arguments

- agent_number:

  Numeric denoting the number of agents that should be included in the
  initial condition.

- model:

  Object of the
  [`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md).

- goal_number:

  Numeric, vector, or function that defines the number of goals the
  agents should accomplish. It's exact value is handled by
  [`determine_values`](https://github.com/ndpvh/predped/reference/determine_values.md).
  Defaults to `\(n) rnorm(n, 10, 2)`.

- group_size:

  Matrix of size n x 2 containing the group sizes you would like to
  simulate (first column) and the probability of observing groups with
  this size (second column). Defaults to a 100 agents.

- space_between:

  Numeric denoting the space that should be left between an object and
  the created path points for the agents (in radii). Defaults to `2.5`,
  meaning a space of `2.5 * radius(agent)` is left between an object and
  the path points agents use in their strategy. Ignored if
  `precomputed_edges` is provided.

- precomputed_edges:

  Output of
  [`compute_edges`](https://github.com/ndpvh/predped/reference/compute_edges.md)
  containing the nodes and edges the agent can use to plan its path.
  Defauls to `NULL`, triggering the creation of these edges whenever
  they are needed.

- cpp:

  Logical denoting whether to use the Rcpp (`TRUE`) or R (`FALSE`)
  alternatives for the lower-level functions. Defaults to `TRUE`.

- ...:

  Additional arguments provided to
  [`add_agent`](https://github.com/ndpvh/predped/reference/add_agent.md).

## Value

List of instances of the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Details

In this function, we use the following approach to simulate an initial
condition. First, we create an agent using the
[`add_agent`](https://github.com/ndpvh/predped/reference/add_agent.md)
function. This agent is given a specific group id based on the
`group_size` argument.

Once created, we then compute all possible paths that the agent may be
walking on through the
[`compute_edges`](https://github.com/ndpvh/predped/reference/compute_edges.md)
function. One of these paths is then chosen, and subsequently a random
point on that path is chosen. We then check whether an agent would be
able to take the chosen position without intersecting any of the objects
in the environment. If not, then we choose another point until we can
place the agent there. If we do not find a fitting position for the
agent after 10 iterations, we break out of the loop and stop trying to
fit in `agent_number` agents in the space.

Finally, group members are identified and given the same set of goals.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md),
[`add_agent`](https://github.com/ndpvh/predped/reference/add_agent.md),
[`simulate`](https://rdrr.io/r/stats/simulate.html),
[`simulate`](https://rdrr.io/r/stats/simulate.html)
