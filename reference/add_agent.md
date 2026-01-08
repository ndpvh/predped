# Add a single agent to the simulation

Add a single agent to the simulation

## Usage

``` r
add_agent(
  model,
  group_number = 1,
  goal_number = 5,
  goal_duration = function(x) rnorm(x, 10, 2),
  precompute_goal_paths = TRUE,
  sort_goals = TRUE,
  precomputed_goals = NULL,
  middle_edge = FALSE,
  precomputed_edges = NULL,
  many_nodes = !is.null(precomputed_edges),
  space_between = 1.25,
  position = NULL,
  standing_start = 0.1,
  individual_differences = FALSE,
  return_characteristics = FALSE
)
```

## Arguments

- model:

  Object of the
  [`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md).

- group_number:

  Numeric denoting the group to which the agent belongs. Defaults to
  `1`.

- goal_number:

  Numeric, vector, or function that defines the number of goals the
  agents should accomplish. It's exact value is handled by
  [`determine_values`](https://github.com/ndpvh/predped/reference/determine_values.md).
  Defaults to `\(n) rnorm(n, 10, 2)`.

- goal_duration:

  Numeric, vector, or function that defines the duration of the goals of
  the agents. Defaults to `\(n) rnorm(n, 10, 2)`.

- precompute_goal_paths:

  Logical denoting whether to run the
  [`find_path`](https://github.com/ndpvh/predped/reference/find_path.md)
  for each of the generated goals beforehand. Assumes that the agent
  does all of the goals in the order of the goal stack. Defaults to
  `FALSE`.

- sort_goals:

  Logical denoting whether to order the goal stack in a logical way.
  Currently implemented in the following way. First, we select the first
  goal as being the one that is closest by the starting position
  provided in the argument `starting_position`. Then, we define each of
  the next goals as being the one that is closest to the position of the
  previous goal. Defaults to `TRUE`.

- precomputed_goals:

  List of goal stacks from which the agent can be assigned one. Defaults
  to `NULL`, triggering the creation of goal stacks in the simulation.

- middle_edge:

  Logical denoting whether to sample the goals from the middle of the
  edge of the objects in the `link[predped]{background-class}` (`TRUE`)
  or to allow the goal locations to fall on all points on these edges
  (`FALSE`). Defaults to `FALSE`.

- precomputed_edges:

  Output of
  [`compute_edges`](https://github.com/ndpvh/predped/reference/compute_edges.md)
  containing the nodes and edges the agent can use to plan its path.
  Defauls to `NULL`, triggering the creation of these edges whenever
  they are needed.

- many_nodes:

  Logical denoting whether to use many nodes when computing the edges.
  Defaults to `FALSE` if `precomputed_edges = FALSE`.

- space_between:

  Numeric denoting the space that should be left between an object and
  the created path points for the agents (in radii). Defaults to `2.5`,
  meaning a space of `2.5 * radius(agent)` is left between an object and
  the path points agents use in their strategy. Ignored if
  `precomputed_edges` is provided.

- position:

  Numeric denoting the position you would like to assign to the agent.
  Defaults to `NULL`, making the agent start at the entrance. Note that
  this is an experimental feature that has not been tested yet, and
  therefore might not work for the moment.

- standing_start:

  Numeric denoting the factor of their preferred speed that agents move
  when they just came from standing still. Defaults to `0.1`.

- individual_differences:

  Logical denoting whether to use the standard deviations in the
  parameter list to create some variation in the parameters. Defaults to
  `FALSE`.

- return_characteristics:

  Logical denoting whether to return the characteristics of the
  generated agents in a list (`TRUE`) or as part of an agent (`FALSE`).
  Defaults to `FALSE`.

## Value

List of instances of the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`add_group`](https://github.com/ndpvh/predped/reference/add_group.md),
[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`

## Examples

``` r
# Create a setting in which to simulate.
my_background <- background(shape = rectangle(center = c(0, 0),
                                              size = c(2, 2)),
                            objects = list(rectangle(center = c(0, 0),
                                                     size = c(1, 1))))

# Create a model from which to simulate
my_model <- predped(setting = my_background,
                    archetypes = c("BaselineEuropean",
                                   "DrunkAussie"))

# Generate an agent
my_agent <- add_agent(my_model, goal_number = 5)
my_agent
#> Agent Attributes 
#> center: 0.6925 3.765789e-17 
#> cell: 0 
#> color: cornflowerblue 
#> current_goal: (a) position: 0.5070711 0.05405127 (b) path: 0.5070711 0.05405127 
#> goals (number): 4 
#> group: 1 
#> id: aglvi 
#> orientation: 180 
#> parameters: 
#>                           [,1]
#> radius                    0.25
#> slowing_time              1.00
#> preferred_speed           1.25
#> randomness                0.10
#> stop_utility          10000.00
#> reroute                  10.00
#> b_turning                 0.20
#> a_turning                 2.00
#> b_current_direction       1.00
#> a_current_direction       2.00
#> blr_current_direction    10.00
#> b_goal_direction         10.00
#> a_goal_direction          2.00
#> b_blocked                 4.00
#> a_blocked                 2.00
#> b_interpersonal           2.00
#> a_interpersonal           2.00
#> d_interpersonal           0.00
#> b_preferred_speed         2.00
#> a_preferred_speed         2.00
#> b_leader                  0.00
#> a_leader                  0.00
#> d_leader                  0.00
#> b_buddy                   0.00
#> a_buddy                   0.00
#> a_group_centroid          0.00
#> b_group_centroid          0.00
#> b_visual_field            0.00
#> central                   0.00
#> non_central               0.00
#> acceleration              0.00
#> constant_speed            0.00
#> deceleration              0.00
#> 
#> radius: 0.25 
#> speed: 0.125 
#> status: move 
#> 
#> For more detailed information, please extract the wanted information from the agent directly.
```
