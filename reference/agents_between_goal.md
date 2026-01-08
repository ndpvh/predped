# Find number of agents blocking agent

This function computes the number of other agents in a particular state
who are blocking the way between the current agent and their goal. This
is done so that the current agent can reevaluate whether they want to
keep pursuing this way to their goal, or whether they want to reroute.

## Usage

``` r
agents_between_goal(agent, state, agent_predictions = NULL)
```

## Arguments

- agent:

  Object of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- state:

  Object of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

- agent_predictions:

  Matrix containing the predicted positions of all agents in the
  simulation, where the id's of the agents serve as the rownames. Is
  typically handled under the hood. Defaults to `NULL`, triggering this
  function to consider where the other agents are right now (instead of
  where they will be in the next iteration).

## Value

List containing all of the agents that are blocking the path

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md),
[`moving_options`](https://github.com/ndpvh/predped/reference/moving_options.md)

## Examples

``` r
# Create an agent and a state
my_background <- background(shape = rectangle(center = c(0, 0),
                                              size = c(6, 6)),
                            objects = list(circle(center = c(0, 0),
                                                  radius = 1)))
my_agent <- agent(center = c(-2.75, 0),
                  radius = 0.25,
                  speed = 1,
                  orientation = 0,
                  current_goal = goal(position = c(-1.01, 0),
                                      path = matrix(c(-1.01, 0), nrow = 1)))

my_state <- state(iteration = 1,
                  setting = my_background,
                  agents = list(agent(center = c(-2, 0), radius = 0.25),
                                agent(center = c(-1.5, 0), radius = 0.25),
                                agent(center = c(2, 0), radius = 0.25),
                                agent(center = c(1.5, 0), radius = 0.25)))

# Find out how many agents are blocking the way for my_agent
agents_between_goal(my_agent, my_state)
#> [[1]]
#> Agent Attributes 
#> center: -1.5 0 
#> cell: 0 
#> color: black 
#> current_goal: (a) position: 0 0 (b) path:  
#> goals (number): 0 
#> group: 0 
#> id: vqeoe 
#> orientation: 0 
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
#> speed: 0.1 
#> status: move 
#> 
#> For more detailed information, please extract the wanted information from the agent directly.
#> 
#> [[2]]
#> Agent Attributes 
#> center: 2 0 
#> cell: 0 
#> color: black 
#> current_goal: (a) position: 0 0 (b) path:  
#> goals (number): 0 
#> group: 0 
#> id: aitrx 
#> orientation: 0 
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
#> speed: 0.1 
#> status: move 
#> 
#> For more detailed information, please extract the wanted information from the agent directly.
#> 
#> [[3]]
#> Agent Attributes 
#> center: 1.5 0 
#> cell: 0 
#> color: black 
#> current_goal: (a) position: 0 0 (b) path:  
#> goals (number): 0 
#> group: 0 
#> id: gfuji 
#> orientation: 0 
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
#> speed: 0.1 
#> status: move 
#> 
#> For more detailed information, please extract the wanted information from the agent directly.
#> 
```
