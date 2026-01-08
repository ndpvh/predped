# Add a group of agents to the simulation

When multiple agents are added to the simulation as a group, these
agents might all be different (either in archetype or in parameter
values), but will share the same goals.

## Usage

``` r
add_group(
  model,
  agent_number = 1,
  standing_start = 0.1,
  individual_differences = FALSE,
  ...
)
```

## Arguments

- model:

  Object of the
  [`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md).

- agent_number:

  Numeric denoting the number of agents to add. Defaults to `1`.

- standing_start:

  Numeric denoting the factor of their preferred speed that agents move
  when they just came from standing still. Defaults to `0.1`.

- individual_differences:

  Logical denoting whether to use the standard deviations in the
  parameter list to create some variation in the parameters. Defaults to
  `FALSE`.

- ...:

  Additional arguments passed on to
  [`add_agent`](https://github.com/ndpvh/predped/reference/add_agent.md).

## Value

List of instances of the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`add_agent`](https://github.com/ndpvh/predped/reference/add_agent.md),
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

# Generate a group of 5 pedestrians who have 5 goals to accomplish
agents <- add_group(my_model,
                    agent_number = 5,
                    goal_number = 5)

# Get id's, parameters, and number of goals
sapply(agents, id)
#>   shlcp   grzau   zbiyk   apwok   tayyj 
#> "shlcp" "grzau" "zbiyk" "apwok" "tayyj" 
sapply(agents, parameters)
#>                       [,1]  [,2]  [,3]  [,4]  [,5] 
#> radius                0.25  0.25  0.25  0.25  0.25 
#> slowing_time          1     1     1     1     1    
#> preferred_speed       1.25  1.25  1.25  1.25  1.25 
#> randomness            0.1   0.1   0.1   0.1   0.1  
#> stop_utility          10000 10000 10000 10000 10000
#> reroute               10    10    10    10    10   
#> b_turning             0.2   0.2   0.2   0.2   0.2  
#> a_turning             2     2     2     2     2    
#> b_current_direction   1     1     1     1     1    
#> a_current_direction   2     2     2     2     2    
#> blr_current_direction 10    10    10    10    10   
#> b_goal_direction      10    10    10    10    10   
#> a_goal_direction      2     2     2     2     2    
#> b_blocked             4     4     4     4     4    
#> a_blocked             2     2     2     2     2    
#> b_interpersonal       2     2     2     2     2    
#> a_interpersonal       2     2     2     2     2    
#> d_interpersonal       0     0     0     0     0    
#> b_preferred_speed     2     2     2     2     2    
#> a_preferred_speed     2     2     2     2     2    
#> b_leader              0     0     0     0     0    
#> a_leader              0     0     0     0     0    
#> d_leader              0     0     0     0     0    
#> b_buddy               0     0     0     0     0    
#> a_buddy               0     0     0     0     0    
#> a_group_centroid      0     0     0     0     0    
#> b_group_centroid      0     0     0     0     0    
#> b_visual_field        0     0     0     0     0    
#> central               0     0     0     0     0    
#> non_central           0     0     0     0     0    
#> acceleration          0     0     0     0     0    
#> constant_speed        0     0     0     0     0    
#> deceleration          0     0     0     0     0    
sapply(agents, \(x) length(goals(x)))
#> [1] 4 4 4 4 4
```
