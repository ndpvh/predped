# Getter/Setter for the `agents`-slot

Works for the
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

## Usage

``` r
agents(object)

agents(object) <- value

# S4 method for class 'state'
agents(object)

# S4 method for class 'state'
agents(object) <- value
```

## Arguments

- object:

  An instance of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

- value:

  Value with which to replace the original value of the `agents` slot.

## See also

[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)

## Examples

``` r
# Initialize state
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(circle(center = c(0, 0), 
                                                  radius = 0.5))) 

my_state <- state(iteration = 0, 
                  setting = my_background,
                  agents = list(agent(center = c(0, 0), radius = 0.25)))

# Access agents slot
agents(my_state)
#> [[1]]
#> Agent Attributes 
#> center: 0 0 
#> cell: 0 
#> color: black 
#> current_goal: (a) position: 0 0 (b) path:  
#> goals (number): 0 
#> group: 0 
#> id: ymbwm 
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

# Change the agents slot
agents(my_state) <- list(agent(center = c(1, 1), radius = 0.25))
agents(my_state)
#> [[1]]
#> Agent Attributes 
#> center: 1 1 
#> cell: 0 
#> color: black 
#> current_goal: (a) position: 0 0 (b) path:  
#> goals (number): 0 
#> group: 0 
#> id: aipjj 
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
