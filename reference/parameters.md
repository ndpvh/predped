# Getter/Setter for the `parameters`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
parameters(object)

parameters(object) <- value

# S4 method for class 'agent'
parameters(object)

# S4 method for class 'agent'
parameters(object) <- value

# S4 method for class 'predped'
parameters(object)

# S4 method for class 'predped'
parameters(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)
  or
  [`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md).

- value:

  Value with which to replace the original value of the `parameters`
  slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  parameters = generate_parameters(1))

# Access the parameters slot for the agent
parameters(my_agent)
#>           radius slowing_time preferred_speed randomness stop_utility  reroute
#> radius 0.2463262    0.9494592        1.263288  0.1623876     10192.85 8.657848
#>        b_turning a_turning b_current_direction a_current_direction
#> radius 0.1810312         2           0.9485869                   2
#>        blr_current_direction b_goal_direction a_goal_direction b_blocked
#> radius                    10         8.505427                2  4.412557
#>        a_blocked b_interpersonal a_interpersonal d_interpersonal
#> radius         2        1.926026               2               0
#>        b_preferred_speed a_preferred_speed b_leader a_leader d_leader b_buddy
#> radius          1.400041                 2        0        0        0       0
#>        a_buddy a_group_centroid b_group_centroid b_visual_field central
#> radius       0                0                0              0       0
#>        non_central acceleration constant_speed deceleration
#> radius           0            0              0            0

# Change the parameters slot for the agent
parameters(my_agent) <- generate_parameters(1)
parameters(my_agent)
#>           radius slowing_time preferred_speed randomness stop_utility  reroute
#> radius 0.2538798    0.9946231        1.181065  0.1166939     10401.98 10.24248
#>        b_turning a_turning b_current_direction a_current_direction
#> radius 0.1891056         2           0.9392615                   2
#>        blr_current_direction b_goal_direction a_goal_direction b_blocked
#> radius                    10         11.93149                2  4.750318
#>        a_blocked b_interpersonal a_interpersonal d_interpersonal
#> radius         2        1.955604               2               0
#>        b_preferred_speed a_preferred_speed b_leader a_leader d_leader b_buddy
#> radius          2.585462                 2        0        0        0       0
#>        a_buddy a_group_centroid b_group_centroid b_visual_field central
#> radius       0                0                0              0       0
#>        non_central acceleration constant_speed deceleration
#> radius           0            0              0            0
```
