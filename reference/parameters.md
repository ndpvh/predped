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
#> radius 0.2489568     1.119115        1.206566  0.0959197      9765.43 9.677835
#>        b_turning a_turning b_current_direction a_current_direction
#> radius 0.2317959         2            0.972154                   2
#>        blr_current_direction b_goal_direction a_goal_direction b_blocked
#> radius                    10         9.419334                2   3.57686
#>        a_blocked b_interpersonal a_interpersonal d_interpersonal
#> radius         2        3.079064               2               0
#>        b_preferred_speed a_preferred_speed b_leader a_leader d_leader b_buddy
#> radius          2.580374                 2        0        0        0       0
#>        a_buddy a_group_centroid b_group_centroid b_visual_field central
#> radius       0                0                0              0       0
#>        non_central acceleration constant_speed deceleration
#> radius           0            0              0            0

# Change the parameters slot for the agent
parameters(my_agent) <- generate_parameters(1)
parameters(my_agent)
#>           radius slowing_time preferred_speed randomness stop_utility  reroute
#> radius 0.2360799    0.9427001         1.28486 0.08026931     9921.571 9.522834
#>        b_turning a_turning b_current_direction a_current_direction
#> radius  0.227726         2            1.353963                   2
#>        blr_current_direction b_goal_direction a_goal_direction b_blocked
#> radius                    10         10.50696                2  5.283771
#>        a_blocked b_interpersonal a_interpersonal d_interpersonal
#> radius         2        2.029752               2               0
#>        b_preferred_speed a_preferred_speed b_leader a_leader d_leader b_buddy
#> radius          1.968734                 2        0        0        0       0
#>        a_buddy a_group_centroid b_group_centroid b_visual_field central
#> radius       0                0                0              0       0
#>        non_central acceleration constant_speed deceleration
#> radius           0            0              0            0
```
