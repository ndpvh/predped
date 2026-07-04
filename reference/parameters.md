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
#> radius 0.2360685    0.9958521        1.261666 0.09512897      10029.5 10.55578
#>        b_turning a_turning b_current_direction a_current_direction
#> radius 0.1922396         2           0.9067952                   2
#>        blr_current_direction b_goal_direction a_goal_direction b_blocked
#> radius                    10         9.709835                2  4.186261
#>        a_blocked b_interpersonal a_interpersonal d_interpersonal
#> radius         2        2.408568               2               0
#>        b_preferred_speed a_preferred_speed b_leader a_leader d_leader b_buddy
#> radius          1.326547                 2        0        0        0       0
#>        a_buddy a_group_centroid b_group_centroid b_visual_field central
#> radius       0                0                0              0       0
#>        non_central acceleration constant_speed deceleration a_lgvf b_lgvf
#> radius           0            0              0            0      0      0
#>        e_lgvf
#> radius      0

# Change the parameters slot for the agent
parameters(my_agent) <- generate_parameters(1)
parameters(my_agent)
#>           radius slowing_time preferred_speed randomness stop_utility  reroute
#> radius 0.2466142    0.9485268         1.15088 0.09688066     9737.849 9.627921
#>        b_turning a_turning b_current_direction a_current_direction
#> radius 0.1613132         2            1.084877                   2
#>        blr_current_direction b_goal_direction a_goal_direction b_blocked
#> radius                    10         9.432476                2  5.625974
#>        a_blocked b_interpersonal a_interpersonal d_interpersonal
#> radius         2        2.234487               2               0
#>        b_preferred_speed a_preferred_speed b_leader a_leader d_leader b_buddy
#> radius          3.282353                 2        0        0        0       0
#>        a_buddy a_group_centroid b_group_centroid b_visual_field central
#> radius       0                0                0              0       0
#>        non_central acceleration constant_speed deceleration a_lgvf b_lgvf
#> radius           0            0              0            0      0      0
#>        e_lgvf
#> radius      0
```
