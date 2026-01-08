# Getter/Setter for the `utility_variables`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
utility_variables(object)

utility_variables(object) <- value

# S4 method for class 'agent'
utility_variables(object)

# S4 method for class 'agent'
utility_variables(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the
  `utility_variables` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  utility_variables = data.frame())

# Access the utility_variables slot for the agent
utility_variables(my_agent)
#>   agent_idx check ps_speed ps_distance gd_angle id_distance id_check id_ingroup
#> 1        NA    NA       NA          NA       NA          NA       NA         NA
#>   ba_angle ba_cones fl_leaders wb_buddies gc_distance gc_radius gc_nped
#> 1       NA       NA         NA         NA          NA        NA      NA
#>   vf_angles
#> 1        NA

# Change the utility_variables slot for the agent
utility_variables(my_agent) <- data.frame(value = 1)
utility_variables(my_agent)
#>   value
#> 1     1
```
