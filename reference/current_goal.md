# Getter/Setter for the `current_goal`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
current_goal(object)

current_goal(object) <- value

# S4 method for class 'agent'
current_goal(object)

# S4 method for class 'agent'
current_goal(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `current_goal`
  slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  current_goal = goal(position = c(1, 0)))

# Access the current_goal slot for the agent
current_goal(my_agent)
#> Goal Attributes 
#> busy: FALSE 
#> counter: 5 
#> done: FALSE 
#> id: goal zwshw 
#> path:
#>      [,1] [,2]
#> 
#> position: 1 0 
#> 
#> For more detailed information, please extract the wanted information from the background directly.

# Change the current_goal slot for the agent
current_goal(my_agent) <- goal(position = c(-1, 0))
current_goal(my_agent)
#> Goal Attributes 
#> busy: FALSE 
#> counter: 5 
#> done: FALSE 
#> id: goal bqqij 
#> path:
#>      [,1] [,2]
#> 
#> position: -1 0 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
```
