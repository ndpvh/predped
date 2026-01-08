# Getter/Setter for the `goals`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
goals(object)

goals(object) <- value

# S4 method for class 'agent'
goals(object)

# S4 method for class 'agent'
goals(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `goals` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  goals = list(goal(position = c(1, 0))))

# Access the goals slot for the agent
goals(my_agent)
#> [[1]]
#> Goal Attributes 
#> busy: FALSE 
#> counter: 5 
#> done: FALSE 
#> id: goal pefxw 
#> path:
#>      [,1] [,2]
#> 
#> position: 1 0 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
#> 

# Change the goals slot for the agent
goals(my_agent) <- list(goal(position = c(-1, 0)))
goals(my_agent)
#> [[1]]
#> Goal Attributes 
#> busy: FALSE 
#> counter: 5 
#> done: FALSE 
#> id: goal nvjxb 
#> path:
#>      [,1] [,2]
#> 
#> position: -1 0 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
#> 
```
