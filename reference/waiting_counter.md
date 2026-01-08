# Getter/Setter for the `waiting_counter`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
waiting_counter(object)

waiting_counter(object) <- value

# S4 method for class 'agent'
waiting_counter(object)

# S4 method for class 'agent'
waiting_counter(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the
  `waiting_counter` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  waiting_counter = 0)

# Access the waiting_counter slot for the agent
waiting_counter(my_agent)
#> wnrkr 
#>     0 

# Change the waiting_counter slot for the agent
waiting_counter(my_agent) <- 5
waiting_counter(my_agent)
#> wnrkr 
#>     5 
```
