# Getter/Setter for the `speed`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
speed(object)

speed(object) <- value

# S4 method for class 'agent'
speed(object)

# S4 method for class 'agent'
speed(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `speed` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  speed = 1)

# Access the speed slot for the agent
speed(my_agent)
#> yfqbw 
#>     1 

# Change the speed slot for the agent
speed(my_agent) <- 2
speed(my_agent)
#> yfqbw 
#>     2 
```
