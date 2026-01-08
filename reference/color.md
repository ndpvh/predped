# Getter/Setter for the `color`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
color(object)

color(object) <- value

# S4 method for class 'agent'
color(object)

# S4 method for class 'agent'
color(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `color` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  color = "black")

# Access the color slot for the agent
color(my_agent)
#>   csahe 
#> "black" 

# Change the color slot for the agent
color(my_agent) <- "blue"
color(my_agent)
#>  csahe 
#> "blue" 
```
