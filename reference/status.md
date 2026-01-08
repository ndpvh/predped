# Getter/Setter for the `status`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
status(object)

status(object) <- value

# S4 method for class 'agent'
status(object)

# S4 method for class 'agent'
status(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `status` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  status = "move")

# Access the status slot for the agent
status(my_agent)
#>  zeahr 
#> "move" 

# Change the status slot for the agent
status(my_agent) <- "reroute"
status(my_agent)
#>     zeahr 
#> "reroute" 
```
