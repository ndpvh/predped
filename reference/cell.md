# Getter/Setter for the `cell`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
cell(object)

cell(object) <- value

# S4 method for class 'agent'
cell(object)

# S4 method for class 'agent'
cell(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `cell` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  cell = 1)

# Access the cell slot for the agent
cell(my_agent)
#> ilmne 
#>     1 

# Change the cell slot for the agent
cell(my_agent) <- 2
cell(my_agent)
#> ilmne 
#>     2 
```
