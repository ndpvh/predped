# Getter/Setter for the `group`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
group(object)

group(object) <- value

# S4 method for class 'agent'
group(object)

# S4 method for class 'agent'
group(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `group` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  group = 1)

# Access the speed slot for the agent
group(my_agent)
#> cunkn 
#>     1 

# Change the speed slot for the agent
group(my_agent) <- 2
group(my_agent)
#> cunkn 
#>     2 
```
