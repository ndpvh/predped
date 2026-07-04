# Getter/Setter for the `group_representative`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
group_representative(object)

group_representative(object) <- value

# S4 method for class 'agent'
group_representative(object)

# S4 method for class 'agent'
group_representative(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the
  `group_representative` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),

## Examples

``` r
# Initialize agent
my_agent <- agent(center = c(0, 0), 
                  radius = 0.25, 
                  group_representative = TRUE)

# Access the group_representative slot for the agent
group_representative(my_agent)
#> zqgrw 
#>  TRUE 

# Change the group_representative slot for the agent
group_representative(my_agent) <- FALSE
group_representative(my_agent)
#> zqgrw 
#> FALSE 
```
