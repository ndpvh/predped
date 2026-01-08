# Getter/Setter for the `variables`-slot

Works for the
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

## Usage

``` r
variables(object)

variables(object) <- value

# S4 method for class 'state'
variables(object)

# S4 method for class 'state'
variables(object) <- value
```

## Arguments

- object:

  An instance of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

- value:

  Value with which to replace the original value of the `variables`
  slot.

## See also

[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)

## Examples

``` r
# Initialize state
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(circle(center = c(0, 0), 
                                                  radius = 0.5))) 

my_state <- state(iteration = 0, 
                  setting = my_background)

# Access variables slot
variables(my_state)
#> list()

# Change the variables slot
variables(my_state) <- list("my_variable" = 10)
variables(my_state)
#> $my_variable
#> [1] 10
#> 
```
