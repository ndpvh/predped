# Getter/Setter for the `iteration_variables`-slot

Works for the
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

## Usage

``` r
iteration_variables(object)

iteration_variables(object) <- value

# S4 method for class 'state'
iteration_variables(object)
```

## Arguments

- object:

  An instance of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

- value:

  Value with which to replace the original value of the
  `iteration_variables` slot.

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

# Access iteration_variables slot
iteration_variables(my_state)
#> data frame with 0 columns and 0 rows

# Change the iteration_variables slot
iteration_variables(my_state) <- data.frame(x = rep(1,3))
iteration_variables(my_state)
#>   x
#> 1 1
#> 2 1
#> 3 1
```
