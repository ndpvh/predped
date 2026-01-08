# Getter/Setter for the `iteration`-slot

Works for the
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

## Usage

``` r
iteration(object)

iteration(object) <- value

# S4 method for class 'state'
iteration(object)

# S4 method for class 'state'
iteration(object) <- value

# S4 method for class 'state'
iteration_variables(object) <- value
```

## Arguments

- object:

  An instance of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

- value:

  Value with which to replace the original value of the `iteration`
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

# Access iteration slot
iteration(my_state)
#> [1] 0

# Change the iteration slot
iteration(my_state) <- 1
iteration(my_state)
#> [1] 1
```
