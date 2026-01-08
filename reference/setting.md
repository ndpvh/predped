# Getter/Setter for the `setting`-slot

Works for the
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

## Usage

``` r
setting(object)

setting(object) <- value

# S4 method for class 'state'
setting(object)

# S4 method for class 'state'
setting(object) <- value

# S4 method for class 'predped'
setting(object)

# S4 method for class 'predped'
setting(object) <- value
```

## Arguments

- object:

  An instance of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

- value:

  Value with which to replace the original value of the `setting` slot.

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

# Access setting slot
setting(my_state)
#> Background Attributes 
#> entrance: 
#>      [,1] [,2]
#> [1,]   -1    0
#> 
#> exit: -1 0 
#>      [,1] [,2]
#> [1,]   -1    0
#> 
#> limited_access (number): 0 
#> objects (number): 1 
#> shape: (a) position: 0 0 (b) size: 2 2 
#> 
#> For more detailed information, please extract the wanted information from the background directly.

# Change the setting slot
other_background <- background(shape = rectangle(center = c(0, 0), 
                                                 size = c(2, 2)), 
                               objects = list(circle(center = c(0, 0), 
                                                     radius = 1))) 

setting(my_state) <- other_background
setting(my_state)
#> Background Attributes 
#> entrance: 
#>      [,1] [,2]
#> [1,]    0   -1
#> 
#> exit: 0 -1 
#>      [,1] [,2]
#> [1,]    0   -1
#> 
#> limited_access (number): 0 
#> objects (number): 1 
#> shape: (a) position: 0 0 (b) size: 2 2 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
```
