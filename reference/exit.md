# Getter/Setter for the `exit`-slot

Works for
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

## Usage

``` r
exit(object)

exit(object) <- value

# S4 method for class 'background'
exit(object)

# S4 method for class 'background'
exit(object) <- value
```

## Arguments

- object:

  An instance of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- value:

  Value with which to replace the original value of the `exit` slot.

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)

## Examples

``` r
# Initialize background
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(circle(center = c(0, 0), 
                                                  radius = 0.5)),
                            limited_access = list(segment(from = c(-1, -1), 
                                                          to = c(1, 1))), 
                            entrance = c(-1, 0), 
                            exit = c(1, 0)) 

# Access the exit slot for the background
exit(my_background)
#>      [,1] [,2]
#> [1,]    1    0

# Change the exit slot for the background
exit(my_background) <- c(-1, 0)
exit(my_background)
#>      [,1] [,2]
#> [1,]   -1    0
```
