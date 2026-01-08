# Getter/Setter for the `center`-slot

Is a more specific version of
[`position`](https://github.com/ndpvh/predped/reference/position.md)
that works for all extensions of the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

## Usage

``` r
center(object)

center(object) <- value

# S4 method for class 'polygon'
center(object)

# S4 method for class 'polygon'
center(object) <- value

# S4 method for class 'rectangle'
center(object)

# S4 method for class 'rectangle'
center(object) <- value

# S4 method for class 'circle'
center(object)

# S4 method for class 'circle'
center(object) <- value

# S4 method for class 'segment'
center(object)

# S4 method for class 'segment'
center(object) <- value
```

## Arguments

- object:

  An instance of the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  or the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `center` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## Examples

``` r
# Initialize a circle
my_circle <- circle(center = c(0, 0), 
                    radius = 1)

# Access the center slot for the circle
center(my_circle)
#> An object of class "coordinate"
#> [1] 0 0

# Change the center slot for the circle
center(my_circle) <- c(1, 1)
center(my_circle)
#> An object of class "coordinate"
#> [1] 1 1

# Note that for some object, changing the center also changes other slots
my_rectangle <- rectangle(center = c(0, 0), 
                          size = c(2, 2))

points(my_rectangle)
#>      [,1] [,2]
#> [1,]   -1   -1
#> [2,]   -1    1
#> [3,]    1    1
#> [4,]    1   -1

center(my_rectangle) <- c(1, 1)
points(my_rectangle)
#>      [,1] [,2]
#> [1,]    0    0
#> [2,]    0    2
#> [3,]    2    2
#> [4,]    2    0
```
