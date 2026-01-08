# Getter/Setter for the `orientation`-slot

Works for all objects that are extensions of the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
except for the
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md)
and the
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md).

## Usage

``` r
orientation(object)

orientation(object) <- value

# S4 method for class 'rectangle'
orientation(object)

# S4 method for class 'rectangle'
orientation(object) <- value

# S4 method for class 'segment'
orientation(object)

# S4 method for class 'segment'
orientation(object) <- value

# S4 method for class 'agent'
orientation(object)

# S4 method for class 'agent'
orientation(object) <- value
```

## Arguments

- object:

  An instance of the
  [`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
  or
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `orientation`
  slot.

## See also

[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## Examples

``` r
# Initialize a rectangle
my_rectangle <- rectangle(center = c(0, 0), 
                          size = c(2, 2),
                          orientation = 0)

# Access the orientation slot
orientation(my_rectangle)
#> [1] 0

# Change the orientation slot
orientation(my_rectangle) <- pi / 4
orientation(my_rectangle)
#> [1] 0.7853982

# Note that for some object, changing the orientation also changes other slots
points(my_rectangle)
#>               [,1]          [,2]
#> [1,] -1.110223e-16 -1.414214e+00
#> [2,] -1.414214e+00  1.110223e-16
#> [3,]  1.110223e-16  1.414214e+00
#> [4,]  1.414214e+00 -1.110223e-16
```
