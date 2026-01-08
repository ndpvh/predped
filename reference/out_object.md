# Check Whether a Point Lies Outside of an Object

Returns the opposite of the
[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md).
Currently works for all classes inside of the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

## Usage

``` r
out_object(object, x, ...)

# S4 method for class 'object'
out_object(object, x, ...)
```

## Arguments

- object:

  Object of the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

- x:

  Numeric vector or matrix containing x- and y-coordinates to be
  checked.

- ...:

  Arguments passed on to
  [`in_object`](https://github.com/ndpvh/predped/reference/in_object.md)

## Value

Logical whether the point is outside of the object (`TRUE`) or inside of
the object (`FALSE`).

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md),
[`raycasting`](https://github.com/ndpvh/predped/reference/raycasting.md)

## Examples

``` r
# Let's create an object
my_circle <- circle(center = c(0, 0), radius = 1)

# Let's create a matrix of different coordinates of which the first is 
# inside of the object, the second on its circumference, and the third  
# outside of the object
coords <- rbind(c(0, 0), 
                c(1, 0), 
                c(2, 0))

# Let's do the test
out_object(my_circle, coords)
#> [1] FALSE  TRUE  TRUE
```
