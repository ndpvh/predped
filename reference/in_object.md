# Check Whether a Point Lies Within an Object

Currently works for all classes inside of the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

## Usage

``` r
in_object(object, x, ...)

# S4 method for class 'polygon'
in_object(object, x, cpp = TRUE)

# S4 method for class 'rectangle'
in_object(object, x, cpp = TRUE)

# S4 method for class 'circle'
in_object(object, x, cpp = TRUE)

# S4 method for class 'segment'
in_object(object, x, ...)
```

## Arguments

- object:

  Object of the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

- x:

  Numeric vector or matrix containing x- and y-coordinates to be
  checked.

- ...:

  Arguments passed on to the methods of this generic

- cpp:

  Logical denoting whether to use the Rcpp alternative (`TRUE`) or the R
  alternative of this function (`FALSE`). Defaults to `FALSE`.

## Value

Logical whether the point is inside of the object (`TRUE`) or outside of
the object (`FALSE`).

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
[`out_object`](https://github.com/ndpvh/predped/reference/out_object.md),
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
in_object(my_circle, coords)
#> [1]  TRUE FALSE FALSE
```
