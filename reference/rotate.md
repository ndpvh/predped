# Rotate object around center

Retrieve an object that is rotated around its center with `degrees` or
`radians`. Currently works for
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
and
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)
We additionally provide this method for R-native numeric vectors and
matrices as well.

## Usage

``` r
rotate(object, ...)

# S4 method for class 'numeric'
rotate(object, radians = 0, degrees = NULL, center = c(0, 0))

# S4 method for class 'matrix'
rotate(object, radians = 0, degrees = NULL, center = c(0, 0))

# S4 method for class 'polygon'
rotate(object, center = object@center, ...)

# S4 method for class 'rectangle'
rotate(object, radians = 0, degrees = NULL, center = object@center)

# S4 method for class 'circle'
rotate(object, center = object@center, ...)

# S4 method for class 'segment'
rotate(object, center = object@center, ...)
```

## Arguments

- object:

  Object of
  [`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
  [`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md),
  [`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
  [`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
  or, alternatively, a numeric vector or matrix containing coordinates.

- ...:

  Arguments passed on to the methods of this generic

- radians:

  Numeric denoting the degrees with which to rotate the object in
  radians. Defaults to `0`.

- degrees:

  Numeric denoting the degrees with which to rotate the object. Defaults
  to `NULL`, triggering the use of `radians` instead. Whenever `degrees`
  is not `NULL`, `radians` will be ignored.

- center:

  Numeric vector denoting the x and y coordinates around which to rotate
  the object. Defaults differ for each object. For instances of the
  [`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md),
  numerics, and matrices, this defaults to the origin (0, 0). For the
  other objects defined under
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
  this defaults to their own centers.

## Value

Object of the same class as the one provided

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## Examples

``` r
# Let's create a numeric vector and degrees of rotation
x <- c(1, 1)

# Let's rotate this numeric 90 degrees around the origin
rotate(x, degrees = 90, center = c(0, 0))
#> An object of class "coordinate"
#> [1] -1  1

# Let's create a numeric matrix and degrees of rotation
x <- cbind(c(1, 1, -1, -1), 
           c(1, -1, -1, 1))
x
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1   -1
#> [3,]   -1   -1
#> [4,]   -1    1

# Let's rotate this matrix 90 degrees around the origin
rotate(x, degrees = 90, center = c(0, 0))
#>      [,1] [,2]
#> [1,]   -1    1
#> [2,]    1    1
#> [3,]    1   -1
#> [4,]   -1   -1
```
