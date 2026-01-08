# Make an Object Larger

Returns an object that is larger than the originally provided one. Used
under the hood for deleting nodes or potential path points that come too
close to the objects in the environment. Works for all objects of the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
with exception of
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## Usage

``` r
enlarge(object, extension, ...)

# S4 method for class 'polygon'
enlarge(object, extension, cpp = TRUE)

# S4 method for class 'rectangle'
enlarge(object, extension, ...)

# S4 method for class 'circle'
enlarge(object, extension, ...)
```

## Arguments

- object:

  An object of a type that extends
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
  with exception of the Works for all objects of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

- extension:

  Numeric denoting the length with which to extend the object in all
  directions.

- ...:

  Arguments passed on to the methods for the generic.

- cpp:

  Logical denoting whether to use the R or Rcpp version of the function.
  Defaults to `TRUE`.

## Value

Object of the same class as the original, but with a larger size.

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
`size<-`

## Examples

``` r
# Create an object
my_circle <- circle(center = c(0, 0), radius = 1)
my_circle@radius
#> [1] 1

# Increase the size of the object
larger_circle <- enlarge(my_circle, extension = 1)
larger_circle@radius
#> [1] 2

# Decrease the size of the object
smaller_circle <- enlarge(my_circle, extension = -0.5)
smaller_circle@radius
#> [1] 0.5
```
