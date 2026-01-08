# Constructor for the [`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md)

Constructor for the
[`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md)

## Usage

``` r
# S4 method for class 'coordinate'
initialize(.Object, ...)
```

## Arguments

- .Object:

  Numerical vector of size 2 denoting the coordinate.

- ...:

  Additional arguments specified in the other objects.

## Value

Object of the
[`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md)

## See also

[`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md),
[`rotate`](https://github.com/ndpvh/predped/reference/rotate.md)

## Examples

``` r
# Initialize a coordinate
my_coordinate <- coordinate(c(1, 1))

# Access the numeric vector again
my_coordinate@.Data
#> [1] 1 1
```
