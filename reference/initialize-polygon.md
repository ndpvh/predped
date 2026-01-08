# Constructor for the [`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md)

Constructor for the
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md)

## Usage

``` r
# S4 method for class 'polygon'
initialize(.Object, points, clock_wise = TRUE, forbidden = numeric(0), ...)
```

## Arguments

- .Object:

  For this class, should be left unspecified (see Example).

- points:

  Numerical matrix containing the coordinates that make up the polygon.

- clock_wise:

  Logical denoting whether the coordinates in `points` are defined in
  clockwise (`TRUE`) or counter-clockwise fashion (`FALSE`). Defaults to
  `TRUE`.

- forbidden:

  Numerical vector containing the indices of those edges that cannot be
  used to generate goals on. Note that edges are created based on the
  [`points`](https://rdrr.io/r/graphics/points.html) of the object.
  Defaults to an empty vector, making all edges worthy of goal
  generation.

- ...:

  Additional arguments passed to `initialize-object`.

## Value

Object of the
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md)

## See also

[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md)
`initialize-object`

## Examples

``` r
# Initialize a polygon
my_polygon <- polygon(id = "my polygon", 
                      points = cbind(c(1, 1, -1, -1), 
                                     c(1, -1, -1, 1)), 
                      clock_wise = TRUE)

# Access the slots that you specified
my_polygon@points 
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1   -1
#> [3,]   -1   -1
#> [4,]   -1    1
my_polygon@id
#> [1] "my polygon"
```
