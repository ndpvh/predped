# Constructor for the [`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)

Constructor for the
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)

## Usage

``` r
# S4 method for class 'rectangle'
initialize(.Object, center, size, orientation = 0, ...)
```

## Arguments

- .Object:

  Numerical matrix containing the coordinates that make up the polygon.
  Best left unspecified, as this numerical matrix will be derived from
  `center` and `size`.

- center:

  Numeric vector denoting the coordinates of the center or position of
  the rectangle.

- size:

  Numeric vector denoting the width and height of the rectangle.

- orientation:

  Numeric denoting the orientation of the rectangle in radians. Defauls
  to `0`.

- ...:

  Additional arguments passed to `initialize-object`. Note that if the
  `points` or `clock_wise` arguments of the `initialize-polygon` are
  provided, that they will not be used in the creation of the rectangle.

## Value

Object of the
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)

## See also

[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
`initialize-object` `initialize-polygon`

## Examples

``` r
# Initialize a rectangle
my_rectangle <- rectangle(id = "my rectangle", 
                          center = c(0, 0), 
                          size = c(2, 2))

# Access slots that are inherited from object, polygon, and rectangle
my_rectangle@size
#> [1] 2 2
my_rectangle@points 
#>      [,1] [,2]
#> [1,]   -1   -1
#> [2,]   -1    1
#> [3,]    1    1
#> [4,]    1   -1
my_rectangle@id
#> [1] "my rectangle"
```
