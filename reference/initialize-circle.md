# Constructor for the [`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md)

Constructor for the
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md)

## Usage

``` r
# S4 method for class 'circle'
initialize(
  .Object,
  center,
  radius,
  forbidden = matrix(nrow = 0, ncol = 0),
  ...
)
```

## Arguments

- .Object:

  For this class, should be left unspecified (see Example).

- center:

  Numeric vector denoting the coordinates of the center or position of
  the circle

- radius:

  Numeric denoting the radius of the circle.

- forbidden:

  Numerical matrix containing the angles for which you cannot generate
  goals (in radians). These angles are computed in the Euclidian space
  and should be contained within the interval 0 and 2 \* pi. Defaults to
  an empty matrix, making all angles worthy of goal generation.

- ...:

  Additional arguments passed to `initialize-object`.

## Value

Object of the
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md)

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
`initialize-object`

## Examples

``` r
# Initialize a circle
my_circle <- circle(id = "my circle", 
                    center = c(0, 0), 
                    radius = 1)

# Access slots that are inherited from object and circle
my_circle@radius
#> [1] 1
my_circle@center 
#> An object of class "coordinate"
#> [1] 0 0
my_circle@id
#> [1] "my circle"
```
