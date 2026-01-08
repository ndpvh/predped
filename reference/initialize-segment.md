# Constructor for the [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

Constructor for the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## Usage

``` r
# S4 method for class 'segment'
initialize(.Object, from, to, ...)
```

## Arguments

- .Object:

  The class you wish to initialize. See the example for how you can
  initialize an instance of a class within this package.

- from:

  Numeric vector denoting the coordinates of the where the segment
  begins.

- to:

  Numeric vector denoting the coordinates of the where the segment ends.

- ...:

  Additional arguments passed to `initialize-object`.

## Value

Object of the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## See also

[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
`initialize-object`

## Examples

``` r
# Initialize a segment
my_segment <- segment(id = "my segment", 
                      from = c(0, 0), 
                      to = c(0, 1))

# Access slots that are inherited from object and segment
my_segment@from
#> [1] 0 0
my_segment@size
#> [1] 1
my_segment@id
#> [1] "my segment"
```
