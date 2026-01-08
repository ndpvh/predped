# Compute which segments can be crossed

Method that determines whether one can cross the segments that are
contained within the `limited_access` slot of the
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).
Determines this ability from the standpoint of `x`, which can either be
a matrix of coordinates or an instance of the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

## Usage

``` r
limit_access(object, x, ...)
```

## Arguments

- object:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- x:

  Either a numeric vector or matrix containing x- and y-coordinates, or
  an object of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)

- ...:

  Arguments passed on to methods of this generic

## Value

Logical vector or matrix denoting whether `segment`s can be passed
through or not.

## Details

This method takes in an object of the
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
and checks whether one can cross the `segment`s in the `limited_access`
slot from the standpoint taken in `x` (if `x` is a matrix) or from the
position of `x` (if `x` is an `agent`).

Returns a Logical vector or matrix denoting which of the segments in the
`limited_access` of the `background` object can be passed through from
the standpoint of the the coordinates in `x` (`TRUE` means that the
segment cannot be passed through while `FALSE` means that it can be
passed through). With multiple coordinates and multiple segments,
logicals are ordered with coordinates in the rows and segments in the
columns.

If `limited_access` is an empty list, it returns an empty logical
vector. If a coordinate or agent is currently intersecting a segment,
they are not blocked (i.e., then they receive the logical value
`FALSE`).

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)

## Examples

``` r
# Create a setting where you can only cross the diagonal of the space if you
# are in the lower-right part of the space
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(), 
                            limited_access = list(segment(from = c(-1, -1), 
                                                          to = c(1, 1))))

# Create two agents of whom the access may be limited or not
agent_not_limited <- agent(center = c(0.75, -0.75), radius = 0.25)
agent_limited <- agent(center = c(-0.75, 0.75), radius = 0.25)

limit_access(my_background, agent_not_limited)
#> [1] FALSE
limit_access(my_background, agent_limited)
#> [1] TRUE

# Create a matrix of coordinates of which the first is not limited and the 
# second is
coords <- rbind(c(0.75, -0.75), c(-0.75, 0.75))
limit_access(my_background, coords)
#>       [,1]
#> [1,] FALSE
#> [2,]  TRUE
```
