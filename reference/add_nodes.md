# Add Nodes along the Outside or Inside of an Object

Used in the
[`create_nodes`](https://github.com/ndpvh/predped/reference/create_nodes.md)
function for creating path- points the agents can use to walk around
objects. Currently works for all instances of
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
but only returns `NULL` for the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## Usage

``` r
add_nodes(object, ...)

# S4 method for class 'polygon'
add_nodes(
  object,
  space_between = 0.5,
  only_corners = FALSE,
  outside = TRUE,
  cpp = TRUE
)

# S4 method for class 'circle'
add_nodes(
  object,
  space_between = 0.5,
  only_corners = FALSE,
  outside = TRUE,
  cpp = TRUE
)

# S4 method for class 'segment'
add_nodes(object, ...)
```

## Arguments

- object:

  Object of
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

- ...:

  Arguments passed on to the methods of this generic

- space_between:

  Numeric denoting the space to leave between the circumference of the
  object and the nodes to create. When `outside = TRUE`, `space_between`
  distance is created to the outside of the object, while if
  `outside = FALSE` this same distance is created towards the inside of
  the object. Defaults to `0.5`.

- only_corners:

  Logical denoting whether to only add nodes at the corners of each
  object (`TRUE`) or to also add nodes between these newly created
  corner-nodes (`FALSE`). When `only_corners = FALSE`, other nodes that
  are added have a distance of `space_between` between them. Defaults to
  `FALSE`.

- outside:

  Logical denoting whether the nodes should lie on the outside (`TRUE`)
  or inside (`FALSE`) of the object. Defaults to `TRUE`.

- cpp:

  Logical denoting whether to use the R or Rcpp version of the function.
  Defaults to `TRUE`.

## Value

Numerical matrix containing the nodes that were created around/within
the provided object.

## Details

This method is related to the
[`nodes_on_circumference`](https://github.com/ndpvh/predped/reference/nodes_on_circumference.md),
but differs in the respect that
[`nodes_on_circumference`](https://github.com/ndpvh/predped/reference/nodes_on_circumference.md)
adds nodes directly on the circumference of an object, while `add_nodes`
adds nodes on the outside of inside of an object.

Please note that if `only_corners = TRUE` for the
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
that the nodes are chosen in such a way that they are at a distance of
`space_between` away from the circumference of the circle AND so that
this node is connected to its neighbours through a tangential line to
the circle. In agent-based terms, this means that if an agent stands at
one of the nodes that are created, they can also see alternative nodes
around the circle. If `only_corners = FALSE`, the general logic is used
to determine where the other nodes should end up.

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
[`nodes_on_circumference`](https://github.com/ndpvh/predped/reference/nodes_on_circumference.md),
[`create_nodes`](https://github.com/ndpvh/predped/reference/create_nodes.md)

## Examples

``` r
# Create an object
my_circle <- circle(center = c(0, 0), radius = 1)

# Generate nodes that fall around this circle with a distance of 1 around 
# the circle
add_nodes(my_circle, space_between = 1)
#>                [,1]          [,2]
#>  [1,]  2.000000e+00  0.000000e+00
#>  [2,]  1.847759e+00  7.653669e-01
#>  [3,]  1.414214e+00  1.414214e+00
#>  [4,]  7.653669e-01  1.847759e+00
#>  [5,]  1.224647e-16  2.000000e+00
#>  [6,] -7.653669e-01  1.847759e+00
#>  [7,] -1.414214e+00  1.414214e+00
#>  [8,] -1.847759e+00  7.653669e-01
#>  [9,] -2.000000e+00  2.449294e-16
#> [10,] -1.847759e+00 -7.653669e-01
#> [11,] -1.414214e+00 -1.414214e+00
#> [12,] -7.653669e-01 -1.847759e+00
#> [13,] -3.673940e-16 -2.000000e+00
#> [14,]  7.653669e-01 -1.847759e+00
#> [15,]  1.414214e+00 -1.414214e+00
#> [16,]  1.847759e+00 -7.653669e-01
#> [17,]  2.000000e+00 -4.898587e-16

# Note that for segments, this function returns NULL
my_segment <- segment(from = c(0, 0), to = c(2, 2))
add_nodes(my_segment)
#> NULL
```
