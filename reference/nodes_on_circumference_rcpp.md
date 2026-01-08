# Add Nodes on the Circumference of an Object

Rcpp alternative of
[`nodes_on_circumference`](https://github.com/ndpvh/predped/reference/nodes_on_circumference.md).

## Usage

``` r
nodes_on_circumference_rcpp(object, space_between)
```

## Arguments

- object:

  Object of
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

- space_between:

  Numeric denoting the space to leave between the circumference of the
  object and the nodes to create. When `outside = TRUE`, `space_between`
  distance is created to the outside of the object, while if
  `outside = FALSE` this same distance is created towards the inside of
  the object. Defaults to `5e-2`.

## Value

Numerical matrix containing the nodes that were created around/within
the provided object.

## Details

Used in the
[`overlap_with_objects`](https://github.com/ndpvh/predped/reference/overlap_with_objects.md)
function for creating nodes of which their presence within an agent can
be checked in an efficient way (see
[`moving_options`](https://github.com/ndpvh/predped/reference/moving_options.md)
and
[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md)).
Currently works for all instances of
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
but only returns `NULL` for the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

Related to the
[`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md)
with the main difference being that the
[`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md)
adds nodes around or within an object, while `nodes_on_circumference`
adds nodes directly on the circumference of an object.

Note that while
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)
is not explicitly mentioned here, this method does work for this class
of objects.

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
[`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md),
[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md),
[`moving_options`](https://github.com/ndpvh/predped/reference/moving_options.md)

## Examples

``` r
# Create an object
my_circle <- circle(center = c(0, 0), radius = 1)

# Generate nodes that fall around this circle with a distance of 1 around 
# the circle
nodes_on_circumference(my_circle, space_between = pi / 2, cpp = TRUE)
#>               [,1]          [,2]
#> [1,]  1.000000e+00  0.000000e+00
#> [2,]  6.123234e-17  1.000000e+00
#> [3,] -1.000000e+00  1.224647e-16
#> [4,] -1.836970e-16 -1.000000e+00
```
