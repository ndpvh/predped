# Evaluate intersections with objects

In a sense, this is a vectorized alternative to
[`seesGoal`](https://rdrr.io/pkg/m4ma/man/seesGoal_rcpp.html), but is
applied more broadly to evaluate whether there are intersections between
the edges that were created and the objects in the environment.

## Usage

``` r
prune_edges(objects, segments, cpp = TRUE)
```

## Arguments

- objects:

  List of objects that extend the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

- segments:

  Numerical matrix of size N x 4 containing the coordinates of the line
  segments in order x_1, y_1, x_2, y_2.

- cpp:

  Logical denoting whether to use the Rcpp alternative (`TRUE`) or the R
  alternative of this function (`FALSE`). Defaults to `TRUE`.

## Value

Logical vector denoting whether a given edge can be retained (`TRUE`) or
should be discarded (`FALSE`)

## Details

In this function, a lot of the heavy lifting is done by the
[`line_intersection`](https://github.com/ndpvh/predped/reference/line_intersection.md)
method.

Note that this function is kept separate from
[`evaluate_edges`](https://github.com/ndpvh/predped/reference/evaluate_edges.md)
so that it can still be used for the same purposes as
[`seesGoal`](https://rdrr.io/pkg/m4ma/man/seesGoal_rcpp.html) was
originally used for.

## See also

[`adjust_edges`](https://github.com/ndpvh/predped/reference/adjust_edges.md),
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md),
[`evaluate_edges`](https://github.com/ndpvh/predped/reference/evaluate_edges.md),
[`line_intersection`](https://github.com/ndpvh/predped/reference/line_intersection.md)

## Examples

``` r
# Let's create a list of objects
objects <- list(rectangle(center = c(0, 0), size = c(1, 1)))

# Create some segments that do and do not go though the objects in the 
# background
segments <- cbind(c(-0.75, -0.75, -0.75), 
                  c(0, 0, 0),
                  c(-0.75, 0.75, 0.95), 
                  c(0.75, 0, 0.95))
segments
#>       [,1] [,2]  [,3] [,4]
#> [1,] -0.75    0 -0.75 0.75
#> [2,] -0.75    0  0.75 0.00
#> [3,] -0.75    0  0.95 0.95

# Do the test
prune_edges(objects, segments)
#> [1]  TRUE FALSE  TRUE
```
