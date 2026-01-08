# Evaluate whether edges pass through objects

This function evaluates whether the connections that are made between
nodes are reachable, that is that the paths are not blocked by any
objects. This also accounts for the one-way blockages that are present
in the `limited_access` slot of the
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

## Usage

``` r
evaluate_edges(segments, background, space_between, cpp = TRUE)
```

## Arguments

- segments:

  Named matrix or dataframe containing the ids of the nodes under column
  names `"from"` and `"to"`, and their coordinates under `"from_x"`,
  `"from_y"`, `"to_x"`, and `"to_y"`.

- background:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- space_between:

  Numeric denoting the space that should be left between an object and
  the created path points for the agents.

- cpp:

  Logical denoting whether to use the Rcpp alternative (`TRUE`) or the R
  alternative of this function (`FALSE`). Defaults to `TRUE`.

## Value

List containing a dataframe with the surviving connections between the
nodes under `"edges"` and a similar dataframe also containing the
coordinates under `"edges_with_coords"`.

## Details

In this function, a lot of the heavy lifting is done by the
[`line_intersection`](https://github.com/ndpvh/predped/reference/line_intersection.md)
method and the
[`prune_edges`](https://github.com/ndpvh/predped/reference/prune_edges.md)
function.

## See also

[`adjust_edges`](https://github.com/ndpvh/predped/reference/adjust_edges.md),
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md),
[`prune_edges`](https://github.com/ndpvh/predped/reference/prune_edges.md),
[`line_intersection`](https://github.com/ndpvh/predped/reference/line_intersection.md)

## Examples

``` r
# Let's create a background
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(rectangle(center = c(0, 0), 
                                                     size = c(1, 1))))

# Create some segments that do and do not go through the objects in the 
# background
potential_edges <- data.frame(from = c("node 1", "node 2", "node 3"), 
                              from_x = c(-0.75, -0.75, -0.75), 
                              from_y = c(0, 0, 0),
                              to = c("node 4", "node 5", "node 6"),
                              to_x = c(-0.75, 0.75, 0.95), 
                              to_y = c(0.75, 0, 0.95))
head(potential_edges)
#>     from from_x from_y     to  to_x to_y
#> 1 node 1  -0.75      0 node 4 -0.75 0.75
#> 2 node 2  -0.75      0 node 5  0.75 0.00
#> 3 node 3  -0.75      0 node 6  0.95 0.95

# Only retain the edges that don't intersect the object
surviving_edges <- evaluate_edges(potential_edges, 
                                  my_background,
                                  space_between = 0)
head(surviving_edges$edges_with_coords)
#>     from from_x from_y     to  to_x to_y   cost
#> 1 node 1  -0.75      0 node 4 -0.75 0.75 0.5625
#> 3 node 3  -0.75      0 node 6  0.95 0.95 3.7925
```
