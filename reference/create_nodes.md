# Create nodes that serve as path points

This function creates a dataframe of nodes that can serve as path points
for the agent to use when walking to their goal. The output is used in
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md)
to create paths the agent can take, and ultimately in
[`find_path`](https://github.com/ndpvh/predped/reference/find_path.md)
for finding the shortest path from the agent to the goal.

## Usage

``` r
create_nodes(
  from,
  to,
  background,
  space_between = 0.5,
  many_nodes = FALSE,
  cpp = TRUE
)
```

## Arguments

- from:

  Numeric denoting the coordinate from which to begin routes.

- to:

  Numeric denoting the coordinate from which to end routes.

- background:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- space_between:

  Numeric denoting the space to leave between the circumference of the
  object and the nodes created under the hood (see
  [`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md)).
  Defaults to `0.5`.

- many_nodes:

  Logical denoting whether to create many nodes or leave it at the
  minimum. If `FALSE`, nodes are only added at the outlines of the
  objects contained within the `objects` slot of `background`. If
  `TRUE`, 400 additional nodes are added at an equal distance in the
  x-direction (20 nodes) and an equal distance in the y-direction (20
  nodes), making the 20 x 20 = 400 additional nodes. Defaults to
  `FALSE`.

- cpp:

  Logical denoting whether to use the Rcpp alternative (`TRUE`) or the R
  alternative of this function (`FALSE`). Defaults to `TRUE`.

## Value

Dataframe with the surviving nodes, containing the node id under
`"node_ID"` and its coordinates under `"X"` and `"Y"`.

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
[`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md),
[`find_path`](https://github.com/ndpvh/predped/reference/find_path.md),
[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md),
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md),
[`out_object`](https://github.com/ndpvh/predped/reference/out_object.md),

## Examples

``` r
# Define a background in which the agent can move
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(rectangle(center = c(0, 0), 
                                                     size = c(1, 1))))

nodes <- create_nodes(c(-0.75, 0), 
                      c(0.75, 0), 
                      my_background, 
                      space_between = 0.25)

# Check the nodes
head(nodes)
#>   node_ID          X          Y
#> 1   agent -0.7500000  0.0000000
#> 2  node 1 -0.6767767 -0.6767767
#> 3  node 2 -0.6767767  0.6767767
#> 4  node 3  0.6767767  0.6767767
#> 5  node 4  0.6767767 -0.6767767
#> 6  node 5 -0.8232233 -0.8232233
```
