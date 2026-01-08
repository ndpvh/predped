# Compute the edges within a given setting

This function combines all non-dynamically determined nodes into a
comprehensive graph that can then be used in the simulation as
`precomputed_edges`. Effectively uses
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md)
to create the edges and then deletes the `agent` and `goal` nodes from
the graph.

## Usage

``` r
compute_edges(
  background,
  space_between = 2.5 * max(params_from_csv[["params_bounds"]]["radius", ]),
  many_nodes = TRUE,
  cpp = TRUE
)
```

## Arguments

- background:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- space_between:

  Numeric denoting the space to leave between the circumference of the
  object and the nodes created under the hood (see
  [`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md)).
  Defaults to 2.5 times the maximal possible radius from the default
  [`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md).

- many_nodes:

  Logical denoting whether to create many nodes or leave it at the
  minimum. If `FALSE`, nodes are only added at the outlines of the
  objects contained within the `objects` slot of `background`. If
  `TRUE`, 400 additional nodes are added at an equal distance in the
  x-direction (20 nodes) and an equal distance in the y-direction (20
  nodes), making the 20 x 20 = 400 additional nodes. Defaults to
  `FALSE`.

- cpp:

  Logical denoting whether to use the R or Rcpp version of the function.
  Defaults to `TRUE`.

## Value

List containing a dataframe with the surviving nodes under `"nodes"`, a
dataframe with the surviving connections between nodes under `"edges"`,
and a similar dataframe to the previous one but with the coordinates
still in there under `"edges_with_coords"`.

## See also

[`adjust_edges`](https://github.com/ndpvh/predped/reference/adjust_edges.md)
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md)
[`create_nodes`](https://github.com/ndpvh/predped/reference/create_nodes.md)
[`find_path`](https://github.com/ndpvh/predped/reference/find_path.md)
[`simulate`](https://rdrr.io/r/stats/simulate.html) `simulate.state`

## Examples

``` r
# Define a background in which the agent can move
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(rectangle(center = c(0, 0), 
                                                     size = c(1, 1))))

edges <- compute_edges(my_background, 
                       space_between = 0.25)

# Check the nodes and edges created
head(edges$nodes)
#>   node_ID     X      Y
#> 2  node 1 -0.75 -0.750
#> 3  node 2 -0.75 -0.675
#> 4  node 3 -0.75 -0.600
#> 5  node 4 -0.75 -0.525
#> 6  node 5 -0.75 -0.450
#> 7  node 6 -0.75 -0.375
head(edges$edges)
#>       from     to     cost
#> 2.2 node 1 node 2 0.005625
#> 2.3 node 1 node 3 0.022500
#> 2.4 node 1 node 4 0.050625
#> 2.5 node 1 node 5 0.090000
#> 2.6 node 1 node 6 0.140625
#> 2.7 node 1 node 7 0.202500
head(edges$edges_with_coords)
#>       from from_x from_y     to  to_x   to_y     cost
#> 2.2 node 1  -0.75  -0.75 node 2 -0.75 -0.675 0.005625
#> 2.3 node 1  -0.75  -0.75 node 3 -0.75 -0.600 0.022500
#> 2.4 node 1  -0.75  -0.75 node 4 -0.75 -0.525 0.050625
#> 2.5 node 1  -0.75  -0.75 node 5 -0.75 -0.450 0.090000
#> 2.6 node 1  -0.75  -0.75 node 6 -0.75 -0.375 0.140625
#> 2.7 node 1  -0.75  -0.75 node 7 -0.75 -0.300 0.202500
```
