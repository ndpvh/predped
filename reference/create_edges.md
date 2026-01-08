# Create edges of graph to walk on

This function creates a list of nodes and edges that define the paths
that an agent might walk on to reach their goal. The output is then used
in
[`find_path`](https://github.com/ndpvh/predped/reference/find_path.md)
to find the shortest path from the agent to the goal.

## Usage

``` r
create_edges(
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

List containing a dataframe with the surviving nodes under `"nodes"`, a
dataframe with the surviving connections between nodes under `"edges"`,
and a similar dataframe to the previous one but with the coordinates
still in there under `"edges_with_coords"`.

## Details

This function depends on many other functions to do its work, and works
in the following way. First, it will create many potential nodes and
evaluate whether these nodes can be accessed, that is, that they do not
fall outside of the `shape` specified in `background` or inside of
objects that can be found in the `objects` slot of the `background`.
This part of the computation is handled by the
[`create_nodes`](https://github.com/ndpvh/predped/reference/create_nodes.md)
function.

Then, each of the nodes is combined to each other to form edges, that is
paths from one node to another that the agent might use to walk to their
goals. This is handled by the
[`combine_nodes`](https://github.com/ndpvh/predped/reference/combine_nodes.md)
function.

The appropriateness of these connections is then checked by the
[`evaluate_edges`](https://github.com/ndpvh/predped/reference/evaluate_edges.md)
function. Specifically, this function checks whether the connections
that are made do not intersect with any of the `objects` in the
`background`, that is whether none of these objects obstructs the path.
Note that this computation also accounts for potential limits in the
bidirectionality of the ways, as defined by the `limited_access` slot in
`background` (see
[`limit_access`](https://github.com/ndpvh/predped/reference/limit_access.md)).

Once this is done, the result is put in a list as required by the
[`find_path`](https://github.com/ndpvh/predped/reference/find_path.md)
to perform its computations.

Note that all edges are unidirectional. This is enforced to allow for
one- directional flow as controlled through the `limited_access` slot in
`background`.

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
[`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md),
[`find_path`](https://github.com/ndpvh/predped/reference/find_path.md),
[`limit_access`](https://github.com/ndpvh/predped/reference/limit_access.md),
[`combine_nodes`](https://github.com/ndpvh/predped/reference/combine_nodes.md),
[`create_nodes`](https://github.com/ndpvh/predped/reference/create_nodes.md),
[`evaluate_edges`](https://github.com/ndpvh/predped/reference/evaluate_edges.md)

## Examples

``` r
# Define a background in which the agent can move
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(rectangle(center = c(0, 0), 
                                                     size = c(1, 1))))

edges <- create_edges(c(-0.75, 0), 
                      c(0.75, 0), 
                      my_background, 
                      space_between = 0.25)

# Check the nodes and edges created
head(edges$nodes)
#>   node_ID          X          Y
#> 1   agent -0.7500000  0.0000000
#> 2  node 1 -0.6767767 -0.6767767
#> 3  node 2 -0.6767767  0.6767767
#> 4  node 3  0.6767767  0.6767767
#> 5  node 4  0.6767767 -0.6767767
#> 6  node 5 -0.8232233 -0.8232233
head(edges$edges)
#>       from     to      cost
#> 1.1  agent node 1 0.4633883
#> 1.2  agent node 2 0.4633883
#> 1.5  agent node 5 0.6830583
#> 1.6  agent node 6 0.6830583
#> 2   node 1  agent 0.4633883
#> 2.2 node 1 node 2 1.8321068
head(edges$edges_with_coords)
#>       from     from_x     from_y     to       to_x       to_y      cost
#> 1.1  agent -0.7500000  0.0000000 node 1 -0.6767767 -0.6767767 0.4633883
#> 1.2  agent -0.7500000  0.0000000 node 2 -0.6767767  0.6767767 0.4633883
#> 1.5  agent -0.7500000  0.0000000 node 5 -0.8232233 -0.8232233 0.6830583
#> 1.6  agent -0.7500000  0.0000000 node 6 -0.8232233  0.8232233 0.6830583
#> 2   node 1 -0.6767767 -0.6767767  agent -0.7500000  0.0000000 0.4633883
#> 2.2 node 1 -0.6767767 -0.6767767 node 2 -0.6767767  0.6767767 1.8321068
```
