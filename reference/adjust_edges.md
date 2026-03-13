# Adjust edges of graph to walk on

This function adjusts a previously created list of nodes and edges to
include new nodes and edges and/or reevaluate the appropriateness of the
old ones. The output is then used in the
[`find_path`](https://github.com/ndpvh/predped/reference/find_path.md)
to find the shortest path from the agent to the goal.

## Usage

``` r
adjust_edges(
  from,
  to,
  background,
  precomputed_edges,
  space_between = 0.5,
  new_objects = NULL,
  reevaluate = !is.null(new_objects),
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

- precomputed_edges:

  Previously computed edges, usually the output of
  [`compute_edges`](https://github.com/ndpvh/predped/reference/compute_edges.md).

- space_between:

  Numeric denoting the space to leave between the circumference of the
  object and the nodes created under the hood (see
  [`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md)).
  Defaults to `0.5`.

- new_objects:

  List of instances of the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  that were previously not contained in the `objects` slot of the
  `background` when computing the edges provided in `precomputed_edges`.
  Typically, these are other pedestrians that an agent wants to account
  for when rerouting. Defaults to `NULL`, so that only `from` and `to`
  are added to the already existing edges.

- reevaluate:

  Logical denoting whether to reevaluate the appropriateness of the
  edges that are contained in the `precomputed_edges`. Defaults to
  `TRUE` when `new_objects` is not empty, and to `FALSE` whenever it is.

- cpp:

  Logical denoting whether to use the R or Rcpp version of the function.
  Defaults to `TRUE`.

## Value

List containing a dataframe with the surviving nodes under `"nodes"`, a
dataframe with the surviving connections between nodes under `"edges"`,
and a similar dataframe to the previous one but with the coordinates
still in there under `"edges_with_coords"`.

## Details

Goes through a similar type of steps as
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md),
with the exception that `adjust_edges` takes in already computed and
evaluated edges and adjusts those for the current purposes of the
environment, there where
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md)
creates these same edges from scratch.

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
[`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md),
[`find_path`](https://github.com/ndpvh/predped/reference/find_path.md),
[`limit_access`](https://github.com/ndpvh/predped/reference/limit_access.md),
[`combine_nodes`](https://github.com/ndpvh/predped/reference/combine_nodes.md),
[`compute_edges`](https://github.com/ndpvh/predped/reference/compute_edges.md),
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md),
[`create_nodes`](https://github.com/ndpvh/predped/reference/create_nodes.md),
[`evaluate_edges`](https://github.com/ndpvh/predped/reference/evaluate_edges.md)

## Examples

``` r
# Define a background in which the agent can move
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(rectangle(center = c(0, 0), 
                                                     size = c(1, 1))))

# Create precomputed edges
edges <- compute_edges(my_background, 
                       space_between = 0.25,
                       many_nodes = FALSE)
head(edges$edges)
#>       from     to       cost
#> 2.2 node 1 node 2 1.83210678
#> 2.4 node 1 node 4 1.83210678
#> 2.5 node 1 node 5 0.04289322
#> 2.6 node 1 node 6 2.27144661
#> 2.8 node 1 node 8 2.27144661
#> 3.1 node 2 node 1 1.83210678

# Adjust these edges and provide values for the from and to arguments
adjusted_edges <- adjust_edges(c(-0.75, 0), 
                               c(0.75, 0), 
                               my_background, 
                               edges,
                               space_between = 0.25)
head(adjusted_edges$edges)
#>       from    to      cost
#> 2   node 1 agent 0.4633883
#> 2.1 node 1  goal 2.4937184
#> 3   node 2 agent 0.4633883
#> 3.1 node 2  goal 2.4937184
#> 4   node 3 agent 2.4937184
#> 4.1 node 3  goal 0.4633883
```
