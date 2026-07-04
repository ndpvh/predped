# Edges of an environment (`many_nodes = FALSE`)

This variable contains the result of running `compute_edges` in a
particular environment while setting the option `many_nodes = FALSE`. It
mainly serves as a variable used to benchmark some of the functions
related to routing.

## Usage

``` r
benchmark_few_edges
```

## Format

Named list with three slots:

- `"nodes"`:

  data.frame with 3 columns containing the identifier of the node along
  with its coordinates.

- `"edges"`:

  data.frame with 3 columns denoting the identifier of the starting and
  end node of the edge, as well as the distance between them

- `"edges_with_coords"`:

  data.frame containing the same information as in `"edges"`, but now
  accompanied with the coordinates of each node in a pair.
