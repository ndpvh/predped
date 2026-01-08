# Plot the edges in a [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)

The color of the edges is determined by the argument `object.color`,
thus following the color of the outer lines of the objects in the
background. Note that `dark_mode` also works here!

## Usage

``` r
plot_edges(
  setting,
  coords = NULL,
  space_between = 1.25 * max(params_from_csv[["params_bounds"]]["radius", ]),
  many_nodes = FALSE,
  coords.color = "cornflowerblue",
  edges.color = "black",
  edges.linewidth = 1,
  nodes.color = "black",
  nodes.size = 1,
  dark_mode = FALSE,
  ...
)
```

## Arguments

- setting:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)

- coords:

  Numeric matrix containing the positions of the agent and the goal.
  Defaults to `NULL`, leaving them out of the equation.

- space_between:

  Numeric denoting the space to leave between the circumference of the
  object and the nodes created under the hood (see
  [`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md)).
  Defaults to `2.5` times the maximal radius of an agent.

- many_nodes:

  Logical denoting whether to create many nodes or leave it at the
  minimum. Defaults to `FALSE`.

- coords.color:

  Character denoting the color to provide to the agent and goal
  positions. Allows one to distinguish between simple path points and
  the start and end positions. Defaults to `"cornflowerblue"`.

- edges.color:

  Character denoting the color of the edges that are plotted. Defaults
  to `"black"`.

- edges.linewidth:

  Numeric denoting the linewidth of the edges that are plotted. Defaults
  to `1`.

- nodes.color:

  Character denoting the color of the nodes that are plotted. Defaults
  to `"black"`.

- nodes.size:

  Numeric denoting the size of the nodes that are plotted. Defaults to
  `1`.

- dark_mode:

  Logical denoting whether to use the predped-default dark mode for
  plotting. Overrides the color arguments provided to the plotting
  function. Defaults to `FALSE`.

- ...:

  Arguments provided to the
  [`plot`](https://rdrr.io/r/graphics/plot.default.html) function for
  the `setting`

## Value

Plot created by `ggplot2`

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
[`compute_edges`](https://github.com/ndpvh/predped/reference/compute_edges.md),
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md),
[`plot`](https://rdrr.io/r/graphics/plot.default.html)
