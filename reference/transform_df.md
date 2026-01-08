# Transform to dataframe of segments

Lower-level functions that transform the provided information into rows
within a `data.frame`. Is used when the `optimize` argument of
[`plot`](https://rdrr.io/r/graphics/plot.default.html) is set to `TRUE`,
as it ensures that `ggplot2` only has to look at one big `data.frame`
instead of many smaller ones, which greatly reduces the computational
load for making the plots.

## Usage

``` r
transform_df(object, ...)

# S4 method for class 'agent'
transform_df(object, plot_goal = TRUE, goal.size = 2/100)

# S4 method for class 'background'
transform_df(object, entry.width = 0.3, ...)

# S4 method for class 'list'
transform_df(object, ...)

# S4 method for class 'object'
transform_df(object, kind = "object", plot_forbidden = FALSE, ...)

# S4 method for class 'segment'
transform_df(
  object,
  kind = "segment",
  segment.hjust = 0.5,
  segment.size = 0.6,
  ...
)

# S4 method for class 'state'
transform_df(object, entry.width = 0.3, plot_goal = TRUE, goal.size = 2/100)
```

## Arguments

- object:

  Instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
  `list`,
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
  or
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)
  to be transformed for plotting purposes

- ...:

  Arguments passed on to lower-level functions.

- plot_goal:

  Logical denoting whether to plot the goal together with the agent.
  Defaults to `TRUE`.

- goal.size:

  Radius of the circle that should be plotted at the location of the
  agent's goal. Ignored if `plot_goal` is `FALSE`. Defaults to `2/100`.

- entry.width:

  Radius of the circle that should be plotted at the location of the
  entries and exits of the background. Defaults to `0.3`.

- kind:

  Character denoting what the category of the objects are. Defaults to
  `"object"` for instances of
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  or `"segment"` for instances of
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).
  Is otherwise handled under the hood.

- plot_forbidden:

  Logical denoting wether to plot the forbidden edges or regions of
  objects. Defaults to `FALSE`.

- segment.hjust:

  Numeric between 0 and 1 denoting the horizontal justification of the
  arrow denoting one-directional movement. Defaults to `0.5`

- segment.size:

  Numeric denoting the size of the arrow denoting one-directional
  movement. Defaults to `0.6`.
