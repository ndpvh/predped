# Plot an object

Plot an object

## Usage

``` r
# S4 method for class 'agent'
plot(
  x,
  plot_goal = TRUE,
  agent.linewidth = 1,
  agent.fill = "white",
  goal.size = 1,
  ...
)

# S4 method for class 'background'
plot(
  x,
  entry.width = 0.3,
  plot_segment = TRUE,
  shape.fill = "white",
  shape.color = "black",
  shape.linewidth = 1,
  object.fill = "grey",
  object.color = "black",
  object.linewidth = 1,
  segment.color = "black",
  segment.linewidth = 1,
  segment.size = 0.6,
  arrow.size = 0.3,
  segment.hjust = 0.5,
  plot_forbidden = FALSE,
  forbidden.color = "red",
  dark_mode = FALSE,
  optimize = TRUE,
  ...
)

# S4 method for class 'list'
plot(x, print_progress = TRUE, ...)

# S4 method for class 'object'
plot(
  x,
  color = "black",
  fill = "gray",
  plot_forbidden = FALSE,
  forbidden.color = "red",
  ...
)

# S4 method for class 'segment'
plot(
  x,
  segment.size = 0.6,
  segment.color = "black",
  segment.linewidth = 1,
  arrow.size = 0.3,
  segment.hjust = 0.5
)

# S4 method for class 'state'
plot(
  x,
  agent.linewidth = 1,
  plot.title.size = 10,
  plot.title.hjust = 0.5,
  axis.title.size = 10,
  axis.text.size = 8,
  plot_goal = TRUE,
  goal.size = 2,
  plot_segment = TRUE,
  agent.fill = shape.fill,
  shape.fill = "white",
  shape.color = "black",
  shape.linewidth = 1,
  object.fill = "grey",
  object.color = "black",
  object.linewidth = 1,
  segment.color = "black",
  segment.linewidth = 1,
  segment.size = 0.6,
  arrow.size = 0.3,
  segment.hjust = 0.5,
  plot_forbidden = FALSE,
  forbidden.color = "red",
  dark_mode = FALSE,
  optimize = TRUE,
  ...
)
```

## Arguments

- x:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
  or
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md),
  or a list containing multiple of these objects.

- plot_goal:

  Logical denoting whether to plot the position of the current goal of
  the agent together with the agent. Defaults to `TRUE`.

- agent.linewidth:

  Numeric denoting the width of the line with which to plot the agent.
  Defaults to `1`.

- agent.fill:

  Character denoting the color with which the agent should be filled in.
  Defaults to `"white"` if only plotting an agent or to `shape.fill`
  when plotting with a `background`.

- goal.size:

  Numeric denoting the size of the point that denotes the goal position.
  Defaults to `1`.

- ...:

  Additional ggplot arguments passed on to the geoms for the objects.

- entry.width:

  Numeric denoting the radius of the entrances and exits to be plotted
  in the background. Defaults to `0.3`,

- plot_segment:

  Logical denoting whether to plot segments (if there are any). If
  `TRUE`, it will add arrows to the plot that indicate the direction in
  which agents can walk. These arrows will be placed around the center
  of the line created by the instances of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).
  Defaults to `TRUE`.

- shape.fill:

  Character defining the fill color of the shape of the background.
  Defaults to `"white"`.

- shape.color:

  Character defining the color of circumference of the shape of the
  background. Defaults to `"black"`.

- shape.linewidth:

  Numeric denoting the width of the circumference of the shape of the
  background. Also concerns the width of the line of the entrances and
  exits. Defaults to `1`.

- object.fill:

  Character defining the fill color of the objects contained in the
  background. Defaults to `"grey"`.

- object.color:

  Character defining the color of the circumference of the objects
  contained in the background. Defaults to `"black"`.

- object.linewidth:

  Numeric denoting the width of the circumference of the objects
  contained in the background. Defaults to `1`.

- segment.color:

  Character defining the color of the arrows drawn when you impose
  one-directional flow. Ignored if `plot_segment = FALSE`. Defaults to
  `"black"`.

- segment.linewidth:

  Numeric denoting the linewidth of the arrows drawn when you impose
  one-directional flow. Ignored if `plot_segment = FALSE`. Defaults to
  `1`.

- segment.size:

  Numeric denoting the length of the arrow-line drawn when you impose
  one-directional flow. Ignored if `plot_segment = FALSE`. Defaults to
  `0.6`.

- arrow.size:

  Numeric denoting the size of the arrow-heads drawn when you impose
  one-directional flow in cm. Ignored if `plot_segment = FALSE`.
  Defaults to `0.3`.

- segment.hjust:

  Numeric bounded between 0 and 1 which defines how to place the arrow
  relative to the center of the segment. If `0.5`, the arrow's center
  coincides with the center of the segment. If `0`, the arrow's tail
  will coincide with the center of the segment, while if `1`, the
  arrow's head will coincide with this center. Defaults to `0.5`.

- plot_forbidden:

  Logical denoting whether to plot forbidden edges explicitly. Defaults
  to `FALSE`.

- forbidden.color:

  Character denoting the color of the forbidden edges. Defaults to
  `"red"`.

- dark_mode:

  Logical that can toggle the default colorpallette of predped's dark
  mode. Defaults to `FALSE`.

- optimize:

  Logical that defines whether to use an optimized version of the
  plotting or an unoptimized version. Defaults to `TRUE`.

- print_progress:

  Logical that denotes whether to print the iteration of the state that
  is currently being plotted. Only applies when plotting a trace.
  Defaults to `TRUE`.

- fill, color:

  Character denoting the lower-level definition of the fill and color of
  an instance of
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)

- plot.title.size:

  Numeric denoting the text size of the plot title. Defaults to `10`.

- plot.title.hjust:

  Numeric denoting the position of the plot title, with `0` coding for
  left, `1` for right, and `0.5` for the middle. Defaults to `0.5`.

- axis.title.size:

  Numeric of the text size of the axis title. Defaults to `10`.

- axis.text.size:

  Numeric denoting the text size of the axis text. Defaults to `8`.

## Value

Either a geom or a ggplot, depending on the object provided (see
Details).

## Details

Returns a geom whenever providing separate instances of the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).
Returns a ggpplot when providing instances of the
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
or of the
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).
Whenever either is provided in a list, a list containing the respective
geoms or ggplots is returned.
