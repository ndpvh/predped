# Check agent and object overlap

This function checks whether there is an overlap between a given agent
and the objects in the environment, provided that the agent would move
to the locations in `centers`. Returns a logical matrix as needed in
[`moving_options`](https://github.com/ndpvh/predped/reference/moving_options.md).

## Usage

``` r
overlap_with_objects(
  agent,
  background,
  centers,
  check,
  space_between = 0.05,
  cpp = FALSE
)
```

## Arguments

- agent:

  Object of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- background:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- centers:

  Numerical matrix containing the coordinates at each position the
  object can be moved to. Should have one row for each cell.

- check:

  Logical matrix of dimensions 11 x 3 denoting whether an agent can move
  to a given cell (`TRUE`) or not (`FALSE`).

- space_between:

  Numeric denoting the space to leave between the nodes put on the
  circumference of the objects in the space (used for checking the
  overlap with an agent). Defaults to `0.05` or 5cm.

- cpp:

  Logical denoting whether to use the Rcpp alternative (`TRUE`) or the R
  alternative of this function (`FALSE`). Defaults to `TRUE`.

## Value

Logical matrix containing availabilities of the centers (`TRUE` if
available, `FALSE` if not).

## Details

In this function, we can only approximately check the intersection of
agent and object. Specifically, we use the following method. First, we
sample nodes on the circumference of each of the objects in the setting
that is provided to this function. For this, we depend on the function
[`nodes_on_circumference`](https://github.com/ndpvh/predped/reference/nodes_on_circumference.md)
and we currently take these nodes to be 5cm.

In the next step, we bind all these coordinates together in a single
matrix. This matrix thus consists of nodes that should not be embedded
in the agents: Whenever one of these points is included in the agents,
we can conclude that the agents and objects intersect. \[Note, however,
that if these points are not included in the agents, that we cannot with
certainty conclude that agent and object do not intersect\]

This check is then performed by looping over all the centers, changing
the agents position to the position of this center, and using the
[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md)
to do the test. This is a vectorized test: For each position in
`centers` we have a logical `TRUE` or `FALSE` for each of the nodes in
the coordinate matrix, resulting in a logical matrix with an equal
number of rows as `centers` and an equal number of columns as nodes in
the coordinate matrix. In a last step, we aggregate over the columns in
this matrix so that we have a single logical for each center.

The reason why we use this approximate method is because of time
efficiency. Using the
[`intersects`](https://github.com/ndpvh/predped/reference/intersects.md)
takes a longer time than using the
[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md),
especially as the number of objects in the environment increases.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md),
[`intersects`](https://github.com/ndpvh/predped/reference/intersects.md),
[`moving_options`](https://github.com/ndpvh/predped/reference/moving_options.md),
[`nodes_on_circumference`](https://github.com/ndpvh/predped/reference/nodes_on_circumference.md)

## Examples

``` r
# Initialize all objects that you need
my_background <- background(shape = rectangle(center = c(0, 0),
                                              size = c(6, 6)),
                            objects = list(circle(center = c(0, 0),
                                                  radius = 2)))
my_agent <- agent(center = c(-2.75, 0),
                  radius = 0.25,
                  speed = 1,
                  orientation = 0,
                  current_goal = goal(position = c(-2.01, 0)))

# Generate several locations the agent can move to
centers <- compute_centers(my_agent)
check <- matrix(TRUE, nrow = 11, ncol = 3)

# Use moving_options to see which of these possibilities is sound
overlap_with_objects(my_agent,
                     my_background,
                     centers,
                     check)
#>        [,1] [,2] [,3]
#>  [1,]  TRUE TRUE TRUE
#>  [2,]  TRUE TRUE TRUE
#>  [3,] FALSE TRUE TRUE
#>  [4,] FALSE TRUE TRUE
#>  [5,] FALSE TRUE TRUE
#>  [6,] FALSE TRUE TRUE
#>  [7,] FALSE TRUE TRUE
#>  [8,] FALSE TRUE TRUE
#>  [9,] FALSE TRUE TRUE
#> [10,]  TRUE TRUE TRUE
#> [11,]  TRUE TRUE TRUE
```
