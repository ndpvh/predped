# Check where an object can be moved to

Rcpp alternative to
[`moving_options`](https://github.com/ndpvh/predped/reference/moving_options.md).
This method checks where an object can be moved to. It returns a logical
matrix that codes `TRUE` for the cells that are available and `FALSE`
for those that aren't.

## Usage

``` r
moving_options_rcpp(agent, state, background, centers)
```

## Arguments

- agent:

  Object of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)
  or the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  (latter not yet supported).

- state:

  Object of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)
  containing the current state.

- background:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- centers:

  Numerical matrix containing the coordinates at each position the
  object can be moved to. Should have one row for each cell.

## Value

Logical matrix containing availabilities of the centers.

## Details

In general, this method works as follows. First, it checks whether any
of the provided cell centers are freely available, in the sense that
they are not contained inside any objects or fall outside of the
setting. This is a crude measure of whether a particular spot is
available and is handled by the
[`free_cells_rcpp`](https://rdrr.io/pkg/m4ma/man/free_cells_rcpp.html)
function of the `m4ma` package.

Second, we check whether the object itself can be moved to this space,
or whether it would intersect with any of the objects and/or the outline
of the setting. This is a more direct measure of availability, as it
doesn't only account for whether a specific spot can be reached
theoretically, but also accounts for the size of the object that is
being moved there. This is handled by the
[`overlap_with_objects`](https://github.com/ndpvh/predped/reference/overlap_with_objects.md)
function in `predped`.

Finally, if the object is an instance of the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
we also check whether the agent can still see there current goal or
path-point when they move to the open spots. They will not move to the
spots from which they cannot see their goal/path-point. This is handled
by the
[`seesGoalOK_rcpp`](https://rdrr.io/pkg/m4ma/man/seesGoalOK_rcpp.html)
function in the `m4ma` package.

WARNING: Due to its reliance on the `m4ma` package, centers needs to be
of length 33 x 2. This corresponds to the 3 (change in speed) x 11
(change in orientation) options that are inherent to M4MA.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md),
[`overlap_with_objects`](https://github.com/ndpvh/predped/reference/overlap_with_objects.md)

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
                  current_goal = goal(position = c(-2.01, 0),
                                      path = matrix(c(-2.01, 0), nrow = 1)))

my_state <- state(iteration = 1,
                  setting = my_background,
                  agents = list())

# Generate several locations the agent can move to
centers <- m4ma::c_vd_r(1:33,
                        position(my_agent),
                        speed(my_agent),
                        orientation(my_agent))

# Use moving_options to see which of these possibilities is sound
moving_options(my_agent,
               my_state,
               my_background,
               centers,
               cpp = TRUE)
#>        [,1]  [,2] [,3]
#>  [1,]  TRUE  TRUE TRUE
#>  [2,]  TRUE  TRUE TRUE
#>  [3,] FALSE  TRUE TRUE
#>  [4,] FALSE  TRUE TRUE
#>  [5,] FALSE  TRUE TRUE
#>  [6,] FALSE FALSE TRUE
#>  [7,] FALSE  TRUE TRUE
#>  [8,] FALSE  TRUE TRUE
#>  [9,] FALSE  TRUE TRUE
#> [10,]  TRUE  TRUE TRUE
#> [11,]  TRUE  TRUE TRUE
```
