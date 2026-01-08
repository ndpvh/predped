# Compute cell centers

Compute cell centers based on a person's current position and velocity,
accounting for potential changes in speed and direction. Alternative to
[`c_vd`](https://rdrr.io/pkg/m4ma/man/c_vd_rcpp.html) that accounts for
biomechanical limitations in the speed one can maintain when turning at
a greater angle. Defaults are based on Seethapathi et al. (2024), Brown
et al. (2020), and Glaister et al. (2007).

## Usage

``` r
compute_centers(
  agent,
  velocities = matrix(rep(c(1.5, 1, 0.5), each = 11), ncol = 3),
  orientations = matrix(rep(c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 327.5, 310, 287.5),
    times = 3), ncol = 3),
  time_step = 0.5,
  cpp = TRUE
)
```

## Arguments

- agent:

  Object of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- velocities:

  Numeric matrix containing the change in speed for an agent whenever
  they move to the respective cell of this matrix. Defaults to a matrix
  in which the columns contain `1.5` (acceleration), `1`, and `0.5`.

- orientations:

  Numeric matrix containing the change in direction for an agent
  whenever they move to the respective cell of this matrix. Defaults to
  a matrix in which the rows contain `72.5`, `50`, `32.5`, `20`, `10`,
  `0`, `350`, `340`, `327.5`, `310`, `287.5` (note that the larger
  angles are actually the negative symmetric versions of the smaller
  angles).

- time_step:

  Numeric denoting the number of seconds each discrete step in time
  should mimic. Defaults to `0.5`, or half a second.

- cpp:

  Logical denoting whether to use the Rcpp alternative (`TRUE`) or the R
  alternative of this function (`FALSE`). Defaults to `TRUE`.

## Value

Numeric matrix of (x, y) coordinates for each cell

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`c_vd`](https://rdrr.io/pkg/m4ma/man/c_vd_rcpp.html)
[`moving_options`](https://github.com/ndpvh/predped/reference/moving_options.md)

## Examples

``` r
# Create two agents, one fast and one slow
slow_agent <- agent(center = c(-2.75, 0),
                    radius = 0.25,
                    speed = 0.5,
                    orientation = 0,
                    current_goal = goal(position = c(-2.01, 0)))

fast_agent <- agent(center = c(-2.75, 0),
                    radius = 0.25,
                    speed = 2,
                    orientation = 0,
                    current_goal = goal(position = c(-2.01, 0)))

# Generate the cell centers with predped
slow_centers <- compute_centers(slow_agent)
fast_centers <- compute_centers(fast_agent)

# Generate the cell centers with m4ma
slow_m4ma <- m4ma::c_vd(1:33,
                        position(slow_agent),
                        speed(slow_agent),
                        orientation(slow_agent))
fast_m4ma <- m4ma::c_vd(1:33,
                        position(fast_agent),
                        speed(fast_agent),
                        orientation(fast_agent))

# Compare both through a plot. This should show that the predped variant
# accounts for an interaction between an agent's speed and change in
# direction when computing the cell centers
base::plot(slow_centers, col = "black")
graphics::points(slow_m4ma[, 1], slow_m4ma[, 2], col = "red")


base::plot(fast_centers, col = "black")
graphics::points(fast_m4ma[, 1], fast_m4ma[, 2], col = "red")


```
