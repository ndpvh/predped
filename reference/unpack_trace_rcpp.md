# Transform trace to comprehensive data.frame

Rcpp alternative for the
[`unpack_trace`](https://github.com/ndpvh/predped/reference/unpack_trace.md)
function.

## Usage

``` r
unpack_trace_rcpp(
  trace,
  velocities,
  orientations,
  stay_stopped = TRUE,
  time_step = 0.5
)
```

## Arguments

- trace:

  List of objects of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)

- velocities:

  Numeric matrix containing the change in speed for an agent whenever
  they move to the respective cell of this matrix. Is used to create the
  cell positions that the agent might move to. Defaults to a matrix in
  which the columns contain `1.5` (acceleration), `1` (maintenance of
  speed), and `0.5` (deceleration).

- orientations:

  Numeric matrix containing the change in direction for an agent
  whenever they move to the respective cell of this matrix. Is used to
  create the cell positions that the agent might move to. Defaults to a
  matrix in which the rows contain `72.5`, `50`, `32.5`, `20`, `10`,
  `0`, `350`, `340`, `327.5`, `310`, `287.5` (note that the larger
  angles are actually the negative symmetric versions of the smaller
  angles).

- stay_stopped:

  Logical denoting whether agents will predict others that are currently
  not moving to remain immobile in the next iteration. Defaults to
  `TRUE`.

- time_step:

  Numeric denoting the time between each iteration. Defaults to `0.5`
  (the same as in [`simulate`](https://rdrr.io/r/stats/simulate.html)).

## Details

This function will take a trace and return a data.frame containing all
information contained within a typical time-series (cfr.
[`time_series`](https://github.com/ndpvh/predped/reference/time_series.md))
and with all the input that should be provided to the utility functions.
This is therefore the primary function to use if you want to go from a
trace to a data.frame that can be used in M4MA-based estimations.

## Examples

``` r
# This is my example
```
