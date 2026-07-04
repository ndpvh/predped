# Derive time between observations from data

These data should comply to the same restrictions as for using the
[`to_trace`](https://github.com/ndpvh/predped/reference/to_trace.md) and
[`add_motion_variables`](https://github.com/ndpvh/predped/reference/add_motion_variables.md)
functions. For more information, look at the documentation of these
functions.

## Usage

``` r
get_time_step(data, fx = mean)
```

## Arguments

- data:

  A data.frame complying to the same restrictions as for the
  [`to_trace`](https://github.com/ndpvh/predped/reference/to_trace.md)
  and
  [`add_motion_variables`](https://github.com/ndpvh/predped/reference/add_motion_variables.md)
  functions.

- fx:

  A summarizing function that should be used to aggregate the result
  across participants. Defaults to `mean`. Note that this function is
  executed twice: First within-participants and then
  between-participants.

## Value

Numeric denoting the aggregated time step in the data.

## Examples

``` r
# This is my example
```
