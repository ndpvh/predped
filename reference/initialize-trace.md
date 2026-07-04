# Constructor for the [`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md)

Constructor for the
[`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md)

## Usage

``` r
# S4 method for class 'trace'
initialize(
  .Object,
  setting = NULL,
  id = character(0),
  time_step = 0.5,
  states = list(),
  variables = list()
)
```

## Arguments

- .Object:

  For this class, should be left unspecified (see Example).

- setting:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).
  Defaults to `NULL`, in which case the constructor will throw an error
  as this is a slot that needs to be specified.

- id:

  Character that serves as an identifier for the trace. Defaults to an
  indicator `"trace"` pasted together with a random 5-letter string.

- time_step:

  Numerical denoting the time that passes at each iteration in seconds.
  Defaults to `0.5`.

- states:

  List of lists containing the state of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)s
  at each iteration. Defaults to an empty list.

- variables:

  List of list of user-specified variables that are used to control the
  simulation (see the `fx` argument of
  [`simulate`](https://rdrr.io/r/stats/simulate.html)) at each
  iteration. Defaults to an empty list.

## Value

Object of the
[`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md)

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md),
[`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md)
[`simulate`](https://rdrr.io/r/stats/simulate.html)

## Examples

``` r
# Create a background 
setting <- background(
  shape = rectangle(center = c(0, 0), size = c(5, 5)),
  objects = list(circle(center = c(0, 0), radius = 1))
)

# Initialize trace
my_trace <- trace(
  id = "my trace",
  time_step = 1,
  setting = setting
)

# Access some of the slots that were specified
my_trace@id
#> [1] "my trace"
my_trace@time_step
#> [1] 1
```
