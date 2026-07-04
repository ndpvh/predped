# Append values to the trace

Method used for appending values to the `states` and `variables` slots
of a
[`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md).
Note that this method can only append a single additional state to the
trace.

## Usage

``` r
append_trace(object, state, ...)

# S4 method for class 'trace,ANY'
append_trace(object, agents = NULL, variables = NULL)

# S4 method for class 'trace,state'
append_trace(object, state)
```

## Arguments

- object:

  Object of the
  [`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md)
  to which you would like to append the values of `states` and
  `variables`.

- state:

  Object of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)
  containing the current state that you would like to append to the
  trace.

- ...:

  Additional arguments specified in the methods.

- agents:

  List of instances of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)
  containing the agents at the current state. Defaults to `NULL`,
  meaning meaning no agents are found at the current trace.

- variables:

  Named list containing the values of the variables at the current
  state. Defaults to `NULL`, meaning no variables are currently tracked
  in the trace.

## Value

Object of the
[`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md)
with appended values for its slots `states` and `variables`. Note that
if both the arguments `agents` and `variables` are NULL, empty lists are
added to both slots.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md),
[`trace-class`](https://github.com/ndpvh/predped/reference/trace-class.md)

## Examples

``` r
# This is my example
```
