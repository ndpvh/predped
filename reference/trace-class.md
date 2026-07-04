# An S4 Class to Represent a Trace.

Defines the `trace` class, which contains all characteristics of a
trace, including the setting, the agents at each iteration, and the time
between each iteration.

## Slots

- `id`:

  Character that serves as an identifier for the trace.

- `time_step`:

  Numerical denoting the time that passes at each iteration in seconds.

- `setting`:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- `states`:

  List of lists containing the state of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)s
  at each iteration.

- `variables`:

  List of list of user-specified variables that are used to control the
  simulation (see the `fx` argument of
  [`simulate`](https://rdrr.io/r/stats/simulate.html)) at each
  iteration.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md),
`initialize-trace` [`simulate`](https://rdrr.io/r/stats/simulate.html)
