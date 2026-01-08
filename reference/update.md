# Update State

Update the current state by updating all of the agents that are
contained in it.

## Usage

``` r
# S4 method for class 'state'
update(object, time_step = 0.5, stay_stopped = TRUE, cpp = TRUE, ...)
```

## Arguments

- object:

  Object of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

- time_step:

  Numeric denoting the number of seconds each discrete step in time
  should mimic. Defaults to `0.5`, or half a second.

- stay_stopped:

  Logical denoting whether agents will predict others that are currently
  not moving to remain immobile in the next iteration. Defaults to
  `TRUE`.

- cpp:

  Logical denoting whether to use the Rcpp alternatives for several of
  the lower-level functions (`TRUE`) or whether to use the R
  alternatives instead (`FALSE`). Defaults to `TRUE`.

- ...:

  Additional arguments passed to `update-agent`.

## Value

Object of the
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

## See also

[`create_agent_specifications`](https://github.com/ndpvh/predped/reference/create_agent_specifications.md),
[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
`update-agent`
