# Predict agents' movement

Uses the agents' current speed and orientation to determine where the
agent might end up in the next step, assuming that they do not change
direction or speed. This information is used by other agents to
determine where (not) to go to avoid collisions.

## Usage

``` r
predict_movement(agent, stay_stopped = TRUE, time_step = 0.5, cpp = TRUE)
```

## Arguments

- agent:

  Object of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- stay_stopped:

  Logical denoting whether agents will predict others that are currently
  not moving to remain immobile in the next iteration. Defaults to
  `TRUE`.

- time_step:

  Numeric denoting the number of seconds each discrete step in time
  should mimic. Defaults to `0.5`, or half a second.

- cpp:

  Logical denoting whether to use the Rcpp alternative of this function
  (`TRUE`) or the R alternative (`FALSE`). Defaults to `TRUE`.

## Value

Numeric matrix containing the predicted positions all agents if they all
maintain their speed and direction.

## See also

[`create_agent_specifications`](https://github.com/ndpvh/predped/reference/create_agent_specifications.md),
[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
`update-agent`, [`update`](https://rdrr.io/r/stats/update.html)
