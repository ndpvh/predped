# Compute the utilities on the agent level

This function uses the operational-level utility functions to compute
the utility of moving to any given potential cell in `centers`. Here, we
assume that none of the utility variables (i.e., the variables that
serve as input to the utility functions) is precomputed, so that it will
first compute their values. This input is then provided to
`utility-data.frame` for the actual computation of the utility.

## Usage

``` r
# S4 method for class 'agent'
utility(
  object,
  state,
  background,
  agent_specifications,
  centers,
  check,
  cpp = TRUE
)
```

## Arguments

- object:

  Object of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- state:

  Object of the
  [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md).

- background:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- agent_specifications:

  List created by the
  [`create_agent_specifications`](https://github.com/ndpvh/predped/reference/create_agent_specifications.md)
  function. Contains all information of all agents within the current
  `state` and allows for the communication between the `predped`
  simulation functions and the `m4ma` utility functions.

- centers:

  Numerical matrix containing the coordinates at each position the
  object can be moved to. Should have one row for each cell.

- check:

  Logical matrix of dimensions 11 x 3 denoting whether an agent can move
  to a given cell (`TRUE`) or not (`FALSE`).

- cpp:

  Logical denoting whether to use the Rcpp version of the function
  (`TRUE`) or the R version (`FALSE`). Defaults to `TRUE`.

## Value

Numeric vector denoting the (dis)utility of moving to each of the cells
in `centers`.

## See also

[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
`update-agent`, [`update`](https://rdrr.io/r/stats/update.html),
`utility-data.frame`, `compute_utility_variables`,
[`update_position`](https://github.com/ndpvh/predped/reference/update_position.md)
