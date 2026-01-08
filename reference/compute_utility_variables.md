# Compute utility variables

This function uses the current state of the environment to determine the
values of a whole range of variables that are used within the utility
functions.

This function uses the current state of the environment to determine the
values of a whole range of variables that are used within the utility
functions.

## Usage

``` r
# S4 method for class 'agent'
compute_utility_variables(
  object,
  state,
  background,
  agent_specifications,
  centers,
  check,
  cpp = TRUE
)

# S4 method for class 'data.frame'
compute_utility_variables(object, background)
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

  Logical denoting whether to use the Rcpp alternative (`TRUE`) or the R
  alternative of this function (`FALSE`). Defaults to `TRUE`.

## Value

Data.frame containing all of the needed variables to be able to compute
the values of the utility functions.

Data.frame containing all of the needed variables to be able to compute
the values of the utility functions.

## See also

[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
`update-agent`, [`update`](https://rdrr.io/r/stats/update.html),
[`update_position`](https://github.com/ndpvh/predped/reference/update_position.md),
[`update`](https://rdrr.io/r/stats/update.html)

[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
`update-agent`, [`update`](https://rdrr.io/r/stats/update.html),
[`update_position`](https://github.com/ndpvh/predped/reference/update_position.md),
[`update`](https://rdrr.io/r/stats/update.html)
