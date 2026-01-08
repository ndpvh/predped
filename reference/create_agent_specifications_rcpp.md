# Create agent specifications

Rcpp alternative to the
[`create_agent_specifications`](https://github.com/ndpvh/predped/reference/create_agent_specifications.md)
function.

## Usage

``` r
create_agent_specifications_rcpp(
  agent_list,
  stay_stopped = TRUE,
  time_step = 0.5
)
```

## Arguments

- agent_list:

  List of objects of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- stay_stopped:

  Logical denoting whether agents will predict others that are currently
  not moving to remain immobile in the next iteration. Defaults to
  `TRUE`.

- time_step:

  Numeric denoting the number of seconds each discrete step in time
  should mimic. Defaults to `0.5`, or half a second.

## Value

List containing all information of all agents within the current state.

## Details

This list translates the information available in the `agents` slot of
the current status of the
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)
to a list with all this information in numeric vectors or matrices
instead of inside objects. Allows for a translation from the
object-oriented way of doing things in `predped` to the vectorized way
of doing things in `m4ma`.

## See also

[`create_agent_specifications`](https://github.com/ndpvh/predped/reference/create_agent_specifications.md),
[`simulate`](https://rdrr.io/r/stats/simulate.html), `simulate.state`,
`update-agent`, [`update`](https://rdrr.io/r/stats/update.html)
