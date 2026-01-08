# Constructor for the [`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)

Constructor for the
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)

## Usage

``` r
# S4 method for class 'state'
initialize(
  .Object,
  iteration,
  setting,
  agents = list(),
  potential_agents = list(),
  iteration_variables = data.frame(),
  variables = list()
)
```

## Arguments

- .Object:

  For this class, should be left unspecified (see Example).

- iteration:

  Numeric denoting the iteration number that this state represents.
  Makes it possible to order states into one coherent trace, showing how
  agents walked around over time.

- setting:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- agents:

  List containing objects of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)
  representing the agents that are currently walking around in the
  `setting`. Defaults to an empty list.

- potential_agents:

  List containing objects of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)
  representing agents that are waiting to enter the `setting`. Defaults
  to an empty list.

- iteration_variables:

  Dataframe containing values for variables that control the simulation
  under the hood, such as `max_agents`. Defaults to an empty data.frame.

- variables:

  Named list containing variables that you want to use to control the
  simulation in the `fx` argument of the
  [`simulate`](https://rdrr.io/r/stats/simulate.html). Defaults to an
  empty list.

## Value

Object of the
[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md)

## See also

[`state-class`](https://github.com/ndpvh/predped/reference/state-class.md),
[`agents`](https://github.com/ndpvh/predped/reference/agents.md),
[`iteration`](https://github.com/ndpvh/predped/reference/iteration.md),
[`potential_agents`](https://github.com/ndpvh/predped/reference/potential_agents.md),
[`setting`](https://github.com/ndpvh/predped/reference/setting.md)

## Examples

``` r
# Create a background in which agents will walk around
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)))

# Initialize state
my_state <- state(iteration = 0,
                  setting = my_background)

# Access the two slots that were specified
my_state@iteration
#> [1] 0
my_state@agents
#> list()
```
