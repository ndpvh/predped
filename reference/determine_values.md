# Determine iterated values from various arguments

Determine iterated values from various arguments

## Usage

``` r
determine_values(x, iterations, positive_integer = TRUE)
```

## Arguments

- x:

  Numeric, numerical vector, or function that should be used to generate
  the needed values.

- iterations:

  Integer denoting the number of values that should be generated.

- positive_integer:

  Logical denoting whether to make all drawn values positive integers.
  Defaults to `TRUE`, given the use-cases that it is currently used for.

## Value

Numeric vector containing values based on the input.

## Details

Takes in a numeric, numeric vector, or function and transforms this
input to a numeric vector that contains values based on the input. When
a numeric is provided, this function will output this same numeric
repeated an `iterations` number of times. Similarly, when a numeric
vector is provided, this numeric vector will also be repeated an
`iterations` number of times. When a function is provided, `iterations`
will be provided as an argument of this function.

This function is used to enhance the generalizability of how certain
values are determined, and is used for determining:

- the number of goals to simulate in each goal stack in the
  [`multiple_goal_stacks`](https://github.com/ndpvh/predped/reference/multiple_goal_stacks.md)
  function

- the counter for each goal in a goal stack in the
  [`goal_stack`](https://github.com/ndpvh/predped/reference/goal_stack.md)
  function

- the number of agents that can maximally be in the simulation at each
  time point in the [`simulate`](https://rdrr.io/r/stats/simulate.html)

- the iteration number at which an agent can be added to the simulation
  in the [`simulate`](https://rdrr.io/r/stats/simulate.html)

Note that for this function to work, one should correctly define their
own function when provided to the argument `x`. Specifically, this
function should take in a single argument `n` which defines the number
of values to generate. For example, a typical default for this function
is `\(n) rnorm(n, 10, 2)`.

## See also

[`simulate`](https://rdrr.io/r/stats/simulate.html)
[`goal_stack`](https://github.com/ndpvh/predped/reference/goal_stack.md)
[`multiple_goal_stacks`](https://github.com/ndpvh/predped/reference/multiple_goal_stacks.md)

## Examples

``` r
# Generate several variables to be tested with determine_values
number <- 1
numeric_vector <- 1:2
generating_function <- \(n) runif(n, 0, 10)

# Test them out with 5 iterations
determine_values(number, 5)
#> [1] 1 1 1 1 1
determine_values(numeric_vector, 5)
#>  [1] 1 2 1 2 1 2 1 2 1 2
determine_values(generating_function, 5)
#> [1] 10  2  5  6  9
```
