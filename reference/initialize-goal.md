# Constructor for the [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)

Constructor for the
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)

## Usage

``` r
# S4 method for class 'goal'
initialize(
  .Object,
  id = character(0),
  position = numeric(2),
  path = NULL,
  busy = FALSE,
  done = FALSE,
  counter = 5
)
```

## Arguments

- .Object:

  For this class, should be left unspecified (see Example).

- id:

  Character that serves as an identifier for the goal. Defaults to an
  empty character, triggering the random generation of an id.

- position:

  Numerical vector denoting the position of the goal. Defaults to
  `c(0, 0)`.

- path:

  Numerical matrix of size n x 2 denoting the different intermediate
  path points between an agent and a goal. In effect defines which path
  the agent will take to move towards the goal. Defaults to an empty
  matrix of size 2 x 2.

- busy:

  Logical denoting whether the goal is currently being interacted with.
  Defaults to `FALSE`.

- done:

  Logical denoting whether a goal has been completed. Defaults to
  `FALSE`.

- counter:

  Numeric denoting the number of time steps the agent should interact
  with the goal before the goal has been completed. Defaults to `5`.

## Value

Object of the
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)

## See also

[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md),
[`busy`](https://github.com/ndpvh/predped/reference/busy.md),
[`counter`](https://github.com/ndpvh/predped/reference/counter.md),
[`done`](https://github.com/ndpvh/predped/reference/done.md),
[`id`](https://github.com/ndpvh/predped/reference/id.md),
[`path`](https://github.com/ndpvh/predped/reference/path.md),
[`position`](https://github.com/ndpvh/predped/reference/position.md)

## Examples

``` r
# Initialize agent
my_goal <- goal(id = "my goal", position = c(1, 1))

# Access the two slots that were specified
my_goal@id
#> [1] "my goal"
my_goal@position
#> An object of class "coordinate"
#> [1] 1 1
```
