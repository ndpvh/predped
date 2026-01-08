# Generate a goal stack

Use the defined setting to generate the stack of random goals an agent
should complete. This function outputs a list of different instances of
the
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## Usage

``` r
goal_stack(
  n,
  setting,
  counter = function(n) rnorm(n, 10, 2),
  sort = TRUE,
  starting_position = entrance(setting)[1, ],
  precompute_goal_paths = FALSE,
  middle_edge = FALSE,
  ...
)
```

## Arguments

- n:

  Integer denoting the number of goals to generate.

- setting:

  Object of
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- counter:

  Numeric, vector, or function that defines the counter for each of the
  goals that are generated. When numeric, each goal will have the same
  value for counter equal to the value provided to this argument. When a
  numeric vector, the values of this vector will be iterated as values
  for the counter in the goals. When a function, a random value for the
  counter will be generated through the function. For this to work, the
  function that is provided should take in the input `n` which defines
  the number of values to draw from the function. Defaults to
  `\(n) rnorm(n, 10, 2)`.

- sort:

  Logical denoting whether to order the goal stack in a logical way.
  Currently implemented in the following way. First, we select the first
  goal as being the one that is closest by the starting position
  provided in the argument `starting_position`. Then, we define each of
  the next goals as being the one that is closest to the position of the
  previous goal. Defaults to `TRUE`.

- starting_position:

  Numeric vector denoting the position at which the agent starts in the
  room. Defaults to the first entrance of the `setting`.

- precompute_goal_paths:

  Logical denoting whether to run the
  [`find_path`](https://github.com/ndpvh/predped/reference/find_path.md)
  for each of the generated goals beforehand. Assumes that the agent
  does all of the goals in the order of the goal stack. Defaults to
  `FALSE`.

- middle_edge:

  Logical denoting whether to sample the goals from the middle of the
  edge of the objects in the `link[predped]{background-class}` (`TRUE`)
  or to allow the goal locations to fall on all points on these edges
  (`FALSE`). Defaults to `FALSE`.

- ...:

  Arguments provided to
  [`find_path`](https://github.com/ndpvh/predped/reference/find_path.md)
  to precompute the paths that the agents should take to reach their
  goals. Only used when `precompute_goal_paths = TRUE`.

## Value

List of instances of the
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
[`compute_edges`](https://github.com/ndpvh/predped/reference/compute_edges.md)
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md)
[`determine_values`](https://github.com/ndpvh/predped/reference/determine_values.md)
[`multiple_goal_stacks`](https://github.com/ndpvh/predped/reference/multiple_goal_stacks.md)

## Examples

``` r
# Create a setting
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(circle(center = c(0, 0), 
                                                  radius = 0.5)))

# Create a goal stack containing two goals
stack <- goal_stack(2, my_background)

# Two goals
length(stack)
#> [1] 2
stack
#> [[1]]
#> Goal Attributes 
#> busy: FALSE 
#> counter: 9 
#> done: FALSE 
#> id: goal tpwnt 
#> path:
#>      [,1] [,2]
#> 
#> position: -0.4004846 -0.3157721 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
#> 
#> [[2]]
#> Goal Attributes 
#> busy: FALSE 
#> counter: 4 
#> done: FALSE 
#> id: goal kdxbw 
#> path:
#>      [,1] [,2]
#> 
#> position: 0.4129072 0.2993453 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
#> 
```
