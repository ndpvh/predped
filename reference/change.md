# Change a goal

Replaces an existing object of
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
with an alternative object of the same class. The new goal is randomly
generated or randomly drawn from a list of options (if provided).

## Usage

``` r
change(object, ...)

# S4 method for class 'goal'
change(
  object,
  setting = NULL,
  goal_list = NULL,
  counter = function(n) rnorm(n, 10, 2)
)
```

## Arguments

- object:

  Object of
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
  which should be replaced by this function.

- ...:

  Arguments provided to the method implementations of the generic.

- setting:

  Object of
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
  within which the goal should be created. Ignored if a list of
  potential goals is provided.

- goal_list:

  List containing instances of
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
  from which the new goal should be chosen. Defaults to `NULL`,
  triggering the generation of a random goal.

- counter:

  Function that takes in no arguments and generates a single numerical
  value that will be used as the counter of the goal. See
  [`goal_stack`](https://github.com/ndpvh/predped/reference/goal_stack.md)
  for details on this argument. Defaults to `\() rnorm(1, 10, 2)`.

## Value

Object of
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## Details

This function allows for a dynamical assignment of goals to agents.

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
[`goal_stack`](https://github.com/ndpvh/predped/reference/goal_stack.md)
[`multiple_goal_stacks`](https://github.com/ndpvh/predped/reference/multiple_goal_stacks.md)

## Examples

``` r
# Create a goal
my_goal <- goal(position = c(0, 0))
my_goal
#> Goal Attributes 
#> busy: FALSE 
#> counter: 5 
#> done: FALSE 
#> id: goal kpmyi 
#> path:
#>      [,1] [,2]
#> 
#> position: 0 0 
#> 
#> For more detailed information, please extract the wanted information from the background directly.

# Replace it with a random goal drawn from the environment. For this to work, 
# we first need to create a background as well.
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(rectangle(center = c(0, 0), 
                                                     size = c(1, 1))))
change(my_goal, setting = my_background)
#> Goal Attributes 
#> busy: FALSE 
#> counter: 14 
#> done: FALSE 
#> id: goal pgwxp 
#> path:
#>      [,1] [,2]
#> 
#> position: -0.5070711 -0.04143884 
#> 
#> For more detailed information, please extract the wanted information from the background directly.

# Replace with a random goal drawn from a list of different goals. For this
# to work, we first define this list.
goal_list <- list(goal(position = c(-0.5, 0)), 
                  goal(position = c(0.5, 0)))
change(my_goal, goal_list = goal_list)
#> Goal Attributes 
#> busy: FALSE 
#> counter: 5 
#> done: FALSE 
#> id: goal eemau 
#> path:
#>      [,1] [,2]
#> 
#> position: 0.5 0 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
```
