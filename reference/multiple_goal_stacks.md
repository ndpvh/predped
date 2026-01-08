# Generate multiple goal stacks

Use the defined setting to generate multiple stacks of random goals that
each agent might have to complete. This function outputs a list of
multiple lists with different instances of the
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## Usage

``` r
multiple_goal_stacks(
  n,
  setting,
  goal_number = function(x) rnorm(x, 10, 2),
  ...
)
```

## Arguments

- n:

  Integer denoting the number of goals to generate.

- setting:

  Object of
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- goal_number:

  Numeric, vector, or function that defines the number of goals for each
  of the goal stacks that will be generated. When numeric, each goal
  stack will have the same number of goals equal to the value provided
  to this argument. When a numeric vector, the values of this vector
  will be iterated as values for the number of goals in the goal stacks.
  When a function, a random value for the number of goals for each goal
  stack counter will be generated through the function. For this to
  work, the function that is provided should take in the input `n` which
  defines the number of values to draw from the function. Defaults to
  `\(n) rnorm(n, 10, 2)`.

- ...:

  Arguments provided to
  [`goal_stack`](https://github.com/ndpvh/predped/reference/goal_stack.md).

## Value

List of lists containing instances of the
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
[`determine_values`](https://github.com/ndpvh/predped/reference/determine_values.md)
[`goal_stack`](https://github.com/ndpvh/predped/reference/goal_stack.md)

## Examples

``` r
# Create a setting
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(circle(center = c(0, 0), 
                                                  radius = 0.5)))

# Create two goal stacks containing two goals each
goal_stack <- multiple_goal_stacks(2, my_background, goal_number = 2)
#> Generating multiple goal stacks

# Two goal stacks of two goals each
length(goal_stack)
#> [1] 2

length(goal_stack[[1]])
#> [1] 2
goal_stack[[1]]
#> [[1]]
#> Goal Attributes 
#> busy: FALSE 
#> counter: 13 
#> done: FALSE 
#> id: goal shgiz 
#> path:
#>      [,1] [,2]
#> 
#> position: 0.4810857 -0.1692824 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
#> 
#> [[2]]
#> Goal Attributes 
#> busy: FALSE 
#> counter: 9 
#> done: FALSE 
#> id: goal qmlio 
#> path:
#>      [,1] [,2]
#> 
#> position: 0.3848236 -0.3346802 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
#> 

length(goal_stack[[2]])
#> [1] 2
goal_stack[[2]]
#> [[1]]
#> Goal Attributes 
#> busy: FALSE 
#> counter: 9 
#> done: FALSE 
#> id: goal pzrux 
#> path:
#>      [,1] [,2]
#> 
#> position: 0.4396041 -0.2585503 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
#> 
#> [[2]]
#> Goal Attributes 
#> busy: FALSE 
#> counter: 13 
#> done: FALSE 
#> id: goal ijagd 
#> path:
#>      [,1] [,2]
#> 
#> position: -0.3123016 -0.4031968 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
#> 
```
