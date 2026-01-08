# Interact with goal

Defines the interaction with an object of
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).
Subtracts `1` from the counter and indicates whether the goal has been
accomplished (whenever the counter drops below 0).

## Usage

``` r
interact(object)

# S4 method for class 'goal'
interact(object)
```

## Arguments

- object:

  Object of
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## Value

Object of
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
with adjusted `counter` slot and, if the goal has been completed, an
adjusted `done` slot

## See also

[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)

## Examples

``` r
# Create a goal
my_goal <- goal(position = c(0, 0), 
                counter = 5)

# Interact with the goal: Decreases the counter, but the goal is not done yet
updated_goal <- interact(my_goal)
updated_goal@counter
#> [1] 4
updated_goal@done
#> [1] FALSE

# Adjust the goal so that the counter is only 1 and interact with it again.
# Now the goal is done.
counter(my_goal) <- 1

updated_goal <- interact(my_goal)
updated_goal@counter
#> [1] 0
updated_goal@done
#> [1] TRUE
```
