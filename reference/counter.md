# Getter/Setter for the `counter`-slot

Works for
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## Usage

``` r
counter(object)

counter(object) <- value

# S4 method for class 'goal'
counter(object)

# S4 method for class 'goal'
counter(object) <- value
```

## Arguments

- object:

  An instance of the
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

- value:

  Value with which to replace the original value of the `counter` slot.

## Examples

``` r
# Initialize a goal with a given counter
my_goal <- goal(position = c(0, 0), 
                counter = 5)

# Access the counter 
counter(my_goal)
#> [1] 5

# Change the counter
counter(my_goal) <- 10
counter(my_goal)
#> [1] 10

```
