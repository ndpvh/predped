# Getter/Setter for the `path`-slot

Works for
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## Usage

``` r
path(object)

path(object) <- value

# S4 method for class 'goal'
path(object)

# S4 method for class 'goal'
path(object) <- value
```

## Arguments

- object:

  An instance of the
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

- value:

  Value with which to replace the original value of the `path` slot.

## Examples

``` r
# Initialize a goal with a given path
my_goal <- goal(position = c(0, 0), 
                path = matrix(1:4, nrow = 2, ncol = 2))

# Access the path 
path(my_goal)
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4

# Change the path
path(my_goal) <- matrix(5:8, nrow = 2, ncol = 2)
path(my_goal)
#>      [,1] [,2]
#> [1,]    5    7
#> [2,]    6    8

```
