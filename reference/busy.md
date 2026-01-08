# Getter/Setter for the `busy`-slot

Works for
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## Usage

``` r
busy(object)

busy(object) <- value

# S4 method for class 'goal'
busy(object)

# S4 method for class 'goal'
busy(object) <- value
```

## Arguments

- object:

  An instance of the
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

- value:

  Value with which to replace the original value of the `busy` slot.

## Examples

``` r
# Initialize a goal with a given business status
my_goal <- goal(position = c(0, 0), 
                busy = TRUE)

# Access the business 
busy(my_goal)
#> [1] TRUE

# Change the business
busy(my_goal) <- FALSE
busy(my_goal)
#> [1] FALSE

```
