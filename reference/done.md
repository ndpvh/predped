# Getter/Setter for the `done`-slot

Works for
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## Usage

``` r
done(object)

done(object) <- value

# S4 method for class 'goal'
done(object)

# S4 method for class 'goal'
done(object) <- value
```

## Arguments

- object:

  An instance of the
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

- value:

  Value with which to replace the original value of the `done` slot.

## Examples

``` r
# Initialize a goal with a given done argument
my_goal <- goal(position = c(0, 0), 
                done = FALSE)

# Access the done status
done(my_goal)
#> [1] FALSE

# Change the done status
done(my_goal) <- TRUE
done(my_goal)
#> [1] TRUE

```
