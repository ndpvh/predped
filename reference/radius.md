# Getter/Setter for the `radius`-slot

Works for the
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md).

## Usage

``` r
radius(object)

radius(object) <- value

# S4 method for class 'circle'
radius(object)

# S4 method for class 'circle'
radius(object) <- value
```

## Arguments

- object:

  An instance of the
  [`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md).

- value:

  Value with which to replace the original value of the `radius` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md)

## Examples

``` r
# Initialize a circle
my_circle <- circle(center = c(0, 0), 
                    radius = 1)

# Access the radius slot in circle
radius(my_circle)
#> [1] 1

# Change the radius slot in circle
radius(my_circle) <- 2
radius(my_circle)
#> [1] 2
```
