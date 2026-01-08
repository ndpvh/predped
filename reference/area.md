# Calculate the Area of an Object

Currently defined for
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md)
and
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md).

## Usage

``` r
area(object)

# S4 method for class 'rectangle'
area(object)

# S4 method for class 'circle'
area(object)
```

## Arguments

- object:

  Object of
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

## Value

Numeric denoting the area of the object.

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)

## Examples

``` r
# Create a circle
my_circle <- circle(center = c(0, 0), radius = 1)

# Compute the area
area(my_circle)
#> [1] 3.141593
```
