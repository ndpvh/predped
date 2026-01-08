# Getter/Setter for the `size`-slot

Works for the
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
and
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## Usage

``` r
size(object)

size(object) <- value

# S4 method for class 'rectangle'
size(object)

# S4 method for class 'rectangle'
size(object) <- value

# S4 method for class 'circle'
size(object)

# S4 method for class 'circle'
size(object) <- value

# S4 method for class 'segment'
size(object)

# S4 method for class 'segment'
size(object) <- value

# S4 method for class 'agent'
size(object)

# S4 method for class 'agent'
size(object) <- value
```

## Arguments

- object:

  An instance of the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  or
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md).

- value:

  Value with which to replace the original value of the `size` slot.

## Details

Note that for
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
this getter outputs the radius (and similarly, the setter changes the
radius).

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## Examples

``` r
# Initialize a circle
my_circle <- circle(center = c(0, 0), 
                    radius = 1)

# Access the radius slot in circle
size(my_circle)
#> [1] 1

# Change the radius slot in circle
size(my_circle) <- 2
size(my_circle)
#> [1] 2

# Also works for objects in which other slots change with a change in size
my_rectangle <- rectangle(center = c(0, 0), 
                          size = c(2, 2))

# Access points before and after changing its size
points(my_rectangle)
#>      [,1] [,2]
#> [1,]   -1   -1
#> [2,]   -1    1
#> [3,]    1    1
#> [4,]    1   -1

size(my_rectangle) <- c(4, 4)
points(my_rectangle) 
#>      [,1] [,2]
#> [1,]   -2   -2
#> [2,]   -2    2
#> [3,]    2    2
#> [4,]    2   -2
```
