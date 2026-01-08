# Getter/Setter for the `to`-slot

Works for the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## Usage

``` r
to(object, ...)

to(object) <- value

# S4 method for class 'segment'
to(object, ...)

# S4 method for class 'segment'
to(object) <- value
```

## Arguments

- object:

  An instance of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

- ...:

  Arguments passed on to the methods of this generic

- value:

  Value with which to replace the original value of the `to` slot.

## See also

[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## Examples

``` r
# Create a segment
my_segment <- segment(from = c(0, 0), to = c(1, 1))

# Access the to slot
to(my_segment)
#> [1] 1 1

# Change the to slot
to(my_segment) <- c(4, 4)
to(my_segment)
#> [1] 4 4

# Note that changing this slot also changes the orientation and size of the 
# line
orientation(my_segment)
#> [1] 0.7853982
size(my_segment)
#> [1] 5.656854
```
