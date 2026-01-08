# Getter/Setter for the `from`-slot

Works for the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## Usage

``` r
from(object, ...)

from(object) <- value

# S4 method for class 'segment'
from(object, ...)

# S4 method for class 'segment'
from(object) <- value
```

## Arguments

- object:

  An instance of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

- ...:

  Arguments passed on to the methods of this generic.

- value:

  Value with which to replace the original value of the `from` slot.

## See also

[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## Examples

``` r
# Create a segment
my_segment <- segment(from = c(0, 0), to = c(1, 1))

# Access the from slot
from(my_segment)
#> [1] 0 0

# Change the from slot
from(my_segment) <- c(4, 4)
from(my_segment)
#> [1] 4 4

# Note that changing this slot also changes the orientation and size of the 
# line
orientation(my_segment)
#> [1] 0.7853982
size(my_segment)
#> [1] 4.242641
```
