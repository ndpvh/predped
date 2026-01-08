# Getter/Setter for the `forbidden`-slot

Works for the all instances of the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
except for the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## Usage

``` r
forbidden(object)

forbidden(object) <- value

# S4 method for class 'polygon'
forbidden(object)

# S4 method for class 'polygon'
forbidden(object) <- value

# S4 method for class 'rectangle'
forbidden(object)

# S4 method for class 'rectangle'
forbidden(object) <- value

# S4 method for class 'circle'
forbidden(object)

# S4 method for class 'circle'
forbidden(object) <- value
```

## Arguments

- object:

  An instance of the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

- value:

  Value with which to replace the original value of the `forbidden`
  slot.

## See also

[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)

## Examples

``` r
# Initialize an object
my_rectangle <- rectangle(center = c(0, 0), 
                          size = c(1, 1), 
                          forbidden = 1)

# Access iteration slot
forbidden(my_rectangle)
#> [1] 1

# Change the iteration slot
forbidden(my_rectangle) <- 2:3
forbidden(my_rectangle)
#> [1] 2 3
```
