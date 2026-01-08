# Getter/Setter for the `weights`-slot

Works for
[`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md).

## Usage

``` r
weights(object, ...)

weights(object) <- value

# S4 method for class 'predped'
weights(object)

# S4 method for class 'predped'
weights(object) <- value
```

## Arguments

- object:

  An instance of the
  [`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md).

- ...:

  Arguments passed to the methods of this generic

- value:

  Value with which to replace the original value of the `weights` slot.

## See also

[`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md)

## Examples

``` r
# Initialize a predped model
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list())

my_model <- predped(setting = my_background, 
                    archetypes = c("BaselineEuropean", 
                                   "DrunkAussie"))

# Access the archetypes slot 
weights(my_model)
#> [1] 0.5 0.5

# Change the archetypes slot
weights(my_model) <- c(0.9, 0.1)
weights(my_model)
#> [1] 0.9 0.1
```
