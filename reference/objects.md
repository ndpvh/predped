# Getter/Setter for the `objects`-slot

Works for
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

## Usage

``` r
objects(object) <- value

# S4 method for class 'background'
objects(name)

# S4 method for class 'background'
objects(object) <- value
```

## Arguments

- value:

  Value with which to replace the original value of the `objects` slot.

- name, object:

  An instance of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).
  Note that `name` and `object` are synonymous for this function: The
  former is only used to ensure compatibility with the `objects`
  function in base R.

## Examples

``` r
# Initialize background
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list(circle(center = c(0, 0), 
                                                  radius = 0.5)),
                            limited_access = list(segment(from = c(-1, -1), 
                                                          to = c(1, 1))), 
                            entrance = c(-1, 0), 
                            exit = c(1, 0)) 

# Access the objects slot for the background
objects(my_background)
#> [[1]]
#> An object of class "circle"
#> Slot "center":
#> An object of class "coordinate"
#> [1] 0 0
#> 
#> Slot "radius":
#> [1] 0.5
#> 
#> Slot "forbidden":
#> <0 x 0 matrix>
#> 
#> Slot "id":
#> [1] "object hpivm"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE
#> 
#> 

# Change the objects slot for the background
#
# Note that the exit is blocked by the new object, which will lead to errors 
# if run in a simulation
objects(my_background) <- list(circle(center = c(1, 0), radius = 0.5))
objects(my_background)
#> [[1]]
#> An object of class "circle"
#> Slot "center":
#> An object of class "coordinate"
#> [1] 1 0
#> 
#> Slot "radius":
#> [1] 0.5
#> 
#> Slot "forbidden":
#> <0 x 0 matrix>
#> 
#> Slot "id":
#> [1] "object nyzbp"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE
#> 
#> 

```
