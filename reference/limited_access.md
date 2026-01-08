# Getter/Setter for the `limited_access`-slot

Works for
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

## Usage

``` r
limited_access(object)

limited_access(object) <- value

# S4 method for class 'background'
limited_access(object)

# S4 method for class 'background'
limited_access(object) <- value
```

## Arguments

- object:

  An instance of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- value:

  Value with which to replace the original value of the `limited_access`
  slot.

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)

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

# Access the limited_access slot for the background
limited_access(my_background)
#> [[1]]
#> An object of class "segment"
#> Slot "from":
#> [1] -1 -1
#> 
#> Slot "to":
#> [1] 1 1
#> 
#> Slot "center":
#> [1] 0 0
#> 
#> Slot "size":
#> [1] 2.828427
#> 
#> Slot "orientation":
#> [1] 0.7853982
#> 
#> Slot "id":
#> [1] "object nvtqw"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE
#> 
#> 

# Change the limited_access slot for the background
limited_access(my_background) <- list(segment(from = c(-1, 1), to = c(1, -1)))
limited_access(my_background)
#> [[1]]
#> An object of class "segment"
#> Slot "from":
#> [1] -1  1
#> 
#> Slot "to":
#> [1]  1 -1
#> 
#> Slot "center":
#> [1] 0 0
#> 
#> Slot "size":
#> [1] 2.828427
#> 
#> Slot "orientation":
#> [1] -0.7853982
#> 
#> Slot "id":
#> [1] "object hdooq"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE
#> 
#> 
```
