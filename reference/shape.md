# Getter/Setter for the `shape`-slot

Works for
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

## Usage

``` r
shape(object)

shape(object) <- value

# S4 method for class 'background'
shape(object)

# S4 method for class 'background'
shape(object) <- value
```

## Arguments

- object:

  An instance of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- value:

  Value with which to replace the original value of the `shape` slot.

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

# Access the shape slot for the background
shape(my_background)
#> An object of class "rectangle"
#> Slot "center":
#> An object of class "coordinate"
#> [1] 0 0
#> 
#> Slot "size":
#> [1] 2 2
#> 
#> Slot "orientation":
#> [1] 0
#> 
#> Slot "points":
#>      [,1] [,2]
#> [1,]   -1   -1
#> [2,]   -1    1
#> [3,]    1    1
#> [4,]    1   -1
#> 
#> Slot "clock_wise":
#> [1] TRUE
#> 
#> Slot "forbidden":
#> numeric(0)
#> 
#> Slot "id":
#> [1] "object otiti"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE
#> 

# Change the shape slot for the background
shape(my_background) <- circle(center = c(1, 0), radius = 1)
shape(my_background)
#> An object of class "circle"
#> Slot "center":
#> An object of class "coordinate"
#> [1] 1 0
#> 
#> Slot "radius":
#> [1] 1
#> 
#> Slot "forbidden":
#> <0 x 0 matrix>
#> 
#> Slot "id":
#> [1] "object bmxcw"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE
#> 

```
