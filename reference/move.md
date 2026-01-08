# Move object to new location

Move an object to a new coordinate. Currently works for
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
and
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## Usage

``` r
move(object, ...)

# S4 method for class 'polygon'
move(object, coord)

# S4 method for class 'rectangle'
move(object, coord)

# S4 method for class 'circle'
move(object, coord)

# S4 method for class 'segment'
move(object, coord)
```

## Arguments

- object:

  Object of
  [`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
  [`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md),
  [`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
  [`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
  or,
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

- ...:

  Arguments passed on to the methods of this generic

- coord:

  Numeric denoting the location to which the object should be moved.

## Value

Object of the same class as the one provided.

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`coordinate-class`](https://github.com/ndpvh/predped/reference/coordinate-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
`center<-`, `position<-`

## Examples

``` r
# Let's create an object
my_circle <- circle(center = c(0, 0), radius = 1)
my_circle@center 
#> An object of class "coordinate"
#> [1] 0 0

# Move the object to another location
moved_circle <- move(my_circle, coord = c(1, 1))
#> Warning: Object is not moveable. Keeping position unchanged.
moved_circle
#> An object of class "circle"
#> Slot "center":
#> An object of class "coordinate"
#> [1] 0 0
#> 
#> Slot "radius":
#> [1] 1
#> 
#> Slot "forbidden":
#> <0 x 0 matrix>
#> 
#> Slot "id":
#> [1] "object mgmag"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE
#> 

# Let's also do the same for a rectangle, showing in the process that moving
# a rectangle also changes its points
my_rectangle <- rectangle(center = c(0, 0), size = c(2, 2))
my_rectangle@points 
#>      [,1] [,2]
#> [1,]   -1   -1
#> [2,]   -1    1
#> [3,]    1    1
#> [4,]    1   -1

moved_rectangle <- move(my_rectangle, coord = c(1, 1))
#> Warning: Object is not moveable. Keeping position unchanged.
moved_rectangle@points
#>      [,1] [,2]
#> [1,]   -1   -1
#> [2,]   -1    1
#> [3,]    1    1
#> [4,]    1   -1
```
