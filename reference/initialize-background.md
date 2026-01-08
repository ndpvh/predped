# Constructor for the [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)

Constructor for the
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)

## Usage

``` r
# S4 method for class 'background'
initialize(
  .Object,
  shape,
  objects = list(),
  limited_access = list(),
  entrance = NULL,
  exit = NULL,
  same_exit = is.null(exit)
)
```

## Arguments

- .Object:

  For this class, should be left unspecified (see Example).

- shape:

  Object of a type that extends
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  defining the shape of the background. Importantly, the shape cannot be
  of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

- objects:

  List of objects of a type that extends
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  defining the objects that are present in the simulated room (e.g.,
  tables and cabinets). Importantly, objects cannot be of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).
  Defaults to an empty list.

- limited_access:

  List of objects of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)
  which define the routes that can only be taken in one direction (see
  the documentation of
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)).
  Defaults to an empty list.

- entrance:

  Numeric vector or matrix specifying the location(s) of the
  entrance(s), where the first value or column denotes the x-coordinate
  and the second value or column the y-coordinate. Defaults to NULL,
  letting the location of the entrance be randomly generated.

- exit:

  Numeric vector or matrix specifying the location(s) of the exit(s),
  where the first value or column denotes the x-coordinate and the
  second value or column the y-coordinate. Defaults to NULL, letting the
  location of the exit be randomly generated. However, if
  `same_exit = TRUE`, then the location(s) of the exit(s) will be the
  same as the location(s) of the entrance(s).

- same_exit:

  Logical denoting whether the location(s) of the exit(s) should be the
  same as for the entrance(s). Defaults to `TRUE` if no exit is provided
  and to `FALSE` if an exit is provided.

## Value

Object of the
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md),
[`entrance`](https://github.com/ndpvh/predped/reference/entrance.md),
[`exit`](https://github.com/ndpvh/predped/reference/exit.md),
[`limited_access`](https://github.com/ndpvh/predped/reference/limited_access.md),
[`objects`](https://rdrr.io/r/base/ls.html),
[`shape`](https://github.com/ndpvh/predped/reference/shape.md)

## Examples

``` r
# Initialize setting
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)), 
                            objects = list())

# Access the two slots that were specified
my_background@shape
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
#> [1] "object liffi"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE
#> 
my_background@objects
#> list()
```
