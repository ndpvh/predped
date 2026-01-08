# Getter/Setter for the `points`-slot

Works for all extensions of the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).
Note that you can only change the `points` slot for the
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md)
and the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## Usage

``` r
points(object) <- value

# S4 method for class 'polygon'
points(x, ...)

# S4 method for class 'polygon'
points(object) <- value

# S4 method for class 'rectangle'
points(x, ...)

# S4 method for class 'circle'
points(x, length.out = 100, ...)

# S4 method for class 'segment'
points(x, ...)

# S4 method for class 'segment'
points(object) <- value
```

## Arguments

- value:

  Value with which to replace the original value of the `points` slot.

- x, object:

  An instance of the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).
  Note that for this function `x` and `object` are synonymous: The `x`
  argument is included to ensure compatibility with the `points`
  function from base R.

- ...:

  Arguments passed on to the methods of this generic

- length.out:

  Numeric denoting the number of points to create when the provided
  object is of an instance
  [`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md).
  Defaults to `100`

## Details

Note that for the
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
you can only access the `points` slot and not change it. This is because
circles don't have a finite number of points that make up their shape,
and we therefore sample points from the circumference of the circle when
calling the `points` method.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## Examples

``` r
# Initialize a rectangle
my_polygon <- polygon(cbind(c(1, 1, -1, -1), 
                            c(1, -1, -1, 1)))

# Access the points slot
points(my_polygon)
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1   -1
#> [3,]   -1   -1
#> [4,]   -1    1

# Change the points slot
points(my_polygon) <- cbind(c(2, 2, -2, -2), 
                            c(2, -2, -2, 2))
points(my_polygon)
#>      [,1] [,2]
#> [1,]    2    2
#> [2,]    2   -2
#> [3,]   -2   -2
#> [4,]   -2    2

# For circles, we can only access the points slot and provide an argument 
# denoting how many points we want to sample from the circle's 
# circumference
my_circle <- circle(center = c(0, 0), 
                    radius = 1)

points(my_circle, length.out = 10)#' 
#>            [,1]          [,2]
#>  [1,]  1.000000  0.000000e+00
#>  [2,]  0.809017  5.877853e-01
#>  [3,]  0.309017  9.510565e-01
#>  [4,] -0.309017  9.510565e-01
#>  [5,] -0.809017  5.877853e-01
#>  [6,] -1.000000  1.224647e-16
#>  [7,] -0.809017 -5.877853e-01
#>  [8,] -0.309017 -9.510565e-01
#>  [9,]  0.309017 -9.510565e-01
#> [10,]  0.809017 -5.877853e-01
```
