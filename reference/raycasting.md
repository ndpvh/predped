# Raycasting algorithm

This algorithm checks whether a point lies within an arbitrary polygon
by checking the even-odd-rule, which says that for any point (x,y) that
lies within a polygon, the number of times it cross the boundaries of
this polygon when x goes to infinity should be uneven/odd.

## Usage

``` r
raycasting(coords, x)
```

## Arguments

- coords:

  Numerical matrix of size N x 2 containing the coordinates of the
  corners of the object.

- x:

  Numerical matrix of size M x 2 containing the coordinates of the
  coordinates of which should be checked whether they are inside of the
  object.

## Value

Logical vector denoting whether each point in `x` lies within (`TRUE`)
or outside (`FALSE`) of the object.

## Details

While this may not seem like the most efficient algorithm, it is quite
fast for the typical objects used in predped.

## See also

[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md),
[`out_object`](https://github.com/ndpvh/predped/reference/out_object.md)

## Examples

``` r
# Create a set of points that come from a rectangle
my_rectangle <- rectangle(center = c(0, 0), size = c(2, 2))
points <- my_rectangle@points 

# Create a set of points that fall inside of or outside of the rectangle
coords <- rbind(c(0, 0), 
                c(2, 0))

# Check where they lie with the raycasting algorithm
raycasting(points, coords)
#> [1]  TRUE FALSE
```
