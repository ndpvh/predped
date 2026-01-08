# Check whether an Object intersects with Line Segments

Generalization of the
[`intersects`](https://github.com/ndpvh/predped/reference/intersects.md)
for segments, allowing for vectorized checking of intersections with
segments. Is often used within the
[`intersects`](https://github.com/ndpvh/predped/reference/intersects.md),
especially when checking the intersections of
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md)
and
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md).
Currently works for all instances of
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

## Usage

``` r
line_intersection(object, segments, ...)

# S4 method for class 'polygon'
line_intersection(object, segments, return_all = FALSE, cpp = TRUE)

# S4 method for class 'circle'
line_intersection(object, segments, return_all = FALSE, cpp = TRUE)

# S4 method for class 'segment'
line_intersection(object, segments, return_all = FALSE, cpp = TRUE)
```

## Arguments

- object:

  Object of a type that extends
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

- segments:

  Numerical matrix of size N x 4 containing the coordinates of the line
  segments in order x_1, y_1, x_2, y_2.

- ...:

  Arguments passed on to the methods of this generic

- return_all:

  Logical denoting whether to return a vector of logicals that denote
  intersections of each line separately (`TRUE`) or whether to only
  return a single logical denoting whether any intersections occurred
  (`FALSE`). Defaults to `FALSE`.

- cpp:

  Logical denoting whether to use the Rcpp (`TRUE`) or R (`FALSE`)
  alternative of this function. Defaults to `TRUE`.

## Value

Logical vector (`return_all = TRUE`) or logical (`return_all = FALSE`)
denoting whether the lines intersect with the provided object. Whenever
there is an intersection, the logical is `TRUE` and otherwise, the
logical is `FALSE`.

## Details

Note that while
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)
is not explicitly mentioned here, this method does work for this class
of objects.

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
[`intersects`](https://github.com/ndpvh/predped/reference/intersects.md)

## Examples

``` r
# Create two objects that intersect with each other, and check their 
# intersection
my_circle <- circle(center = c(0, 0), radius = 1)
my_lines <- rbind(c(0, 0, 2, 2), 
                  c(0, 2, 2, 2))

line_intersection(my_circle, my_lines)
#> [1] TRUE
```
