# Check whether an Object intersects with Other Object

Currently works for all combinations of instances of
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

## Usage

``` r
intersects(object, other_object, ...)

# S4 method for class 'polygon,polygon'
intersects(object, other_object, cpp = TRUE)

# S4 method for class 'polygon,circle'
intersects(object, other_object, ...)

# S4 method for class 'polygon,segment'
intersects(object, other_object, ...)

# S4 method for class 'circle,polygon'
intersects(object, other_object, cpp = TRUE)

# S4 method for class 'circle,circle'
intersects(object, other_object, ...)

# S4 method for class 'circle,segment'
intersects(object, other_object, ...)

# S4 method for class 'segment,polygon'
intersects(object, other_object, cpp = TRUE)

# S4 method for class 'segment,circle'
intersects(object, other_object, cpp = TRUE)

# S4 method for class 'segment,segment'
intersects(object, other_object, ...)
```

## Arguments

- object:

  Object of
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

- other_object:

  Object of
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  to check intersection with.

- ...:

  Arguments passed on to methods of this generic

- cpp:

  Logical denoting whether to use the Rcpp alternative (`TRUE`) or the R
  alternative of this function (`FALSE`). Defaults to `TRUE`.

## Value

Numerical matrix containing the nodes that were created around/within
the provided object.

## Details

Note that this function is less efficient than the combination of
[`nodes_on_circumference`](https://github.com/ndpvh/predped/reference/nodes_on_circumference.md)
and
[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md),
which is why this combination is used in
[`overlap_with_objects`](https://github.com/ndpvh/predped/reference/overlap_with_objects.md)
instead of the `intersects` method

Note that while
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)
is not explicitly mentioned here, this method does work for this class
of objects.

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
[`in_object`](https://github.com/ndpvh/predped/reference/in_object.md),
[`moving_options`](https://github.com/ndpvh/predped/reference/moving_options.md),
[`nodes_on_circumference`](https://github.com/ndpvh/predped/reference/nodes_on_circumference.md),
[`overlap_with_objects`](https://github.com/ndpvh/predped/reference/overlap_with_objects.md)

## Examples

``` r
# Create two objects that intersect with each other, and check their 
# intersection
my_circle <- circle(center = c(0, 0), radius = 1)
my_rectangle <- rectangle(center = c(0, 0), size = c(0.75, 0.75))

intersects(my_circle, my_rectangle)
#> [1] TRUE

# Create two objects that do not intersect with each other, and check their 
# intersection
my_circle <- circle(center = c(0, 0), radius = 1)
my_rectangle <- rectangle(center = c(0, 0), size = c(1.25, 1.25))

intersects(my_circle, my_rectangle)
#> [1] TRUE
```
