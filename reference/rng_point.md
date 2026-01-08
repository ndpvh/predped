# Sample a Random Point on the Circumference

Currently works for all instances of
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

## Usage

``` r
rng_point(object, middle_edge = TRUE)

# S4 method for class 'polygon'
rng_point(object, middle_edge = TRUE)

# S4 method for class 'circle'
rng_point(object, middle_edge = TRUE)

# S4 method for class 'segment'
rng_point(object, middle_edge = TRUE)
```

## Arguments

- object:

  Object of
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

- middle_edge:

  Logical denoting whether the point should lie in the middle of a
  random edge. Ignored for circles. Defaults to `TRUE`.

## Value

Numerical vector denoting a coordinate on the circumference of the
provided object.

## Details

Note that while
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)
is not explicitly mentioned here, this method does work for this class
of objects.

Furthermore note that `forbidden` is ignored for the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
[`add_nodes`](https://github.com/ndpvh/predped/reference/add_nodes.md),
[`nodes_on_circumference`](https://github.com/ndpvh/predped/reference/nodes_on_circumference.md)

## Examples

``` r
# Create an object
my_circle <- circle(center = c(0, 0), 
                    radius = 1)
rng_point(my_circle)
#> [1] -0.1443180  0.9895314

# Generate a point on the circumference of the circle with limitations, so 
# that it cannot lie between the angles (0, pi/2) and (pi, 3 * pi/2), 
# meaning the coordinate cannot have both positive or both negative values 
# in its coordinates (one always has to be positive, the other negative).
my_circle <- circle(center = c(0, 0), 
                    radius = 1,
                    forbidden = rbind(c(0, pi/2), 
                                      c(pi, 3 * pi/2)))
rng_point(my_circle)
#> [1]  0.1086277 -0.9940825
```
