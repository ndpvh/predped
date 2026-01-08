# An S4 class to Represent Polygon Objects

Polygons can be used to create flexible shapes and are defined through a
set of points. The last point is automatically connected with the first
point.

## Slots

- `points`:

  Numerical matrix with two columns containing the x- and y- coordinates
  of the points that define the polygon.

- `center`:

  Numerical vector containing the position of the center of the polygon.
  For now, the mean of all coordinates in `points` used as an indication
  of the center of the polygon (as the center of a polygon is not easily
  defined).

- `clock_wise`:

  Logical indicating whether the points define the polygon in a
  clockwise (`TRUE`) or counter-clockwise fashion (`FALSE`).

- `forbidden`:

  Numerical vector containing the indices of those edges that cannot be
  used to generate goals on.

- `...`:

  Slots shared with the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
`initialize-polygon`

Other objects:
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)
