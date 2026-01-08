# An S4 Class to Represent Circle Objects

Special case of the
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
defined through its center and radius.

## Slots

- `center`:

  Numeric vector denoting the center or position of the circle.

- `radius`:

  Numeric denoting the radius of the circle.

- `forbidden`:

  Numerical matrix containing the angles for which you cannot generate
  goals (in radians).

- `...`:

  Slots shared with
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

## See also

[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
`initialize-circle`

Other objects:
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)
