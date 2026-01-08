# An S4 Class to Represent the Background

Defines the `background` class, which contains all characteristics of
the setting in which agents will be simulated to walk. Currently
consists of the shape of the simulated room and objects that are
contained within the room.

## Slots

- `shape`:

  Object of a type that extends
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  defining the shape of the background. Importantly, the shape cannot be
  of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

- `objects`:

  List of objects of a type that extends
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  defining the objects that are present in the simulated room (e.g.,
  tables and cabinets). Importantly, objects cannot be of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

- `limited_access`:

  List of objects of the
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)
  which define the routes that can only be taken in one direction (see
  the documentation of
  [`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)).

- `precomputed_limited_access`:

  List of non-penetrable objects based on the values of slot
  `limited_access`. Is not meant to be changed by the user, but is used
  as a slot to significantly speed up computations.

- `entrance`:

  Numeric matrix specifying the location(s) of the entrance(s), where
  the first column denotes the x-coordinate and the second column the
  y-coordinate.

- `exit`:

  Numeric matrix specifying the location(s) of the exit(s), where the
  first column denotes the x-coordinate and the second column the
  y-coordinate.

## See also

[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
[`entrance`](https://github.com/ndpvh/predped/reference/entrance.md),
[`exit`](https://github.com/ndpvh/predped/reference/exit.md),
[`limited_access`](https://github.com/ndpvh/predped/reference/limited_access.md),
[`objects`](https://rdrr.io/r/base/ls.html),
[`shape`](https://github.com/ndpvh/predped/reference/shape.md),
`initialize-background`,
