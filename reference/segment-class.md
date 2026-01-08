# An S4 class to Represent Lines

Lines are used to determine intersections and to prevent pedestrians
from walking certain ways.

## Details

It is important to realize exactly how it is determined whether agents
can move through a segment or not before constructing or using these
segments in your environment. For this, consider the following logic.

Consider the `from` slot to be the center of a circle. Furthermore
consider the radius of this circle to be equal to the `size` slot, and
the `orientation` of the segment to be determined by the positions of
the `from` and `to` slots, so that we look at the orientation of the
location of `to` relative to the location of `from`. This is how the
`segment-class` is constructed, and is an important starting point for
determining when pedestrians can and cannot move through a segment.

Consider another point in the Euclidian space, so that it has a position
P = (x, y). Now we compute the relative angle of this coordinate within
the circle that is defined by the `from` and `to` slots. In other words,
we compute the absolute angle of coordinate P relative to `from` – let's
call this \\\alpha\\ – and then subtract the orientation of the line
from this to get a relative angle within the circle defined by the
segment – let's call the result \\\beta = \alpha - orientation\\. We
then block an agent whenever \\\beta \in (0, pi)\\, and let him pass
through whenever \\\beta \in \[pi, 2 \* pi\]\\.

To gain some intuition for this: If you create a segment that is
vertical, where `from` has a lower y-coordinate than `to` (e.g.,
`from = c(0, 0)` and `to = c(0, 1)`), then agents will be blocked
whenever they find themselves to the left of this segment, while they
will be able to pass through the segment whenever they are to the right
of this segment.

## Slots

- `from`:

  Numeric vector denoting where the segment begins.

- `to`:

  Numeric vector denoting where the segment ends.

- `center`:

  Numeric vector denoting the center of the segment.

- `size`:

  Numeric denoting the length of the segment.

- `orientation`:

  Numeric denoting the orientation of the segment in radians.

- `...`:

  Slots that are inherited from
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)

## See also

[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
`initialize-segment`,
[`limit_access`](https://github.com/ndpvh/predped/reference/limit_access.md)
