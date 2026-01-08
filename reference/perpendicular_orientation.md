# Find the perpendicular orientation

Finds the perpendicular or tangential orientation of a line starting
from a point on an edge of the background's shape and going inwards. The
point on the edge is chosen to be the entrance to the space. Is used to
find in what orientation the agents should be heading when entering the
space.

## Usage

``` r
perpendicular_orientation(object, co)
```

## Arguments

- object:

  An object of class object

- co:

  A vector of size 2 containing x and y coordinates for a location from
  which to deduce the perpendicular orientation

## Value

Numeric denoting the perpendicular orientation to the entrance of the
space in degrees
