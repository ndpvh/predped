# Getting Started

The `predped` package serves as a coding interface to use the Minds for
Mobile Agents pedestrian model, hereafter abbreviated as M4MA. On this
page, we will focus on how to install of `predped` and on its underlying
logic. For the theoretical background of `predped` and details on the
M4MA, we refer the interested reader to the vignette [*Theoretical
Background*](https://ndpvh.github.io/predped/articles/theory.html). For
information on how to run simulations, we refer readers to the vignette
[*Simulations*](https://ndpvh.github.io/predped/articles/simulation.html)
for the basic workflow and to the vignette [*Advanced
Simulations*](https://ndpvh.github.io/predped/articles/advanced_simulation.html)
for some additional functionalities provided within `predped`.
Information on how to estimate the parameters of an M4MA model from
movement data can be found in the vignette
[*Estimation*](https://ndpvh.github.io/predped/articles/estimation.html).

## Installation

To install `predped`, you should use the `install_github()` function
from the package `remotes`:

``` r

remotes::install_github("ndpvh/predped")
```

One can load the package through
[`library()`](https://rdrr.io/r/base/library.html) to make use of the
package’s functionalities:

``` r

library(predped)
```

## Implementation

The package has been designed to allow a great deal of flexibility on
the side of the user while ensuring stability of the package at the
lower levels. To achieve this balance, we make use of object-oriented
programming, specifically leveraging on the functionality of the *S4*
class native to R. In this section, we will discuss what this choice
implies within the context of `predped` and refer the interested reader
to Wickham (2019) for more information on object-oriented programming
and S4 classes more generally.

### Object-oriented programming

For users, the S4 class serves as an *object* that contains a specific
(fixed) set of *attributes* or characteristics. For example, a room may
be treated as an object, containing a particular shape and a set of
objects that are inside of it (e.g., a living room may consist of a
couch, some closets, and a coffee table). `predped` allows for the
definition of several shape types, namely
[`rectangle`](https://ndpvh.github.io/predped/reference/rectangle-class.html)s,
[`polygon`](https://ndpvh.github.io/predped/reference/polygon-class.html)s,
and
[`circle`](https://ndpvh.github.io/predped/reference/circle-class.html)s,
which can be created by calling their constructor and defining their
characteristics:

``` r

# Main characteristics of rectangle are `center` and `size`
rectangle(
  center = c(0, 0),
  size = c(2, 4)
)
#> An object of class "rectangle"
#> Slot "center":
#> An object of class "coordinate"
#> [1] 0 0
#> 
#> Slot "size":
#> [1] 2 4
#> 
#> Slot "orientation":
#> [1] 0
#> 
#> Slot "points":
#>      [,1] [,2]
#> [1,]   -1   -2
#> [2,]   -1    2
#> [3,]    1    2
#> [4,]    1   -2
#> 
#> Slot "clock_wise":
#> [1] TRUE
#> 
#> Slot "forbidden":
#> numeric(0)
#> 
#> Slot "id":
#> [1] "object mwlef"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE

# Main characteristics of polygon are `points` connecting the outline
polygon(
  points = rbind(
    c(0, 1),
    c(1, 0),
    c(-1, 0)
  )
)
#> An object of class "polygon"
#> Slot "points":
#>      [,1] [,2]
#> [1,]    0    1
#> [2,]    1    0
#> [3,]   -1    0
#> 
#> Slot "center":
#> An object of class "coordinate"
#> [1] 0.0 0.5
#> 
#> Slot "clock_wise":
#> [1] TRUE
#> 
#> Slot "forbidden":
#> numeric(0)
#> 
#> Slot "id":
#> [1] "object oodfi"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE

# Main characteristics of circle are `center` and `radius`
circle(
  center = c(0, 0),
  radius = 1
)
#> An object of class "circle"
#> Slot "center":
#> An object of class "coordinate"
#> [1] 0 0
#> 
#> Slot "radius":
#> [1] 1
#> 
#> Slot "forbidden":
#> <0 x 0 matrix>
#> 
#> Slot "id":
#> [1] "object eexom"
#> 
#> Slot "moveable":
#> [1] FALSE
#> 
#> Slot "interactable":
#> [1] TRUE
```

Importantly, the real strength of the S4 class is not only in its
attributes, but also in the creation of *methods*, functions of which
their underlying computations differ depending on the object’s class.
Take, for example, the computation of the area of different shapes. For
rectangles, the area can be computed as:

``` math
\begin{equation}
  A = w h
\end{equation}
```
where $`A`$ represents the area and $`w`$, $`h`$ are the width and the
height of the rectangle respectively. For circles, the area is defined
differently, where:

``` math
\begin{equation}
  A = \pi r^2
\end{equation}
```
where $`r`$ is the radius of the circle. Traditionally, to differentiate
between both use-cases, one would have to implement two different
functions, for example `area_rectangle` and `area_circle`. When making
use of object-oriented programming, however, we can create a separate
method for both use-cases, preserving the same name `area` in both
cases:

``` r

# Create the objects for which to compute the area
my_rectangle <- rectangle(
  center = c(0, 0),
  size = c(2, 4)
)
my_circle <- circle(
  center = c(0, 0),
  radius = 0.5
)

# Compute the area
area(my_rectangle)
#> [1] 8
area(my_circle)
#> [1] 0.7853982
```

While S4 classes have several advantages, they do imply some
inflexibility in their definition, as each instance of a class needs to
have a fixed set of attributes. To find out what attributes each class
has, we refer the reader to the documentation site or ask them to use
the helper function `?` to call the documentation page of the function
of interest.

### Getters and setters

Getters and setters are a special type of method that try to either
retrieve (“get”) or change (“set”) an attribute of an object. For
example, the getter/setter
[`size`](https://ndpvh.github.io/predped/reference/size.html) can be
used to either retrieve or overwrite the size of the object provided to
it. Exemplified with the rectangle we made earlier:

``` r

size(my_rectangle)
#> [1] 2 4

size(my_rectangle) <- c(1, 1)
size(my_rectangle)
#> [1] 1 1
```

The getters/setters defined in `predped` serve two purposes. First, they
make it easier for users to work with the objects provided by the
package. Second, they ensure that if an object’s attribute is changed,
that all attributes that depend on this change are changed as well. For
example, the coordinates of a rectangles corners depend on both the size
of the rectangle and its center. Changing one of the two thus changes
these coordinates. To ensure that this happens, we need to use the
setters rather than overwriting the attribute manually:

``` r

# Create a rectangle with a given size
my_rectangle <- rectangle(
  center = c(0, 0), 
  size = c(2, 2)
)
points(my_rectangle)
#>      [,1] [,2]
#> [1,]   -1   -1
#> [2,]   -1    1
#> [3,]    1    1
#> [4,]    1   -1

# Change the attribute size directly: Does not change the points attribute
my_rectangle@size <- c(4, 4)
points(my_rectangle)
#>      [,1] [,2]
#> [1,]   -1   -1
#> [2,]   -1    1
#> [3,]    1    1
#> [4,]    1   -1

# Change the attribute through the setter: Changes the points attribute
size(my_rectangle) <- c(4, 4)
points(my_rectangle)
#>      [,1] [,2]
#> [1,]   -2   -2
#> [2,]   -2    2
#> [3,]    2    2
#> [4,]    2   -2
```

This property of the setters make them the preferred way of changing
attributes of an object.

### Classes in `predped`

In this section, we briefly summarize the most important classes in
`predped`, their most important attributes, and their purpose within the
package.

Objects in space:

- [`object`](https://ndpvh.github.io/predped/reference/object.html):
  Defines geometric objects that exist in space and on which most of the
  computations are based. Encompasses
  [`rectangle`](https://ndpvh.github.io/predped/reference/rectangle.html),
  [`polygon`](https://ndpvh.github.io/predped/reference/polygon.html),
  [`circle`](https://ndpvh.github.io/predped/reference/circle.html),
  [`segment`](https://ndpvh.github.io/predped/reference/segment.html),
  and [`agent`](https://ndpvh.github.io/predped/reference/agent.html),
  meaning these classes share the attributes with `object`;
  - `id`: Character denoting the identifier of the object. Typically
    randomly assigned;
  - `interactable`: Logical denoting whether agents can interact with
    this object;
- [`rectangle`](https://ndpvh.github.io/predped/reference/rectangle.html):
  An object of a rectangular shape;
  - `center`: Numeric vector with two values defining the center of the
    rectangle;
  - `size`: Numeric vector of two values defining the width and height
    of the rectangle;
  - `orientation`: Numeric denoting the direct the agent is facing in
    radians;
  - `points`: Numeric matrix containing the coordinates of the corners
    of the rectangle in clockwise fashion;
- [`polygon`](https://ndpvh.github.io/predped/reference/polygon.html):
  An object of a regular or irregular shape defined by the coordinates
  of its corners;
  - `points`: Numeric matrix containing the coordinates of the corners
    of the rectangle in clockwise fashion;
- [`circle`](https://ndpvh.github.io/predped/reference/circle.html): An
  object of a circular shape;
  - `center`: Numeric vector with two values defining the center of the
    rectangle;
  - `radius`: Numeric denoting the radius of the circle;
- [`segment`](https://ndpvh.github.io/predped/reference/segment.html):
  An object defining a line in space that can be used to induce
  one-directional motion (see [*Advanced
  Simulations*](https://ndpvh.github.io/predped/articles/advanced_simulation.html));
  - `from`: Numeric vector with two values defining the coordinate at
    which the segment starts;
  - `to`: Numeric vector with two values defining the coordinate at
    which the segments ends;
- [`agent`](https://ndpvh.github.io/predped/reference/agent.html): An
  agent that walks around according to the principles of the M4MA. In
  `predped`, agents are circular, meaning they share some of their
  attributes with
  [`circle`](https://ndpvh.github.io/predped/reference/circle.html);
  - `speed`: Numeric denoting the speed of the agent;
  - `orientation`: Numeric denoting the orientation of the agent in
    degrees;
  - `status`: Character denoting what the agent is doing (e.g., move,
    plan etc.) ;
  - `current_goal`, `goals`: An instance of the
    [`goal`](https://ndpvh.github.io/predped/reference/goal.html) class
    and a collection of these respectively, denoting what the agent
    wants to achieve in the environment;
  - `parameters`: A `data.frame` containing the parameter values for
    this agent.

Space itself:

- [`background`](https://ndpvh.github.io/predped/reference/background.html):
  The setting in which the agents walk around;
  - `shape`: An instance of the
    [`object`](https://ndpvh.github.io/predped/reference/object.html)
    class that defines the shape of the setting;
  - `objects`: A list of
    [`object`](https://ndpvh.github.io/predped/reference/object.html)s
    defining the objects in the setting;
  - `entrance`, `exit`: Numeric matrices defining the entrances and/or
    exits of the space;
- [`goal`](https://ndpvh.github.io/predped/reference/goal.html): A goal
  that the agent wants to achieve within the setting;
  - `id`: Character denoting the identifier of the goal;
  - `position`: Numeric vector denoting the location of the goal;
  - `path`: Numeric matrix denoting the path the agent has to take to
    get to the goal;
  - `counter`: Numeric denoting how many iterations the agent has to
    stay at the goal to achieve it.

Others:

- [`predped`](https://ndpvh.github.io/predped/reference/background.html):
  Relates a particular set of agent characteristics to a setting, to be
  used for a simulation;
  - `id`: Character denoting the identifier of the `predped` model;
  - `setting`: An instance of the
    [`background`](https://ndpvh.github.io/predped/reference/background.html)
    class denoting the setting agents have to walk in;
  - `archetypes`, `weights`: Character vector denoting which agent
    archetypes (i.e., a type of agent defined by a set of parameters)
    are expected to walk into the setting, and with what probability the
    next is of that type;
- [`state`](https://ndpvh.github.io/predped/reference/state.html):
  Defines one particular iteration of the simulation;
  - `iteration`: Numeric denoting the iteration number;
  - `setting`: An instance of the
    [`background`](https://ndpvh.github.io/predped/reference/background.html)
    class denoting the setting agents were walking in at this iteration;
  - `agents`: A list of
    [`agent`](https://ndpvh.github.io/predped/reference/agent.html)s
    that walked around in the setting at this iteration.
- [`trace`](https://ndpvh.github.io/predped/reference/trace.html):
  Defines a particular set of states that follow each other one after
  the other;
  - `id`: Character denoting the identifier of the trace;
  - `time_step`: Numeric denoting the time that elapses between
    iterations in seconds;
  - `setting`: An instance of the
    [`background`](https://ndpvh.github.io/predped/reference/background.html)
    class denoting the setting agents have to walk in;
  - `states`: An ordered list of lists of
    [`agent`](https://ndpvh.github.io/predped/reference/agent.html)s
    that walked around in the setting at each iteration;
  - `variables`: An ordered list of named lists that contain values for
    variables at each iteration, only relevant when specifying the `fx`
    argument in
    [`simulate`](https://ndpvh.github.io/predped/reference/simulate.html).

## References

Wickham, H. (2019). *Advanced R, 2nd edition.* CRC Press. Available
online at <https://adv-r.hadley.nz/>
