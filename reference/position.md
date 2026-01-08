# Getter/Setter for the `position`-slot

Works for
[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
and
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

## Usage

``` r
position(object, return_matrix = FALSE)

position(object) <- value

# S4 method for class 'polygon'
position(object)

# S4 method for class 'polygon'
position(object) <- value

# S4 method for class 'rectangle'
position(object)

# S4 method for class 'rectangle'
position(object) <- value

# S4 method for class 'circle'
position(object)

# S4 method for class 'circle'
position(object) <- value

# S4 method for class 'segment'
position(object)

# S4 method for class 'segment'
position(object) <- value

# S4 method for class 'goal'
position(object)

# S4 method for class 'goal'
position(object) <- value

# S4 method for class 'agent'
position(object, return_matrix = FALSE)

# S4 method for class 'agent'
position(object) <- value
```

## Arguments

- object:

  An instance of the
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
  or
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

- return_matrix:

  Logical denoting whether to return the position in a matrix of size 1
  x 2. Defaults to `FALSE`.

- value:

  Value with which to replace the original value of the `position` or
  `center` slot.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md),
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md),
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md),
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md),
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md),
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md),
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)

## Examples

``` r
# Initialize all objects for which this getter works
my_agent <- agent(center = c(1, 1), radius = 0.25)
my_circle <- circle(center = c(1, 0), radius = 0.25)
my_goal <- goal(position = c(0, 1))
my_polygon <- polygon(cbind(c(1, 1, -1, -1), c(1, -1, -1, 1)))
my_rectangle <- rectangle(center = c(1, 2), size = c(1, 1))
my_segment <- segment(from = c(0, 0), to = c(2, 2))

# Access the position slot for the different objects
position(my_agent)
#> An object of class "coordinate"
#> [1] 1 1
position(my_circle)
#> An object of class "coordinate"
#> [1] 1 0
position(my_goal)
#> An object of class "coordinate"
#> [1] 0 1
position(my_polygon)
#> An object of class "coordinate"
#> [1] 0 0
position(my_rectangle)
#> An object of class "coordinate"
#> [1] 1 2
position(my_segment)
#> [1] 1 1

# Change the goals slot for the agent
position(my_agent) <- c(0, 0)
position(my_agent)
#> An object of class "coordinate"
#> [1] 0 0

position(my_circle) <- c(0, 0)
position(my_circle)
#> An object of class "coordinate"
#> [1] 0 0

position(my_goal) <- c(0, 0)
position(my_goal)
#> An object of class "coordinate"
#> [1] 0 0

position(my_polygon) <- c(0, 0)
position(my_polygon)
#> An object of class "coordinate"
#> [1] 0 0

position(my_rectangle) <- c(0, 0)
position(my_rectangle)
#> An object of class "coordinate"
#> [1] 0 0

position(my_segment) <- c(0, 0)
position(my_segment)
#> [1] 0 0
```
