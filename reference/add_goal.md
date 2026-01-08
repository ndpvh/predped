# Add a goal to an object

Uses the characteristics of an object to add a goal to it.

## Usage

``` r
add_goal(object, ...)

# S4 method for class 'object'
add_goal(object, background, id = character(0), counter = 5, ...)
```

## Arguments

- object:

  Object or instance of the
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md).

- ...:

  Arguments passed on to
  [`rng_point`](https://github.com/ndpvh/predped/reference/rng_point.md)

- background:

  Object of
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
  containing all objects in the environment that can have a goal
  attached to them in its `objects` slot.

- id:

  Character that serves as an identifier for the goal. Defaults to an
  empty character, triggering the random generation of an id.

- counter:

  Numeric denoting the number of time steps the agent should interact
  with the goal before the goal has been completed. Defaults to `5`.

## Value

Object of
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md).

## Details

Takes in an instance of
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
and uses its characteristics to generate an instance of
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
that is attached to it. In practice, uses one of the objects in the
`objects` slot of a
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
instance.

Is defined for all instances of
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
except for
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md),
which cannot contain goals due to its primary function.

## See also

[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
[`circle-class`](https://github.com/ndpvh/predped/reference/circle-class.md)
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
[`polygon-class`](https://github.com/ndpvh/predped/reference/polygon-class.md)
[`rectangle-class`](https://github.com/ndpvh/predped/reference/rectangle-class.md)
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md)
[`rng_point`](https://github.com/ndpvh/predped/reference/rng_point.md)

## Examples

``` r
# Create a background
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)))

# Adjust the objects in the background for each of the different objects to 
# showcase how add_goals works for each
objects(my_background) <- list(circle(center = c(0, 0), radius = 0.5))
add_goal(my_background@objects[[1]], 
         my_background)
#> Goal Attributes 
#> busy: FALSE 
#> counter: 5 
#> done: FALSE 
#> id: goal qmfaa 
#> path:
#>      [,1] [,2]
#> 
#> position: 0.3893627 -0.3293884 
#> 
#> For more detailed information, please extract the wanted information from the background directly.

objects(my_background) <- list(rectangle(center = c(0, 0), size = c(1, 1)))
add_goal(my_background@objects[[1]], 
         my_background)
#> Goal Attributes 
#> busy: FALSE 
#> counter: 5 
#> done: FALSE 
#> id: goal dgwxb 
#> path:
#>      [,1] [,2]
#> 
#> position: -0.5070711 0 
#> 
#> For more detailed information, please extract the wanted information from the background directly.

objects(my_background) <- list(polygon(points = cbind(c(0.5, 0.5, -0.5, -0.5), 
                                                      c(0.5, -0.5, -0.5, 0.5))))
add_goal(my_background@objects[[1]], 
         my_background)
#> Goal Attributes 
#> busy: FALSE 
#> counter: 5 
#> done: FALSE 
#> id: goal myucm 
#> path:
#>      [,1] [,2]
#> 
#> position: 0.5070711 0 
#> 
#> For more detailed information, please extract the wanted information from the background directly.
```
