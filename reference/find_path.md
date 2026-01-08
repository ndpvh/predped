# Find path to a goal

Creates a numerical matrix of coordinates which contain the path points
that the agent will use to move towards a goal. It thus finds itself on
the strategic level (when performed in an initial planning phase) and
the tactical level (when performed in response to blockages).

## Usage

``` r
find_path(object, ...)

# S4 method for class 'goal'
find_path(
  object,
  agent,
  background,
  space_between = radius(agent),
  many_nodes = FALSE,
  algorithm = "Dijkstra",
  precomputed_edges = NULL,
  new_objects = NULL,
  reevaluate = FALSE
)
```

## Arguments

- object:

  Object of
  [`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
  for which the path should be computed.

- ...:

  Arguments passed on to the methods of this generic

- agent:

  Object of
  [`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)
  who wants to move towards the goal defined in argument `object`.

- background:

  Object of
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
  within which the goal and agent are contained.

- space_between:

  Numeric denoting the amount of space to leave between an object and a
  path point. Within this function, the default is the radius of the
  agent that is provided. However, in the
  [`simulate`](https://rdrr.io/r/stats/simulate.html) function, this
  default is overwritten to be equal to `2.5` times this radius.

- many_nodes:

  Logical denoting whether to create many nodes or leave it at the
  minimum. Defaults to `FALSE`.

- algorithm:

  Character denoting the algorithm to be used for optimizing the path
  towards the goal. Is provided to the
  [`get_path_pair`](https://rdrr.io/pkg/cppRouting/man/get_path_pair.html)
  function in the `cppRouting` package. Defaults to `"Dijkstra"`. Note
  that one-way routing as controlled by the `limited_access` slot in the
  `background` may not work if a bidirectional algorithm is used.

- precomputed_edges:

  List containing the network of nodes and edges for the environment, as
  created by
  [`compute_edges`](https://github.com/ndpvh/predped/reference/compute_edges.md).
  Defaults to `NULL`, triggering the creation of these edges again.

- new_objects:

  List of instances of
  [`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
  that are not yet contained in the `objects` slot in the argument
  `background`. Usually consists of other agents that the `agent` has to
  account for when planning their route. Defaults to `NULL`.

- reevaluate:

  Logical denoting whether to reevaluate the network that is provided in
  `precomputed_edges`. Is useful whenever `new_objects` is not `NULL`,
  allowing us to check whether some nodes and edges are now occluded by
  the new objects. Defaults to `FALSE`.

## Value

Numerical matrix of coordinates representing the path points the agent
will have to move to to reach the goal.

## Details

Creates a numerical matrix containing coordinates of path points which,
together, form a strategic path to the goal specified in this function.
This forms one of the functions that operate on the strategic and
tactical level of the model.

To create this path, the function takes the following approach. First,
it will create a network of nodes that are connected through
unidirectional edges, as created by the
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md)
function. Nodes are created based on the objects in the environment, so
that the nodes are always strategically placed at a certain distance
away from the objects in the environment (at a distance
`space_between`). If the argument `many_nodes = TRUE`, then a grid of
other nodes are added to this list of initial nodes. This grid consists
of nodes that are placed at an equal distance apart in the x- and
y-direction, and spans 20 rows and 20 columns. Once the nodes have been
generated, those nodes that fall within any of the objects in the
environment or falls outside of the environment are deleted.
Unidirectional edges connect the different nodes and are similarly
pruned based on intersections with objects or, when defined,
intersections with instances of the
[`segment-class`](https://github.com/ndpvh/predped/reference/segment-class.md).

Once the network of nodes and edges has been created, we translate this
network into a graph that can be used by the `ccpRouting` package.
Specifically, we use
[`makegraph`](https://rdrr.io/pkg/cppRouting/man/makegraph.html) for
this purpose. We then use the
[`get_path_pair`](https://rdrr.io/pkg/cppRouting/man/get_path_pair.html)
function of the `cppRouting` package to find the shortest route to the
goal. The user can specify which algorithm to use for this optimization
through the argument `algorithm`, but the algorithm should be
unidirectional to ensure that one-way walking can be enforced (as
controlled through the `limited_access` slot in the
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)).

Finally, the optimal path is checked for any redundancy and formatted so
that the path points are contained in an n x 2 matrix.

## See also

[`agent-class`](https://github.com/ndpvh/predped/reference/agent-class.md)
[`background-class`](https://github.com/ndpvh/predped/reference/background-class.md)
[`goal-class`](https://github.com/ndpvh/predped/reference/goal-class.md)
[`object-class`](https://github.com/ndpvh/predped/reference/object-class.md)
[`simulate`](https://rdrr.io/r/stats/simulate.html)
[`adjust_edges`](https://github.com/ndpvh/predped/reference/adjust_edges.md)
[`compute_edges`](https://github.com/ndpvh/predped/reference/compute_edges.md)
[`create_edges`](https://github.com/ndpvh/predped/reference/create_edges.md)

## Examples

``` r
# Create a setting
my_background <- background(shape = rectangle(center = c(0, 0), 
                                              size = c(2, 2)),
                            objects = list(circle(center = c(0, 0), 
                                                  radius = 0.5)))

# Create an agent that is walking around there and a goal that the agent 
# will move to
my_goal <- add_goal(objects(my_background)[[1]], my_background)
my_agent <- agent(center = c(0.7, 0.7), radius = 0.25)

# Find the path of the agent to his goal
find_path(my_goal, 
          my_agent,
          my_background)
#>              x         y
#> [1,] -0.205821 0.4666237
```
