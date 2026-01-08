# Distances to group centroid

Compute the distance of a given agent to the group centroid. This group
centroid is computed as a summary statistic of the predicted x- and y-
coordinates of all pedestrians belonging to the same group as the agent.
The summary statistic of choice should be one of mean-tendency, but can
be specified by the user through the argument `fx`.

## Usage

``` r
distance_group_centroid(predictions, centers, number_agents, fx = mean)
```

## Arguments

- predictions:

  Numeric matrix with shape N x 2 containing predicted positions of all
  pedestrians that belong to the social group of the agent.

- centers:

  Numerical matrix containing the coordinates at each position the
  object can be moved to. Should have one row for each cell.

- number_agents:

  Integer indicating number of people in the pedestrian's social group.

- fx:

  Function used to find the group centroid. Defaults to `mean`

## Value

Numeric vector containing the distance from each cell in the \`center\`
to the group centroid. If not other agents belong to the same group as
the agent, returns `NULL`.

## Details

Note that this function has been defined to be in line with the `m4ma`
utility functions.

## See also

[`gc_utility`](https://github.com/ndpvh/predped/reference/gc_utility.md),
`utility-agent`
