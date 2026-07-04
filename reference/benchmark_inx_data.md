# Initial condition used in benchmarks (data)

This data.frame contains an initial condition that is used in the
benchmarks.

## Usage

``` r
benchmark_inx_data
```

## Format

A data frame with 70 rows and 30 variables:

- `iteration`:

  Integer denoting the iteration number

- `time`:

  Numeric denoting the time of the measurement

- `id`:

  Character denoting the identifier of an agent

- `x`, `y`:

  Numeric denoting the position of the agent

- `speed`:

  Numeric denoting the speed of the agent

- `orientation`:

  Numeric denoting the orientation of the agent

- `cell`:

  Integer denoting the cell the agent moved to on this iteration

- `group`:

  Numeric denoting group the agent belongs to

- `status`:

  Character denoting what the agent was doing on this iteration

- `goal_id`:

  Character denoting the identifier of the agent's current goal

- `goal_x`, `goal_y`:

  Numeric denoting the position of the agent's current goal

- `radius`:

  Numeric denoting agent's size

- `agent_idx`:

  Integer denoting the index of the agent in the shared specification
  list (utility-related)

- `check`:

  List containing a logical matrix denoting the locations the agent can
  and cannot move to (utility-related)

- `ps_speed`, `ps_distance`:

  Numeric denoting the speed of the agent and the distance from the
  current goal, used to evaluate the preferred speed utility function
  (utility-related)

- `gd_angle`:

  List containing a numeric matrix of size \\1 \times 11\\

- `id_distance`, `id_check`, `id_ingroup`:

  List containing a numeric matrix of size \\A \times 33\\ containing
  the distance to other agents from each cell, a logical matrix of size
  \\11 \times 3\\ containing an update of cells the agent can and cannot
  move to when taking agents into account, and a logical vector denoting
  whether the other agents belong to the current agents ingroup. This
  information is used to evaluate the interpersonal distance utility
  function (utility-related).

- `ba_angle`, `ba_cones`:

- `fl_leaders`, `wb_buddies`:

  List containing the leaders and buddies needed to evaluate the
  follow-the-leader and walk-beside utility functions (utility-related).

- `gc_distance`, `gc_radius`, `gc_nped`:

  List containing an integer denoting the number of group members, a
  numeric vector of size \\33\\ containing the distance of a particular
  cell to the predicted position of the group centroid and the radius of
  the agent itself. Used to evaluate the group-centroid utility function
  (utility- based).

- `vf_angles`:

  List containing a numeric vector of size \\33\\ containing the
  relative angles of the agent's group members towards the agent for
  each cell. Used to evaluate the visual-field utility function
  (utility-based).
