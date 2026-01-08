# Default Parameter List

This parameter list contains the default mean parameters under slot
`params_archetypes`, their standard deviations under slot
`params_sigma`, and their bounds under `params_bounds`. These parameters
automatically come with the package.

## Usage

``` r
params_from_csv
```

## Format

An object of class `list` of length 3.

## Details

\# Content of the slots

Values under slots `params_archetypes` and `params_sigma` contain some
variation as imposed through what we call the "archetypes", which for
`params_archetypes` can be found in the column `name` and for
`params_sigma` in the names of this list. These archetypes represent
parameter sets that have been created to display a given type of
behavior, such as rushing to get to the goals ("Rushed") or making very
random moment-to-moment decisions ("DrunkAussie"). These archetypes thus
represent a part of the individual variability that `predped` allows.

Another aspect of this variability is controlled by the values under
`params_sigma`. For each of the archetypes a covariance matrix is
defined in the list `params_sigma` that allows for variation around the
values found in `params_archetypes`. However, do not mistake the
matrices in `params_sigma` to be covariance matrices: Instead, these
matrices have standard deviations on the diagonal and correlations
between the parameters on the off-diagonal, allowing for users to more
intuitively set up these matrices themselves. Under the hood, the
covariance matrix COV is computed through the provided matrix X by
defining:

\\SD = diag(X) ,\\

and by creating the matrix COR which consists of X with its diagonal
turned to 1. We can then compute the covariance matrix by multplying
both matrices:

\\COV = COR \* SD \* SD^T .\\

Importantly, the standard deviation and correlations should be defined
between each of the parameters. We furthermore note that the standard
deviations can also be equal to 0, allowing no variation in the selected
parameters.

\# Parameters

Each of the parameters in `params_archetypes` controls an aspect of the
decisions pedestrians make when walking around in an environment,
namely:

- `radius`::

  the radius of the agent

- `slowing_time`::

  the number of seconds the agent needs to slow down when approaching a
  goal

- `preferred_speed`::

  the speed at which the agent is comfortable walking

- `randomness`::

  the temperature parameter that controls the overall unpredictability
  of the nex decision an agent will make. Larger values make movement
  more deterministic (i.e., strongly determined by the utility
  functions).

- `stop_utility`::

  utility value of stopping instead of moving

- `reroute`::

  number of pedestrians that should be in the way for an agent to
  consider rerouting 50% of the time

- `b_turning`::

  slope that scales the effect of turning on the speed an agent can
  maintain, where `1 - b_turning` denotes the maximal decrease in
  velocity in percent

- `a_turning`::

  exponent that determines the shape of the effect of turning on the
  speed an agent can maintain

- `b_current_direction`::

  slope that scales the utility of continuing waking in the current
  direction

- `a_current_direction`::

  exponent that determines the power to which the difference of not
  walking in the current direction is taken

- `blr_current_direction`::

  scales the preference for walking to the left or right when heading in
  a given direction. Done in such a way that `b_current_direction`
  defines the slope for the left side and is divided by
  `blr_current_direction` for the right side, meaning that the slope for
  the right side increases when `blr_current_direction < 1` and
  decreases when `blr_current_direction > 1`

- `b_goal_direction`::

  slope that scales the utility of heading in the direction of your
  current goal

- `a_goal_direction`::

  exponent that determines the power to which the utility of not heading
  towards the current goal is taken

- `b_blocked`::

  slope that scales the extent to which agents will avoid directions
  that in the long run will lead to blockage (e.g., because of other
  agents)

- `a_blocked`::

  exponent that determines the power of the function that determines the
  utility for avoiding directions that will lead to blockage

- `b_interpersonal`::

  slope that scales the steepness of the utility for keeping an
  interpersonal distance

- `a_interpersonal`::

  exponent that determines the power to which the interpersonal distance
  is taken. Note that – in contrast to the other exponents – the
  exponent here concerns the exponent of a hyperbolic function, rather
  than a power function.

- `d_interpersonal`::

  increment added to `b_interpersonal` when pedestrians close to the
  agent are of a different social group, effectively increasing the
  interpersonal distance

- `b_preferred_speed`::

  slope that scales the effect of trying to walk at your preferred speed

- `a_preferred_speed`::

  exponent that determines the power to which the difference of not
  walking at your preferred speed is taken

- `b_leader`::

  slope that scales the effect of selecting and following in a leader's
  footsteps

- `a_leader`::

  exponent that determines the power of the function for the
  follow-the-leader effect

- `d_leader`::

  increment added to `b_interpersonal` when the leader is of the same
  social group as the agent, effectively increasing the tendency for the
  agent to follow this leader

- `b_buddy`::

  slope that scales the effect of selecting and walking besides a buddy

- `b_group_centroid`::

  slope that scales the effect of trying to maintain a small distance
  between an agent and their group members

- `a_group_centroid`::

  exponent that determines the power with which the distance of the
  agent to their group members is taken

- `b_visual_field`::

  slope that scales the effect of maintaining group members within the
  visual field

## See also

[`predped-class`](https://github.com/ndpvh/predped/reference/predped-class.md),
[`generate_parameters`](https://github.com/ndpvh/predped/reference/generate_parameters.md),
[`load_parameters`](https://github.com/ndpvh/predped/reference/load_parameters.md),
`utility-agent` `utility-data.frame`
