# An S4 Class to Represent the M4MA model.

The `predped` class defines some of the settings that will be used when
simulating data with the M4MA model. It contains the setting in which
the agents should walk around as well as the parameters of those agents.
At this moment, only supports the use of archetypes: Prototypical agents
that have a given parameter set (see
[`generate_parameters`](https://github.com/ndpvh/predped/reference/generate_parameters.md)
and
[`params_from_csv`](https://github.com/ndpvh/predped/reference/params_from_csv.md)).

## Slots

- `id`:

  Character that defines an identifier for the model.

- `setting`:

  Object of the
  [`background-class`](https://github.com/ndpvh/predped/reference/background-class.md).

- `parameters`:

  Dataframe containing the parameters per archetype in its columns.

- `archetypes`:

  Character or character vector containing the names of the archetypes
  that should be included in the simulation.

- `weights`:

  Numeric vector containing the probability with which an agent of each
  archetype can be selected to wander around in the environment. The
  weights should be in the same length as the `archetypes`-slot.

## See also

`initialize-predped`
