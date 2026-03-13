# predped: An R package to simulate the Minds for Mobile Agents (M4MA) pedestrian model

This project serves as a tool to simulate pedestrian movement using the
Minds for Mobile Agents (M4MA) model. It contains several helper
functions for people who wish to use the model to investigate pedestrian
flow in their own settings. Before diving into how to use the package,
we will first provide the reader with some background on the model.

## Background

Pedestrian models are popular tools to investigate how people navigate
the complex world that we live in. While these models have certainly
been useful to understand, for example, evacuation behavior, they most
often assume that pedestrians are homogeneous – that is, that all
pedestrians are one and the same. Because of this assumption, these
pedestrian models are not suited to capture the variety of walking
behaviors we observe in most low-density situations, such as those
observed in the supermarket, in the train station, or even on the
street.

To alleviate these difficulties, Andrew Heathcote and Dora Matzke
recently proposed M4MA to capture pedestrian movement. This model is
based on the pedestrian model developed by Robin et al. (2009) and,
similarly, proposes that pedestrian movement is determined on three
levels:

- *Strategic level*: Consists of planning where the pedestrian wants to
  go to and via which route. Typically, a pedestrian will have to
  fulfill a goal, which will serve as the destination of the route. To
  get to this goal, pedestrians will walk along so-called “path-points”:
  Waypoints along which the pedestrian can walk, serving as intermediate
  steps to get to their destination.
- *Tactical level*: Once their route is determined, pedestrians will
  still be able to adapt if their original plan does not seem feasible.
  For example, other pedestrians can block your route, which makes
  avoiding the crowd more attractive when trying to get to your goal.
  These adaptations are handled on this level.
- *Operational level*: This final level handles the low-level,
  moment-to-moment decisions about where to walk to. In practice, this
  constitutes a choice between one of 34 movement options, created by 11
  angular directions in which a pedestrian can move (going from -72.5 to
  72.5 degrees) and 3 changes in velocity (deceleration, constant speed,
  or acceleration), with the $34^{st}$ option representing stopping. The
  probability with which a pedestrian will move to a given location is
  determined by a utility function that combines several moving
  components, namely (a) preferred speed, (b) current direction, (c)
  goal direction, (d) interpersonal distance, and (e) blocked angles, as
  well as several social components, namely (a) following a person
  heading in the same direction (follow-the-leader) and (b) walking
  besides someone heading in the same direction (walk-besides-buddy).

Critically, and different from other pedestrian models, M4MA assigns
each pedestrian a “personality”, in that all pedestrians have their own
unique values for the parameters that are defined on the operational
level. These individual differences are implemented in two ways:

- *Qualitative differences*: We define several “archetypes” of
  pedestrians, each of which have different discrete values for their
  parameters. These archetypes were created through trial-and-error by
  the researchers in this project and are contained in *archetypes.csv*.
  In no way should these archetypes be interpreted as ground truth or
  real differences between individuals.
- *Quantitative differences*: Each of the parameter values that are
  defined in the archetypes CSV-file serves as a mean around which each
  pedestrian’s actual parameter value fluctuates. Without going into
  detail, each archetype has their own matrix that contains standard
  deviations for each parameter on the diagonal and correlations between
  parameters on the off-diagonal. These matrices are stored in
  *archetypes_sigma.Rds*.

## How to use

This package allows its users to simulate pedestrian behavior as
expected by M4MA. Users can specify the environment, the characterstics
the pedestrians, and the characteristics of the simulation itself (e.g.,
maximal number of agents, initial conditions,…), therefore allowing for
a great variety of potential research questions that can be answered.
For more information on the workflow of this package, see *Getting
started*.

This package depends heavily on the
[`m4ma`](https://github.com/m4ma/m4ma) package.

## Installation

One can install `predped` through the `remotes` package in the following
way:

``` r
remotes::install_github("ndpvh/predped")
```

To use the package, you should load it through `library`:

``` r
library(predped)
```

## Getting started

Within `predped`, we use a particular workflow.

In the first step, you should define the environment in which you want
pedestrians to walk around. For this, you will use the
[`background`](https://ndpvh.github.io/reference/background-class.html)
class. This S4 class contains slots for the `shape` (i.e., the shape of
the room), the `objects` (i.e., which objects are contained within the
room), and a potential `entrance` and/or `exit`. The `shape` and
`objects` should all consist of instances of the
[`object`](https://ndpvh.github.io/reference/object-class.html) class,
that is they should be either a
[`rectangle`](https://ndpvh.github.io/reference/rectangle-class.html),
[`polygon`](https://ndpvh.github.io/reference/polygon-class.html), or
[`circle`](https://ndpvh.github.io/reference/circle-class.html). The
`entrance`/`exit` should be either a numeric vector or a numeric matrix
with two columns specifying their coordinates.

A simple example of an environment is the following circular room with a
square object placed in the middle:

``` r
setting <- background(
  shape = circle(
    center = c(0, 0), 
    radius = 2
  ), 
  objects = list(
    rectangle(
      center = c(0, 0), 
      size = c(1, 1)
    )
  ),
  entrance = c(-2, 0)
)
```

It is good practice to visualize what the environment looks like. For
this, we can use the
[`plot`](https://ndpvh.github.io/reference/plot.html) function:

``` r
plot(setting)
```

![One sees a plot visualizing a circular room with a square gray object
in the middle. This represents the room in which the agents will walk
around if the room is used for a
simulation.](reference/figures/README-plot-1.png)

Once an environment has been defined, on should link this environment
with the characteristics of the agents who are expected to walk around
in this environment. This is achieved through the definition of a
[`predped`](https://ndpvh.github.io/reference/predped.html) model,
another S4 class with slots `setting` (the environment we just created),
`parameters` (dataframe of parameter values), `archetypes` (a character
string of pedestrians to include), and `weights` (the probability of
each archetype entering the room).

Agent characteristics are defined through a `data.frame` that contains
parameter values for a given “class” of people. One such dataframe is
provided by us in *archetypes.csv* and can be called in the package
through the variable
[`params_from_csv`](https://ndpvh.github.io/reference/params_from_csv.html)
or through calling the function
[`load_parameters()`](https://ndpvh.github.io/reference/load_parameters.html).
In this example, we wish to use the `"BaselineEuropean"`, specifying the
model as:

``` r
model <- predped(
  id = "my model", 
  setting = setting, 
  archetypes = "BaselineEuropean"
)
```

Now that the model has been defined, we can now simulate pedestrian
movement by calling the function
[`simulate`](https://ndpvh.github.io/reference/simulate.html):

``` r
set.seed(1)
trace <- simulate(
  model,
  max_agents = 25, 
  iterations = 50
)
```

The variable `trace` will be a list consisting of
[`state`](https://ndpvh.github.io/reference/state.html)s of the
environment, each state itself consisting of a copy of the environment
(under slot `setting`) and of a list of pedestrians walking around in
the environment (under slot `agents`). If you wish to visualize this
trace, you can again use the
[`plot`](https://ndpvh.github.io/reference/plot.html) function:

``` r
plots <- plot(
  trace,
  print_progress = FALSE
)
```

The [`plot`](https://ndpvh.github.io/reference/plot.html) outputs a list
of plots. For research purposes, it is useful to transform this list to
a gif, which can be achieved by using the `gifski` package:

``` r
gifski::save_gif(
  lapply(plots, print), 
  file.path("readme.gif"),
  delay = 1/10
)
```

``` R
#> [1] "/Users/nielsvanhasbroeck/Documents/UvA/Projects/Software, Pedestrian Modeling/man/figures/readme.gif"
```

Looking at the created `.gif` then gives us an idea of how the agents
walked around in the room:

![A .gif displaying how an agent comes in through the entrance on the
left side, interacts with a goal on the left side of the object,
completes it, and then moves to a next goal on the bottom side of the
rectangle.](reference/figures/readme.gif)

## Getting help

You can find the documentation for this package on its dedicated
[documentation site](https://ndpvh.github.io/predped). This site
includes additional information on the [theoretical
background](https://ndpvh.github.io/predped/articles/theory.html), on
[running
minimal](https://ndpvh.github.io/predped/articles/simulation.html) and
[advanced
simulations](https://ndpvh.github.io/predped/articles/advanced_simulation.html),
and on [estimating the model on
data](https://ndpvh.github.io/predped/articles/estimation.html).

If you encounter a bug, you can report the bug with a minimal working
example as an [Issue](https://github.com/ndpvh/predped/issues).

## Contribute

If you otherwise wish to contribute to this project, feel free to reach
out to Niels Vanhasbroeck (<niels.vanhasbroeck@gmail.com>) and Andrew
Heathcote (<ajheathcote@gmail.com>).

## Credits

The development of this package would not have been possible without the
help of its many contributors. For the development of the `m4ma`
package, we thank (in alphabetical order):

- [Andrew Heathcote](https://github.com/andrewheathcote)
- [Malte Lüken](https://github.com/maltelueken)
- [Charlotte Tanis](https://github.com/CharlotteTanis)

For the creation of the `predped`, we thank several permanent project
members (in alphabetical order):

- [Andrew Heathcote](https://github.com/andrewheathcote)
- [Niels Vanhasbroeck](https://github.com/ndpvh)

as well as many who worked with us temporarily (in alphabetical order):

- [Alexander Anderson](https://github.com/Alexanderson31)
- [Joris Goossen](https://github.com/JorisGoosen)
- [Malte Lüken](https://github.com/maltelueken)
- [Ece Yatıkçı](https://github.com/eceyatikci)

## See also

For more information on the project, please see its dedicated section on
the lab website:
<https://www.ampl-psych.com/projects/minds-for-mobile-agents/>.

## References

Robin, T., Antonini, G., Beirlaire, M., & Cruz, J. (2009).
Specification, estimation, and validation of a pedestrian walking
behavior model. *Transportation Research Part B, 43*, 36-56. doi:
[10.1016/j.trb.2008.06.010](https://github.com/ndpvh/predped/doi.org/10.1016/j.trb.2008.06.010)
