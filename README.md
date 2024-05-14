<!-- badges: start --> 
[![build](https://github.com/ndpvh/predped/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ndpvh/predped/actions/workflows/R-CMD-check.yaml)
[![coverage](https://github.com/ndpvh/predped/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/ndpvh/predped/actions/workflows/test-coverage.yaml)
[![license: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

# predped: An R package to simulate the Minds for Mobile Agents (M4MA) pedestrian model

This project serves as a tool to simulate pedestrian movement using the Minds for Mobile Agents (M4MA) model. It contains several helper functions for people who wish to use the model to investigate pedestrian flow in their own settings. Before delving into how to use the package, we will first provide the reader with some background on the model.

## Background

Pedestrian models are popular tools to investigate how people navigate the complex world that we live in. While these models have certainly been useful to understand, for example, evacuation behavior, they most often assume that pedestrians are homogeneous -- that is, that all pedestrians are one and the same. Because of this assumption, these pedestrian models are not suited to capture the variety of walking behaviors we observe in most low-density situations, such as those observed in the supermarket, in the train station, or even on the street.

To alleviate these difficulties, Andrew Heathcote, Dora Matzke, and Charlotte Tanis recently proposed M4MA to capture pedestrian movement. This model is based on the pedestrian model developed by Robin et al. (2009) and, similarly, proposes that pedestrian movement is determined on three levels: 
- _Strategic level_: Consists of planning where the pedestrian wants to go to and via which route. Typically, a pedestrian will have to fulfill a goal, which will serve as the destination of the route. To get to this goal, pedestrians will walk along so-called "path-points": Waypoints along which the pedestrian can walk, serving as intermediate steps to get to their destination. 
- _Tactical level_: Once their route is determined, pedestrians will still be able to adapt if their original plan does not seem feasible. For example, other pedestrians can block your route, which makes avoiding the crowd more attractive when trying to get to your goal. These adaptations are handled on this level.
- _Operational level_: This final level handles the low-level, moment-to-moment decisions about where to walk to. In practice, this constitutes a choice between one of 34 movement options, created by 11 angular directions in which a pedestrian can move (going from -72.5 to 72.5 degrees) and 3 changes in velocity (deceleration, constant speed, or acceleration), with the 34^st^ option representing stopping. The probability with which a pedestrian will move to a given location is determined by a utility function that combines several moving components, namely (a) preferred speed, (b) current direction, (c) goal direction, (d) interpersonal distance, and (e) blocked angles, as well as several social components, namely (a) following a person heading in the same direction (follow-the-leader) and (b) walking besides someone heading in the same direction (walk-besides-buddy).

Critically, and different from other pedestrian models, M4MA assigns each pedestrian a "personality", in that all pedestrians have their own unique values for the parameters that are defined on the operational level. These individual differences are implemented in two ways:
- _Qualitative differences_: We define several "archetypes" of pedestrians, each of which have different discrete values for their parameters. These archetypes were created through trial-and-error by the researchers in this project and are contained in _archetypes.csv_. In no way should these archetypes be interpreted as ground truth or real differences between individuals.
- _Quantitative differences_: Each of the parameter values that are defined in the archetypes CSV-file serves as a mean around which each pedestrian's actual parameter value fluctuates. Without going into detail, each archetype has their own matrix that contains standard deviations for each parameter on the diagonal and correlations between parameters on the off-diagonal. These matrices are stored in _archetypes\_sigma.Rds_.

## How to use

This package allows its users to simulate pedestrian behavior as expected by M4MA. Users can specify the environment, the characterstics the pedestrians, and the characteristics of the simulation itself (e.g., maximal number of agents, initial conditions,...), therefore allowing for a great variety of potential research questions that can be answered. For more information on the workflow of this package, see _Getting started_.

An unofficial benchmark is available through the repository. This package depends heavily on the [```m4ma```](https://github.com/m4ma/m4ma) package.

## Installation

One can install ```predped``` through ```devtools``` in the following way:

```R
install.packages('devtools')

devtools::install_github('ndpvh/predped', auth_token = "PERSONAL_ACCESS_TOKEN")
```

Note that currently, ```predped``` is still a private package. This means that you should input your own personal access token to be able to download the package.

It is also useful to be mindful of the dependencies of ```predped```. Currently, it imports ```matrixStats```, ```methods```, ```m4ma```, and ```ggplot2```. Of these, [```m4ma```](https://github.com/m4ma/m4ma) has also been developed by our lab.

## Getting started

The explanation in this section will follow the code contained within _getting\_started.R_. A more elaborate example of how to use the package is contained within _demo.R_. Both scripts adhere to the same workflow. 

Before you can get started, you should load the package:

```R
library(prepded)
```

In the first step, you should define the environment in which you want pedestrians to walk around. For this, you will use the ```background``` class. This S4 class contains slots for the ```shape``` (i.e., the shape of the room), the ```objects``` (i.e., which objects are contained within the room), and a potential ```entrance``` and/or ```exit```. The ```shape``` and ```objects``` should all consist of instances of the ```object``` class, that is they should be either a ```rectangle```, ```polygon```, or ```circle```. The ```entrance```/```exit``` should be instances of the ```coordinate``` class.

A simple example of an environment is the following circular room with a square placed in the middle.

```R
setting <- background(shape = circle(center = c(0, 0), 
                                     radius = 2), 
                      objects = list(rectangle(center = c(0, 0), 
                                               size = c(1, 1), 
                                               interactable = TRUE)),
                      entrance = coordinate(c(-2, 0)),
                      same_exit = TRUE)
```

In this chunk of code, the argument ```interactable = TRUE``` makes sure that the square in the middle of the room can contain goals for the pedestrians to fulfill. If none of the objects in the ```objects``` list are interactable, then the model will not be able to run. The argument ```same_exit``` defines whether the entrance and exit are at the same coordinate. If ```same_exit = FALSE```, then a coordinate for the exit should also be provided.

It is good practice to visualize what the environment looks like. For this, we can use the ```plot``` function.

```R
plot(setting)
```

Now that we have defined the environment in which pedestrians will walk around, we have to define the characteristics of the pedestrians and link these with the environment in what is called a ```predped``` model. This is another S4 class with slots ```setting``` (the environment we just created), ```parameters``` (dataframe of parameter values), ```archetypes``` (a character string of pedestrians to include), and ```weights``` (the probability of each archetype entering the room). 

For legacy reasons, the current package works with a dataframe that contains parameter values for a given "class" of people. One such dataframe is provided by us in _archetypes.csv_ and can be called in the package through the variable ```params_archetypes```. As you can see, this dataframe consists of different parameter values, as well as a "name" and "color". The provided color will be the color that is used to plot the pedestrians when plotting out the results of the simulation. The name is what will be used to distinguish between different parameter sets, and concerns the variable that is selected upon based on the values of ```archetypes``` in the ```predped``` model. For example, if we only wish to have a simulation with ```"BaselineEuropean"```s, then we can specify the model like.

```R
model <- predped(id = "my model", 
                 setting = setting, 
                 archetypes = "BaselineEuropean")
```

Of course, you are free to specify whichever combination of archetypes you wish. Note that if you use more than one archetype and you want the probability of entering the room to be different for each of those, you should also provide values to the ```weights``` slot.

With the model defined, we can now finally simulate pedestrian movement. For this, we use the function ```simulate``` as follows.

```R
trace <- simulate(model,
                  max_agents = 25, 
                  iterations = 50,
                  plot_live = TRUE)
```

In this piece of code, we specify that within this simulation:
- There can only be a maximum of 25 agents in the room;
- It can only run for 50 iterations;
- We want the pedestrian movement to be plotted to our plot while running the simulation (disclaimer: This is useful, but runs significantly slower than when ```plot_live = FALSE```).

Once the simulation is done, the variable ```trace``` will be a list of different so-called intermediate "states" of the environment. Each state is another list containing the environment (under slot ```setting```) and the pedestrians themselves (under slot ```agents```). If you wish to visualize this trace, you can again use the ```plot``` function, this time with the additional argument ```trace = TRUE```.

```R
plots <- plot(trace, 
              trace = TRUE)
```

The variable ```plots``` is a list containing a plot for each state in ```trace```.

## Documentation

The documentation of m4ma is build with ```roxygen2``` and ```pkgdown``` and is currently only locally available.

## Contribute

While this model already came far, there are still many gaps in the behaviors it can capture. Some of these are currently being implemented, such as queueing and walking in group. Others may be missing without us knowing. If you want to give us pointers as to which behaviors should be implemented next, or if you otherwise wish to contribute to this project, feel free to reach out to Niels Vanhasbroeck (niels.vanhasbroeck@gmail.com) and Andrew Heathcote (ajheathcote@gmail.com).

## Credits

The development of this package would not have been possible without the help of its many contributors. For the original implementation of the model, we thank (in alphabetical order):

- [Andrew Heathcote](https://github.com/andrewheathcote)
- [Charlotte Tanis](https://github.com/CharlotteTanis)

For the reworked version of ```predped```, we thank several permanent project members (in alphabetical order):

- [Andrew Heathcote](https://github.com/andrewheathcote)
- [Niels Vanhasbroeck](https://github.com/ndpvh)

as well as many who worked with us temporarily (in alphabetical order):

- [Alexander Anderson](https://github.com/Alexanderson31)
- [Joris Goossen](https://github.com/JorisGoosen)
- [Malte Lüken](https://github.com/maltelueken)
- [Ece Yatıkçı](https://github.com/eceyatikci) 

## See also

For more information on the project, please see its dedicated section on the lab website: [https://www.ampl-psych.com/projects/minds-for-mobile-agents/](https://www.ampl-psych.com/projects/minds-for-mobile-agents/).

## References

Robin, T., Antonini, G., Beirlaire, M., & Cruz, J. (2009). Specification, estimation, and validation of a pedestrian walking behavior model. _Transportation Research Part B, 43_, 36-56. doi: [10.1016/j.trb.2008.06.010](doi.org/10.1016/j.trb.2008.06.010)