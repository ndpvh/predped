################################################################################
# demo.R                                                                       #
#                                                                              #
# In this demo, we will provide an example of the typical workflow to be       #
# used when simulating the M4MA model. Specifically consists of four steps:    #
#   - Define the environment in which pedestrians will walk around.            #
#   - Define the agent characteristics in a comprehensive model to simulate.   #
#   - Simulate a trace of pedestrian movement.                                 #
#   - Create a gif that displays this movement.                                #
#                                                                              #
################################################################################

# Load package
library(predped)

# Create a background in which we want to let the agents walk around. Contains 
# several components:
#   - Shape: The shape of the room/building that you would like to simulate.
#   - Objects: A list of different objects that are contained within this room.
#   - Entrance/Exit: An entrance and/or exit through which the pedestrians can 
#     enter and exit the space.
#
# The shape and objects can consist of any `object` that is defined in 
# `objects.R`, namely `rectangles`, `polygons`, and `circles`.
setting <- background(shape = rectangle(center = c(4, 4), 
                                        size = c(8, 8)),
                      objects = list(circle(center = c(4, 4), 
                                            radius = 1,
                                            interactable = TRUE),
                                     rectangle(center = c(1.5, 4),
                                               size = c(1, 6),
                                               interactable = TRUE),
                                     polygon(points = rbind(c(3, 1),
                                                            c(3, 2),
                                                            c(6, 2),
                                                            c(6, 6), 
                                                            c(3, 6),
                                                            c(3, 7),
                                                            c(7, 7),
                                                            c(7, 1)),
                                             interactable = TRUE)),
                      entrance = coordinate(c(0, 4)),
                      exit = coordinate(c(8, 4)),
                      same_exit = FALSE)

# You can visualize this setting with the `plot` function. Additional to the 
# setting, you can provide this function with several other arguments that 
# influence how the plot looks like. These additional arguments are passed on to
# `ggplot` functions.
plot(setting, object.fill = "grey", object.color = "black", object.linewidth = 1.5)

# Once you defined the setting, it is time to define the characteristics of the 
# pedestrians and bind both together in what we call a `predped` model. This 
# format contains the following specifications:
#   - ID: Something to call the model by
#   - Setting: The setting we just created above
#   - Archetypes: A character vector containing the names of the archetypical 
#     pedestrians that you would like to include in your simulation. The full
#     list of archetypes as well as their defining parameters can be found in 
#     the `archetypes.csv` file
#   - Weights: The probability with which each archetype can be drawn in the 
#     simulation. Order of these probabilities should match the order in 
#     `archetypes`
#
# Importantly, users can also specify their own `archetypes` dataframe. This 
# dataframe should have the same format as `archetypes.csv` for the code to 
# work.
my_model <- predped(id = "demo", 
                    setting = setting, 
                    archetypes = c("BaselineEuropean", 
                                   "CautiousOldEuropean"), 
                    weights = c(0.75, 0.25))

# Now that the model is defined, we will simulate some pedestrian movement. 
# The `simulate` function has many arguments that can be used to tailor the 
# simulation. The most important ones for our demonstration are the following:
#   - The predped-model to be simulated from
#   - The maximal number of agents that can walk in the room at the same time
#   - The number of iterations
#   - Whether to plot each time step in real-time
#
# This function will output a list of different so-called "states": The state 
# of the environment at a given time t. These states contain information on the
# environment and the agents, including their position, orientation, and goal 
# stacks.
set.seed(1)
trace_1 <- simulate(my_model,
                    max_agents = 25, 
                    iterations = 50,
                    plot_live = TRUE)

saveRDS(trace_1,
        file.path("demo", "trace_1.Rds"))

# An alternative way of using the `simulate` function is to provide an initial 
# condition yourself, which will then be used to start the simulation. Let's 
# say that we will use the last state in `trace` and wish to simulate starting 
# from this state, we can do so in the following way.
set.seed(2) # Different seed used to ensure that new agents won't have the same id as the agents in the initial condition
trace_2 <- simulate(my_model, 
                    initial_condition = trace_1[[51]],
                    max_agents = 25,
                    iterations = 50, 
                    plot_live = TRUE)

saveRDS(trace_2, 
        file.path("demo", "trace_2.Rds"))

# A final option is to not provide an initial condition directly, but to let the 
# simulation start with a given number of pedestrians in the room. This can be 
# achieved in the following way.
set.seed(3)
trace_3 <- simulate(my_model, 
                    initial_number_agents = 5, 
                    max_agents = 25, 
                    iterations = 50, 
                    plot_live = TRUE)

saveRDS(trace_3, 
        file.path("demo", "trace_3.Rds"))

# Finally, we can generate a gif to visualize the movements of the pedestrians. 
# This is done by using the `plot` function of predped for lists (where 
# `trace = TRUE`) and then calling these plots one for one in the `save_gif` 
# function.
#
# In the first part of this code, we use the size of the background to determine 
# what the size of the gif should be (in pixels). Then we create a list of plots
# that contain a figure for each state in the trace, with which we then merge 
# into a gif. This is all put in a function so that we can create gifs for each 
# of the created traces.
create_gif <- function(x) {
    # Load the trace
    trace <- readRDS(file.path("demo", paste0(x, ".Rds")))

    # Get size of gif in pixels
    size <- shape(trace[[1]]$setting)@points |>
        matrixStats::colRanges() |>
        t() |>
        diff()
    size <- size * 200

    # Create the plots and save them to a gif
    plt <- plot(trace, 
                trace = TRUE, 
                linewidth = 1.5, 
                size = 4) 

    gifski::save_gif(lapply(plt, \(x) print(x)), 
                     file.path("demo", paste0(x, ".gif")),
                     delay = 1/10, 
                     width = size[1], 
                     height = size[2])
}

traces <- c("trace_1", "trace_2", "trace_3")
for(i in traces) {
    create_gif(i)
}
