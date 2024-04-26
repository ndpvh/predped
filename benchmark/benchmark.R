devtools::load_all()

# Define the supermarket environment as the setting of choice. This environment
# consists of a rectangular shape, several shelves, and a separate entrance and
# exit. All values are in meters.
setting <- background(shape = rectangle(center = c(20, 12.5),
                                        size = c(40, 25)), 
                      objects = list(# Special shelves
                                     rectangle(center = c(20, 1.2 / 2), 
                                               size = c(32, 1.2), 
                                               interactable = TRUE),
                                     rectangle(center = c(38, 4),
                                               size = c(1.2, 4),
                                               interactable = TRUE),
                                     rectangle(center = c(39.4, 7.5 + 8.75), 
                                               size = c(1.2, 25 - 7.5), 
                                               interactable = TRUE),
                                     polygon(points = rbind(c(0, 19.5),
                                                            c(0, 25),
                                                            c(16, 25),
                                                            c(16, 22.5),
                                                            c(1.2, 22.5),
                                                            c(1.2, 19.5)),
                                             interactable = TRUE),
                                     rectangle(center = c(1, 14.5),
                                               size = c(2, 1.2),
                                               interactable = TRUE),
                                     polygon(points = rbind(c(0, 3.5), 
                                                            c(0, 11.6),
                                                            c(17, 11.6),
                                                            c(17, 10.4),
                                                            c(1.2, 10.4),
                                                            c(1.2, 3.5)),
                                             interactable = TRUE),
                                     # Bottom left
                                     rectangle(center = c(12, 4),
                                               size = c(12, 1.2),
                                               interactable = TRUE),
                                     rectangle(center = c(12, 7),
                                               size = c(12, 1.2),
                                               interactable = TRUE),
                                     # Top left
                                     rectangle(center = c(12.5, 14),
                                               size = c(13, 1.2),
                                               interactable = TRUE),
                                     rectangle(center = c(12.5, 17),
                                               size = c(13, 1.2),
                                               interactable = TRUE),
                                     rectangle(center = c(12.5, 20),
                                               size = c(13, 1.2),
                                               interactable = TRUE),
                                     # Top right
                                     rectangle(center = c(29, 11),
                                               size = c(14, 1.2),
                                               interactable = TRUE),
                                     rectangle(center = c(29, 14),
                                               size = c(14, 1.2),
                                               interactable = TRUE),
                                     rectangle(center = c(29, 17),
                                               size = c(14, 1.2),
                                               interactable = TRUE),
                                     rectangle(center = c(29, 20),
                                               size = c(14, 1.2),
                                               interactable = TRUE),
                                     rectangle(center = c(29, 23),
                                               size = c(14, 1.2),
                                               interactable = TRUE),
                                     # Bottom right
                                     rectangle(center = c(27, 4),
                                               size = c(12, 1.2),
                                               interactable = TRUE),
                                     rectangle(center = c(27, 7),
                                               size = c(12, 1.2),
                                               interactable = TRUE)),
                      entrance = c(0, 0.5),
                      exit = c(0, 13),
                      same_exit = FALSE)

# Define the model that we want to run. Here, we define the setting, a dataframe
# that contains the parameters of the model, which of these "agent archetypes"
# to use, and how many of each should be used.
#
# Given that we want to benchmark, I'll only use "BaselineEuropean"s
my_model <- predped(id = "benchmark", 
                    setting = setting, 
                    parameters = params_archetypes,
                    archetypes = "BaselineEuropean")

# Benchmark the simulation.
#
# Here, we will start with 50 agents and try to keep the maximum to 50 agents.
# We will use the default values for the simulation, which means that:
#   - Goals and their duration are allowed to vary according to a normal 
#     distribution with mean 10 and standard deviation 2
#   - Goal stacks are ordered according to their distance to the entrance
#   - Paths along which the agent can walk are precomputed beforehand
#
# To stop the startup costs (which are extensive: estimated 3min), we will use
# the final state of a previously created state in which 50 agents were walking
# around in this environment.
#
# Running this shows that the simulation takes 9.7min, meaning about 12sec per 
# iteration.
initial_condition <- readRDS(file.path("benchmark", "benchmark_inx.Rds"))

set.seed(1)
Rprof(0.001)
trace <- predped::simulate(my_model,
                           max_agents = 50, 
                           initial_agents = initial_condition$agents,
                           iterations = 50,
                           report = FALSE)
Rprof(NULL)
summaryRprof()

# TO DO
#    - Vectorize `add_nodes`
#    - Take look at best_angle and moving_options to see whether there are any 
#      things that can go faster
#    - Do the same for the routing algorithms; maybe there is something that 
#      can make it faster!

saveRDS(trace, file.path("benchmark", "benchmark_trace.Rds"))

# To make a gif out of the trace, just use the following bits of code. In this 
# code: 
#   - We first extract information on the size of the environment. Is used later
#     to determine the size of the figure
#   - We create a list of different plots that lay out the trace
#   - We then transform these plots to a gif using the `gifski` package
#
# Running this shows that the plotting takes 47sec.
Rprof(0.001)
points <- shape(setting)@points
poly_size <- c(max(points[,1] - min(points[,1])),
               max(points[,2] - min(points[,2])))

plt <- plot(trace, trace = TRUE, linewidth = 2, size = 4)
gifski::save_gif(lapply(plt, \(x) print(x)), 
                 file.path("benchmark", "benchmark.gif"),
                 delay = 1/10, 
                 width = poly_size[1] * 200, 
                 height = poly_size[2] * 200)
Rprof(NULL)
summaryRprof()