# Load in the predictive predestrian package, together with 
# all of its libraries
source("RCore/PredictivePedestrian.R")

# Create an environment in which the agents can roam.
# In effect, this will open all the environment variables you need 
# to be able to run the simulations
#
# The print statement is not really needed, but it does give an 
# idea about which objects are loaded in the first place
print(load("Diversity/Data/1-make/make_Diversity_m01.RData"))      # objects etc.
print(load("Diversity/Data/2-stack/stack_Diversity_m01s06.RData")) # stacks

# Read in the parameters of the agents, which you can then link to 
# the agents you actually want.
#
# Format is a data.frame with both the names and colors of the agents
# ("Name", "Color") as well as a whole range of predefined parameters
# (e.g., "bCA", "aCA",...)
#
# TO DO: Make this an internal thing, just like you do for the AIM.
#        Allow parameters to be called in a function, so you don't 
#        need to read all this in. Additionally, allow people to make
#        their own parameters in a predefined format. This will make
#        the package much easier to use and more extendable
#        E.g., load_agents(c("BaselineEuropean", "BigRushingDutch"))
#
#        This whole workflow should be an internal thing, which might
#        allow for an easier time for all other individuals who would
#        want to play with this.
agent_means <- read.csv("Diversity/Config/agents.csv",
                        stringsAsFactors = FALSE )
agent_sds <- read.csv("Diversity/Config/agents_sd.csv",
                      stringsAsFactors = FALSE )

# Determine which agents you want to have in your data.
# These are names that are internally used to determine the parameters
# of the agents: If you want to play around, you can also introduce
# new names such as these
agents <- c(BaselineEuropean = 1/8,
            BigRushingDutch = 1/8,
            DrunkAussie = 1/4,
            CautiousOldEuropen = 1/2 )

# Determine the characteristics color characteristics of the agents you 
# want to have and filter out all the agents you do not want to have.
#
# TO DO: This should also be handled under the hood, not for the user
#        itself
agent_names <- names(agents)
agent_means <- agent_means[agent_means$Name %in% agent_names,]
agent_sds <- agent_sds[agent_sds$Name %in% agent_names,]

types <- setNames(agent_means[,"Color"],
                  agent_means[,"Name"] )

# Delete the first two rows that define the agent characteristics and 
# change the row names to the agent names
agent_means <- agent_means[,-(1:2)]
row.names(agent_means) <- agent_names

agent_sds <- agent_sds[,-1] |>
    t()
colnames(agent_sds) <- agent_names

# Transform parameters to real scale, add agent probabilities
agent_probability <- rbind( p = agents,                           # Probability
                            apply( agent_means, 1, toReal ) )      # Parameters

# Make a state for the agents
state <- makeState(gstack = stacks[[1]],                # Goals that one can do
                   p = agent_probability,               # Parameter means for agents as well as probability
                   pSD = agent_sds,                     # Variation on these parameter means
                   group = 1,                           # Group number to determine in- and outgroup
                   types = types )                      # Color for the agents

# Some settings for the simulation
#
# Originally, loaded in the `simulation.csv` file in `Config`
# which has some defaults on what these parameters should be.
# Can be replaced by a "default" function that automatically 
# takes in these arguments, or by creating default simulation
# operations (similar to csv, but then as variables, e.g. as 
# named arguments or methods)
time_between_entry <- p_entryCycles <- 10                       # Every `time_between_entry` iterations, someone new is added 
store_capacity <- p_max_caps <- 25                              # Maximal number of pedestrians in the store
iterations <- 2000                                              # Maximal number of iterations to simulate
interaction_time <- p_interactionTime <- 5                      # Number of iterations needed to interact with a goal

# Nests, not used in multinomial
#
# Not sure what to do with these yet. To check in parameter
# refactorization
nests <- list(Central = c(0, 6, 17, 28),
              NonCentral = c(0:33)[-c(0 , 6, 17, 28)],
              acc = c(1:11),
              const = 12:22,
              dec = c(0, 23:33) )
# All alternatives a member of 2 nests so always alpha = .5
alpha <- setAlpha(nests)

# Create a directory in which the figures will be saved
wme_directory <- function(x) {
    path <- file.path(".", "MinimalWorkingExample", x, "Trace2")
    if(!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
    return(path)
}
plot_directory <- wme_directory("Figures")
data_directory <- wme_directory("Data")

# Set up a state like you did before
state <- makeState(gstack = stacks[[1]],                # Goals that one can do
                   p = agent_probability,               # Parameter means for agents as well as probability
                   pSD = agent_sds,                     # Variation on these parameter means
                   group = 1,                           # Group number to determine in- and outgroup
                   types = types )                      # Color for the agents

# Create a vector in which the trace will be saved so
# that some memory is allocated already
trace <- vector(mode = "list", 
                length = iterations )

# Create an image-list that will contain all the images
# that have been created
img_list <- list()

# Create function that will be used to define the output
# names: Makes sure that they are chronological
format_number <- function(x,                            # The number to be transformed
                          n_characters = 5) {           # The number of characters the number should contain

    # Define the command for leading zeros in the sprintf function 
    leading_zeros <- paste0("%0", n_characters, "d")

    # Format the number and return
    return(sprintf(leading_zeros, x))
}

# And do the simulation itself
for(i in seq_along(trace)) {
    # Print iteration number and # pedestrians present
    cat(paste0("Iteration: ", i, ", Number of participants: ", nrow(state$p), "\n"))

    # Add current state to trace
    trace[[i]] <- state

    # Get the relevant goals from the trace and change some of 
    # its attributes
    #
    # However, not sure why this is relevant
    change_attribute <- function(x) {
        attr(x, "replan") <- NULL 
        return(x)
    }
    goals <- lapply(trace[[i]]$P,
                    function(x) change_attribute(x) )
    attr(goals,"numLetter") <-  attr(trace[[i]]$P,"numLetter")
    attr(goals,"reroute") <-  attr(trace[[i]]$P,"reroute")
    attr(goals,"replan") <-  attr(trace[[i]]$P,"replan")
    trace[[i]]$P <- goals
    
    # Get the directory for the figure
    filename <- file.path(plot_directory, 
                          paste0("p", format_number(i)))
    
    # Change the current state for all agents in the 
    # field
    state <- moveAll(state,                                     # Current state
                     objects,                                   # Objects in the space
                     nests,                                     # Those extra parameters (to figure out what they do)
                     alpha,                                     # And their alpha's
                     plotSim = TRUE,                            # Whether to plot the simulation
                     fname = filename,                          # Filename under which to save the plot
                     reportGoal = FALSE,                        # ? I assume it has something to do with printing the goal an agent is doing now
                     delay = 0,                                 # Delay to be put on the plots                           
                     interactionTime = interaction_time,        # Time it takes to interact with the goals 
                     iteration = i )                            # Needed to stop an error that makes use of the iteration number as a global variable
    
    # Add chosen cell to previous state
    #
    # Done so that you have the new positions of everyone.
    #
    # However, would it not be easier to just add the state to the 
    # trace at a later point in this loop? At no point do you need 
    # it before this
    trace[[i]]$cell <- state$cell

    # Append the plot to the image-list
    img_list[[i]] <- image_read_pdf(paste0(filename, ".pdf"))
    
    # Remove pedestrians at exit state
    state <- exitPed(state) # if reached goal exit
    
    # Stop simulation if there are no pedestrians anymore
    if (dim(state$p)[1] == 0) {
        break 
    }
    
    # Add pedestrian to state unless store full or start position occupied
    if (!(nrow(state$p) >= store_capacity) & (i %% time_between_entry == 0)) { 
        # Randomly pick goal stack from available sets
        gstack = stacks[[sample(1:length(stacks), 1)]]
        state <- addPed(gstack,                              # Goals that one can do
                        p = agent_probability,               # Parameter means for agents as well as probability
                        pSD = agent_sds,                     # Variation on these parameter means 
                        state = state,                       # Current state of the simulation
                        addPedReport = FALSE,                # ? Not sure what it does
                        types = types,                       # Types of pedestrian in the running
                        objects = objects )                  # Objects to account for in the space
    }
}

# Remove empty slots in trace in case sim is topped before n_iterations
trace <- trace[!unlist(lapply(trace, is.null))]

# Add space attributes for replay
attr(trace, "space") <- list(objects = objects, 
                             goalLines = goalLines,
                             goalRectangles = goalRectangles, 
                             entry = entry,
                             exitHigh = exitHigh, 
                             pathPoints = pathPoints,
                             oneWay1 = oneWay1, 
                             oneWay2 = oneWay2 )

# Add alpha and nests attributes
attr(trace, "alpha") <- alpha
attr(trace, "nests") <- nests

# Only using RDS for compatibility with old cog servers
saveRDS(trace, 
        file = file.path(data_directory, 
                         "trace.RDS" ) )

# Join the images together and animate them with a given fps rate
img_joined <- image_join(img_list)
img_animated <- image_animate(img_joined, fps = 2)

# View animated image
# img_animated

## save to disk
image_write(image = img_animated,
            path = file.path(plot_directory, "trace_8842.gif"))
