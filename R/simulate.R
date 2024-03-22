#' Simulate the M4MA
#' 
#' This function allows users to simulate data from their specified `predped`
#' model. 
#' 
#' @param object The `predped` model that you want to simulate
#' @param max_agents Integer denoting the maximal number of agents that can be 
#' present in the environment. Defaults to `20`.
#' @param iterations Integer denoting the number of iterations to run the 
#' simulation for. Defaults to `1800`, which corresponds to 15 minutes of 
#' simulation (each iterations is 500 msec).
#' @param add_agent_after Integer, vector of integers, or function that determines 
#' after how many iterations an agent gets added to the environment. Defaults to 
#' a function that draws `x` numbers from a normal distribution with mean 60 
#' (30 sec) and standard deviation 15 (7.5 sec).
#' @param goal_number Integer, vector of integers, or function that determines 
#' how many goals each of the agents should receive. Defaults to a function that 
#' draws `x` numbers from a normal distribution with mean 10 and standard 
#' deviation 2. 
#' @param goal_duration Integer or function that determines the duration of each 
#' goal. Defaults to a function that draws `x` numbers from a normal distribution 
#' with mean 10 (5 sec) and standard deviation 2 (1 sec).
#' @param ... Arguments passed on to the \code[predped]{update_state} function.
#' 
#' @export
#
# TO DO
#   - At this moment, setting is kept separate from rest in trace. However, at 
#     some point, agents should be able to move things in the environment, meaning
#     we should keep a trace of moveable objects as well (either list in 
#     `setting` or a separate list as `moveable_objects`)
setGeneric("simulate", function(object,...) standardGeneric("simulate"))

setMethod("simulate", "predped", function(object,
                                          max_agents = 20,
                                          iterations = 1800,
                                          add_agent_after = \(x) rnorm(x, 60, 15),
                                          goal_number = \(x) rnorm(x, 10, 2), 
                                          goal_duration = \(x) rnorm(x, 10, 2),
                                          radius = 0.2, 
                                          standing_start = 0.1,
                                          print_iteration = TRUE,
                                          close_enough = 2 * radius,
                                          space_between = radius,
                                          time_step = 0.5,
                                          ...) {

    # Simulate the iterations after which agents should be added to the simulation
    # (`add_agent`) and the number of goals each agent should pursue (`goal_number`).
    # For `add_agent`, we should furthermore create a cumulative sum, as we just 
    # need to iteration numbers themselves, not the gaps that are created by 
    # `number`
    add_agent_index <- draw_number(add_agent_after, iterations)
    add_agent_index <- c(1, cumsum(add_agent_index))
    add_agent_index <- add_agent_index[add_agent_index <= iterations]

    goal_number <- draw_number(goal_number, iterations)

    # If `goal_duration` is not a function, make it a function anyway (assumed
    # by the `goal` class: To be changed)
    if(typeof(goal_duration) != "closure") {
        number <- goal_duration[1]
        goal_duration <- function(x) number
    }

    # Initialize the trace and state lists. The state will already contain the 
    # initial condition. The trace list also contains this state. 
    state <- list("setting" = object@setting,
                  "agents" = list())
    trace <- list(state)

    # Loop over each iteration of the model
    for(i in seq_len(iterations)) {
        # Check whether to add a pedestrian and, if so, initiate a new 
        # agent. Things to consider are: whether it is time to add a new 
        # pedestrian, whether we already reached the maximal number of agents,
        # and whether there is any space to add the new pedestrian.
        if((i %in% add_agent_index) & (length(state$agents) < max_agents)) {
            potential_agent <- add_agent(object,
                                         object@setting,
                                         goal_number[i],
                                         goal_duration = goal_duration,
                                         radius = radius, 
                                         standing_start = standing_start,
                                         close_enough = close_enough,
                                         space_between = space_between,
                                         time_step = time_step)
            agent_in_cue <- TRUE
        }

        # Check whether there is any space to add the pedestrian. Otherwise
        # will have to keep waiting in the cue.
        if(agent_in_cue) {
            agents_in_the_way <- sapply(state$agents, 
                                       \(x) intersects(potential_agent, x))
            agent_in_cue <- any(agents_in_the_way)

            if(!agent_in_cue) {
                state$agents <- append(state$agents, potential_agent)
            }
        }

        # Provide feedback if wanted
        if(print_iteration) {
            print(paste0("Iteration: ", i, "; Number of agents: ", length(state$agents)))
        }

        # Update the current state
        state <- update_state(state, 
                              object@setting, 
                              close_enough = close_enough,
                              space_between = space_between,
                              time_step = time_step,
                              ...)

        # Check whether one of the pedestrians is waiting at the exit
        idx <- c()
        for(j in seq_along(state$agents)) {
            if(status(state$agents[[j]]) == "exit") {
                idx <- c(idx, j)
            }
        }

        if(length(idx) != 0) {
            state$agents <- state$agents[-idx]
        }

        # Save the new state in the trace
        trace[[i + 1]] <- state
    }
    
    return(trace)
})

#' Add an Agent to the Simulation
#' 
#' @param object The `predped` model that you want to simulate
#' @param goal_number Integer denoting the number of goals the agent should
#' receive.
#' @param goal_duration Function that determines the duration of each goal. 
#' Defaults to a function that draws `x` numbers from a normal distribution 
#' with mean 10 (5 sec) and standard deviation 2 (1 sec).
#' 
#' @export 
add_agent <- function(object,
                      background,
                      goal_number,
                      goal_duration = \(x) rnorm(x, 10, 2),
                      radius = 0.2,
                      standing_start = 0.1,
                      close_enough = 2 * radius,
                      space_between = radius,
                      time_step = 0.5) {

    # Sample a random set of parameters from the `predped` class
    idx <- sample(1:nrow(object@parameters), 1, prob = object@weights)

    # Create this agents' goal stack
    goal_stack <- generate_goal_stack(goal_number, 
                                      object@setting, 
                                      counter_generator = goal_duration)

    # Compute the agent's orientation: Perpendicular to the wall in which you 
    # have the entrance.
    angle <- perpendicular_orientation(background)

    # Create the agent themselves
    starting_position <- background@entrance + radius * c(cos(angle * pi / 180), sin(angle * pi / 180))
    tmp_agent <- agent(center = starting_position,
                       radius = radius,
                       speed = standing_start,
                       orientation = angle,
                       parameters = object@parameters[idx, -c(1,2)],
                       goals = goal_stack[-1],
                       current_goal = goal_stack[[1]],
                       color = object@parameters[idx, "Color"])

    # Adjust the preferred and slowing speeds of the agents based on the 
    # time_step (in seconds): These speeds are per second
    parameters(tmp_agent)[["sPref"]] <- parameters(tmp_agent)[["sPref"]] * time_step
    parameters(tmp_agent)[["sSlow"]] <- parameters(tmp_agent)[["sSlow"]] * time_step

    # Create the path to walk on for the current goal and return the agent
    current_goal(tmp_agent)@path <- find_path(current_goal(tmp_agent), 
                                              tmp_agent, 
                                              background,
                                              space_between = space_between)
    
    return(tmp_agent)
}

# Undocumented function because this is in no way a particularly beautiful 
# function, nor is it meant to be the final way in which we do this.
#
# What this function does is create a set of numbers based on either a function
# or on a numeric. These numbers should in the end represent integers and are 
# used to define the number of goals (`goal_number`) or the indices at which 
# an agent gets added to the simulation (`add_agent`).
#
# @param number Integer, vector of integers, or function that defines the numbers
# to be drawn and used in the simulation.
# @param iterations Integer denoting the number of iterations to be used in the 
# simulation
draw_number <- function(number, iterations) {
    # First check whether `number` is a function or a numeric
    if(typeof(number) == "closure") {
        # If it is a function, just draw `iterations` many numbers
        number <- number(iterations)
    } else if(is.numeric(number)) {
        # If it is a numeric, extend the already existing vector by 
        # `iterations/length(number)` times
        number <- rep(number, 
                      times = ceiling(iterations / length(number)))
    } else {
        stop("Cannot draw numbers if the type is not closure or numeric.")
    }

    # Given that we need positive integers, we first round up all values and 
    # replace each value lower than 1 with a 1. 
    number <- ceiling(number)
    number[number <= 0] <- 1

    return(as.integer(number))
}