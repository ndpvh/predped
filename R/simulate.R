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
#' 
#' @export
setGeneric("simulate", function(object,...) standardGeneric("simulate"))

setMethod("simulate", "predped", function(object,
                                          max_agents = 20,
                                          iterations = 1800,
                                          add_agent_after = \(x) rnorm(x, 60, 15),
                                          goal_number = \(x) rnorm(x, 10, 2), 
                                          goal_duration = \(x) rnorm(x, 10, 2)) {

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
    state <- list("setting" = list(object@setting),
                  "agents" = list())
    trace <- list(state)

    # Loop over each iteration of the model
    for(i in seq_len(iterations)) {
        # Check whether to add a pedestrian and, if so, initiate a new 
        # agent
        if((i %in% add_agent_index) & (length(state$agents) <= max_agents)) {
            state$agents <- append(state$agentsagents, 
                                   add_agent(object,
                                             object@setting,
                                             goal_number[i],
                                             goal_duration = goal_duration))
        }

        # Update the current state
        state <- update_state(state)

        # Check whether one of the pedestrians is waiting at the exit
        idx <- c()
        for(j in seq_along(state$agents)) {
            if(state$agents[[j]]@status == "exit") {
                idx <- c(idx, j)
            }
        }
        state$agents <- state$agents[-idx]

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
                      standing_start = 0.2) {
    # Sample a random set of parameters from the `predped` class
    idx <- sample(1:nrow(object@parameters), 1, prob = object@weights)

    # Create this agents' goal stack
    goal_stack <- generate_goal_stack(goal_number, 
                                      object@setting, 
                                      counter_generator = goal_duration)

    # Compute the agent's orientation: Perpendicular to the wall in which you 
    # have the entrance.
    angle <- perpendicular_orientation(background)

    return(agent(center = background@entrance,
                 radius = radius,
                 speed = standing_start,
                 orientation = angle,
                 parameters = object@parameters[idx, -c(1,2)],
                 goals = goal_stack[-1],
                 current_goal = goal_stack[[1]]))
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

# Undocumented function because this is in no way a particularly beautiful 
# function, nor is it meant to be the final way in which we do this.
#
# It takes in the background as an object and computes the orientation the 
# agent needs to walk in in order to walk perpendicular to the entrance they 
# just came from.
#
# @param background Object of the background class.
perpendicular_orientation <- function(background) {
    # Find out where the entrance lies in the background
    # For this, we compute the sum of the distances between each of 
    # the points that make up an edge and the entrance point. The edge that has 
    # the distance closest to the actual size of the edge will be taken as the 
    # entrance wall.
    co <- entrance(background)@.Data
    points <- shape(background)@points
    points <- cbind(points, points[c(2:nrow(points), 1),]) # Make a 4-columned matrix with (x1, y1) and (x2, y2)

    distances <- cbind(sqrt((points[,1] - points[,3])^2 + (points[,2] - points[,4])^2),
                       sqrt((points[,1] - co[1])^2 + (points[,2] - co[2])^2),
                       sqrt((points[,3] - co[1])^2 + (points[,4] - co[2])^2))
    distances <- distances[,1] - distances[,2] - distances[,3]

    # Now that we know on which edge the entrance lies, we can compute the 
    # perpendicular orientation to this edge. Approach computes the orientation 
    # of the line defined by the entrance and the edge of the wall that contains 
    # it and then either subtracts (clockwise == TRUE) or adds (clockwise == FALSE)
    # 90 degrees from it. The formula to do this is simply tan^{-1} (slope), 
    # where slope is the slope of the edge
    edge <- as.numeric(points[which.min(distances),])
    slope <- (edge[2] - edge[4]) / (edge[1] - edge[3])

    angle <- atan(slope) * 180 / pi     # Conversion from radians to degrees
    if(background@shape@clock_wise) {
        angle <- angle - 90
    } else {
        angle <- angle + 90
    }

    # If you have a negative angle, convert this to a positive one
    if(angle < 0) {
        angle <- angle + 360
    }

    return(angle)
}
