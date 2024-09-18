#' Simulate the M4MA
#' 
#' This function allows users to simulate data from their specified `predped`
#' model. 
#' 
#' @param object The `predped` model that you want to simulate
#' @param max_agents Integer, vector, or function that defines the maximal number
#' of agents at each iteration in the simulation. Defaults to `20`.
#' @param iterations Integer denoting the number of iterations to run the 
#' simulation for. Defaults to `1800`, which corresponds to 15 minutes of 
#' simulation (each iterations is 500 msec).
#' @param add_agent_after Integer, vector of integers, or function that determines 
#' after how many iterations an agent gets added to the environment. Defaults to 
#' a function that draws `x` numbers from a normal distribution with mean 60 
#' (30 sec) and standard deviation 15 (7.5 sec).
#' @param standing_start Numeric denoting the speed of agents when they start 
#' moving. Defaults to `0.1`.
#' @param initial_agents List of agents with which to start the simulation. 
#' Defaults to `NULL`, meaning the simulation should start with no agents in the
#' room.
#' @param initial_condition State containing a setting and agents to start the 
#' simulation from. Defaults to `NULL`, meaning the simulation will start with 
#' no agents in the room.
#' @param initial_number_agents Integer denoting the number of agents that 
#' the simulation should start out with. Defaults to `NULL`, meaning the
#' simulation should start with no agents in the room. Ignored if `initial_agents`
#' is provided.
#' @param goal_number Integer, vector of integers, or function that determines 
#' how many goals each of the agents should receive. Defaults to a function that 
#' draws `x` numbers from a normal distribution with mean 10 and standard 
#' deviation 2. 
#' @param goal_duration Integer or function that determines the duration of each 
#' goal. Defaults to a function that draws `x` numbers from a normal distribution 
#' with mean 10 (5 sec) and standard deviation 2 (1 sec).
#' @param precompute_goal_paths Logical denoting whether to precompute all path
#' points beforehand. This means that the agent does not only preplan the way 
#' towards their next goal, but also to all goals that follow. Defaults to 
#' `FALSE`.
#' @param order_goal_stack Logical denoting whether to order the goal stack based
#' on the distance of the agent to the goal. Defaults to `TRUE`
#' @param precomputed_goals List of goal stacks from which the agent can be 
#' assigned one. Defaults to `NULL`, meaning that goal stacks should be created
#' in the simulation.
#' @param close_enough Numeric denoting how close (in radii) the agent needs to 
#' be to an object in order to interact with it. Defaults to `2`, meaning the 
#' agent can interact with objects at `2 * radius(agent)` distance away.
#' @param space_between Numeric denoting the space that should be left between 
#' an object and the created path points for the agents (in radii). Defaults to 
#' `1`, meaning a space of `1 * radius(agent)` is left between an object and the
#' path points agents use in their strategy.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to `0.5`, or half a second.
#' @param precompute_edges Logical denoting whether to precompute the path points
#' on which agents can move. Defaults to `TRUE`.
#' @param individual_differences Logical denoting whether variety on the parameters
#' should be accounted for (even within archetypes). Defaults to `TRUE`.
#' @param plot_live Logical denoting whether to plot each iteration while the 
#' simulation is going on. Defaults to `FALSE`.
#' @param plot_time Numeric denoting the amount of time (in seconds) to wait 
#' between iterations, i.e., the time between updating the plot. Defaults to 
#' `0.2`.
#' @param ... Arguments passed on to the \code{\link[predped]{update_state}} function.
#' 
#' @export
#
# TO DO
#   - At this moment, setting is kept separate from rest in trace. However, at 
#     some point, agents should be able to move things in the environment, meaning
#     we should keep a trace of moveable objects as well (either list in 
#     `setting` or a separate list as `moveable_objects`)
#   - Make it possible to have a mix of ordered agents (ordering goal_stacks 
#     beforehand) and chaotic agents (not ordering goal_stacks beforehand)
#   - At this moment, still assumed that agents are circular. Try to remove this
#     assumption in functions and in its definition
setGeneric("simulate", function(object,...) standardGeneric("simulate"))

setMethod("simulate", "predped", function(object,
                                          max_agents = 20,
                                          iterations = 1800,
                                          add_agent_after = \(x) rnorm(x, 60, 15),
                                          standing_start = 0.1,
                                          initial_agents = NULL,
                                          initial_condition = NULL,
                                          initial_number_agents = NULL,
                                          goal_number = \(x) rnorm(x, 10, 2), 
                                          goal_duration = \(x) rnorm(x, 10, 2),
                                          precompute_goal_paths = FALSE,
                                          order_goal_stack = TRUE,
                                          precomputed_goals = NULL,
                                          close_enough = 2,
                                          space_between = 2.5,
                                          time_step = 0.5,
                                          precompute_edges = TRUE,
                                          many_options = precompute_edges,
                                          individual_differences = TRUE,
                                          plot_live = FALSE,
                                          plot_time = 0.2,
                                          group_size = matrix(1, nrow = 1, ncol = 2),
                                          ...) {

    # Used to be an argument, but if `FALSE`, users have no way of terminating 
    # the process without killing the terminal. Hence not included as an argument
    # anymore.
    print_iteration <- TRUE

    # Simulate the iterations after which agents should be added to the simulation
    # (`add_agent`) and the number of goals each agent should pursue (`goal_number`).
    # For `add_agent`, we should furthermore create a cumulative sum, as we just 
    # need to iteration numbers themselves, not the gaps that are created by 
    # `number`
    add_agent_index <- draw_number(add_agent_after, iterations)
    add_agent_index <- c(1, cumsum(add_agent_index))
    add_agent_index <- add_agent_index[add_agent_index <= iterations]

    goal_number <- draw_number(goal_number, iterations)

    max_agents <- draw_number(max_agents, iterations)

    # If `goal_duration` is not a function, make it a function anyway (assumed
    # by the `goal` class: To be changed)
    if(typeof(goal_duration) != "closure") {
        number <- goal_duration[1]
        goal_duration <- function(x) number
    }

    # If the edges need to be precomputed, do so already and delete the mock 
    # position of agent and goal: These are the only dynamical components to 
    # this recomputation
    if(print_iteration & precompute_edges) {
        cat("\nPrecomputing edges")
    } else if(print_iteration) {
        cat("\n")
    }

    if(precompute_edges) {
        edges <- create_edges(c(0, 0), 
                              c(0, 0), 
                              object@setting,
                              space_between = space_between * max(params_bounds["radius",]),
                              many_options = many_options)

        edges$edges <- edges$edges[!(edges$edges$from %in% c("agent", "goal")),]
        edges$edges <- edges$edges[!(edges$edges$to %in% c("agent", "goal")),]
        edges$nodes <- edges$nodes[!(edges$nodes$node_ID %in% c("agent", "goal")),]
        edges$edges_with_coords <- edges$edges_with_coords[!(edges$edges_with_coords$from %in% c("agent", "goal")),]
        edges$edges_with_coords <- edges$edges_with_coords[!(edges$edges_with_coords$to %in% c("agent", "goal")),]

    } else {
        edges <- NULL
    }

    # If you want a number of agents to be there at the start, and you don't 
    # have an initial condition yet, generate several agents that stand on 
    # random positions in the environment.
    if(is.null(initial_agents) & !is.null(initial_number_agents)) {
        initial_agents <- create_initial_condition(initial_number_agents,
                                                   object,
                                                   goal_number[1:initial_number_agents],
                                                   goal_duration = goal_duration,
                                                   standing_start = standing_start,
                                                   space_between = space_between,
                                                   time_step = time_step,
                                                   precomputed_edges = edges,
                                                   precompute_goal_paths = precompute_goal_paths,
                                                   order_goal_stack = order_goal_stack,
                                                   precomputed_goals = precomputed_goals,
                                                   individual_differences = individual_differences,
                                                   group_size = group_size)

        # First index deleted here so that agents don't immediately get added 
        # to the environment when the initial condition is to be generated 
        # (ensures only `initial_number_agents` are in the first state).
        add_agent_index <- add_agent_index[-1]
    }

    # Initialize the trace and state lists. The state will already contain the 
    # initial condition. The trace list also contains this state. 
    state <- predped::state(iteration = 0, 
                            setting = object@setting, 
                            agents = list())
    if(!is.null(initial_agents)) {
        agents(state) <- initial_agents
        
    } else if(!is.null(initial_condition)) {
        if(!identical(initial_condition$setting, state@setting)) {
            stop(paste0("Setting in the `predped` model is not the same as the ",
                        "setting in the initial condition. ", 
                        "Please make sure the initial condition is compatible ",
                        "with your model."))
        }

        agents(state) <- initial_condition$agents
    }
    trace <- list(state)

    agent_in_queue <- FALSE
    rerouted <- 0
    planned <- 0
    
    # Loop over each iteration of the model
    for(i in seq_len(iterations)) {
        # Check whether to add a pedestrian and, if so, initiate a new 
        # agent. Things to consider are: whether it is time to add a new 
        # pedestrian, whether we already reached the maximal number of agents,
        # and whether there is any space to add the new pedestrian. If there is 
        # already an agent waiting, don't create a new one.
        if((i %in% add_agent_index) & (length(state@agents) < max_agents[i] & !agent_in_queue)) {
            # Sample how many agents you would like to generate (if group, 
            # generate them all together)
            if(nrow(group_size) == 1) {
                number_agents <- group_size[,1]
            } else {
                number_agents <- sample(group_size[,1], 1, replace = TRUE, prob = group_size[,2])
            }

            # Generate as many potential agents as you need to form a group of 
            # size `number_agents`
            potential_agent <- list()
            for(j in 1:number_agents) {
                potential_agent[[j]] <- add_agent(object,
                                                  goal_number[i],
                                                  goal_duration = goal_duration,
                                                  standing_start = standing_start,
                                                  space_between = space_between,
                                                  time_step = time_step,
                                                  precomputed_edges = edges,
                                                  precompute_goal_paths = precompute_goal_paths,
                                                  order_goal_stack = order_goal_stack,
                                                  precomputed_goals = precomputed_goals,
                                                  individual_differences = individual_differences)
                group(potential_agent[[j]]) <- length(state@agents) + 1

                # Once done, we need to provide all agents with the same set of 
                # goals. Currently imposed so that a group of people that enter
                # a space also want to achieve the same goals.
                #
                # Furthermore change position and orientation so that they all 
                # come in in the same place
                if(j != 1) {
                    position(potential_agent[[j]]) <- position(potential_agent[[1]])
                    orientation(potential_agent[[j]]) <- orientation(potential_agent[[1]])

                    goals(potential_agent[[j]]) <- goals(potential_agent[[1]])
                    current_goal(potential_agent[[j]]) <- current_goal(potential_agent[[1]])
                }
            }

            agent_in_queue <- TRUE
        }

        # Check whether there is any space to add the pedestrian. Otherwise
        # will have to keep waiting in the cue.
        if(agent_in_queue) {
            agents_in_the_way <- sapply(state@agents, 
                                        \(x) in_object(potential_agent[[1]], 
                                                       points(x), 
                                                       outside = FALSE))
            if(!any(agents_in_the_way)) {
                agents(state) <- append(agents(state), potential_agent[[1]])
                potential_agent <- potential_agent[-1]
            }

            agent_in_queue <- length(potential_agent) != 0
        }

        # Provide feedback if wanted
        if(print_iteration) {
            cat(paste0("\rIteration: ", i, "; Number of agents: ", length(agents(state))))
        }

        # Update the current state
        state <- update_state(state, 
                              object@setting, 
                              space_between = space_between,
                              time_step = time_step,
                              precomputed_edges = edges,
                              standing_start = standing_start,
                              many_options = many_options,
                              ...)

        # Check whether one of the pedestrians is waiting at the exit
        idx <- c()
        for(j in seq_along(agents(state))) {
            if(status(state@agents[[j]]) == "exit") {
                idx <- c(idx, j)
            }
        }

        if(length(idx) != 0) {
            agents(state) <- state@agents[-idx]
        }

        # Save the new state in the trace
        trace[[i + 1]] <- state

        # If you want to plot the result immediately, do so
        if(plot_live) {
            print(plot(list(state), 
                       trace = TRUE,
                       print_progress = FALSE,
                       iterations = i)[[1]])
            Sys.sleep(plot_time)
        }
    }

    if(print_iteration) {
        cat("\n")
    }
    
    return(trace)
})

#' Add an Agent to the Simulation
#' 
#' @param object The `predped` model that you want to simulate
#' @param goal_number Integer, vector of integers, or function that determines 
#' how many goals each of the agents should receive. Defaults to a function that 
#' draws `x` numbers from a normal distribution with mean 10 and standard 
#' deviation 2. 
#' @param goal_duration Integer or function that determines the duration of each 
#' goal. Defaults to a function that draws `x` numbers from a normal distribution 
#' with mean 10 (5 sec) and standard deviation 2 (1 sec).
#' @param position Vector denoting the position of the agent you wish to create.
#' Defaults to `NULL`, meaning the agent's location will be at the entrance of 
#' the room.
#' @param standing_start Numeric denoting the speed of agents when they start 
#' moving. Defaults to `0.1`.
#' @param space_between Numeric denoting the space that should be left between 
#' an object and the created path points for the agents (in radii). Defaults to 
#' `1`, meaning a space of `1 * radius(agent)` is left between an object and the
#' path points agents use in their strategy.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to `0.5`, or half a second.
#' @param precomputed_edges List of nodes and edges that can be used by the 
#' path-finding algorithms under the hood. Defaults to `NULL`, meaning edges 
#' have not been precomputed.
#' @param precompute_goal_paths Logical denoting whether to precompute all path
#' points beforehand. This means that the agent does not only preplan the way 
#' towards their next goal, but also to all goals that follow. Defaults to 
#' `FALSE`.
#' @param order_goal_stack Logical denoting whether to order the goal stack based
#' on the distance of the agent to the goal. Defaults to `TRUE`
#' @param precomputed_goals List of goal stacks from which the agent can be 
#' assigned one. Defaults to `NULL`, meaning that goal stacks should be created
#' in the simulation.
#' @param individual_differences Logical denoting whether variety on the parameters
#' should be accounted for (even within archetypes). Defaults to `TRUE`.
#' 
#' @export 
#
# TO DO
#   - Allow for optional "position" and "orientation" arguments. Will make it 
#     easier to create initial conditions, as goals are immediately computed 
#     then
add_agent <- function(object,
                      goal_number,
                      goal_duration = \(x) rnorm(x, 10, 2),
                      position = NULL,
                      standing_start = 0.1,
                      space_between = 2.5,
                      time_step = 0.5,
                      precomputed_edges = NULL,
                      precompute_goal_paths = TRUE,
                      order_goal_stack = TRUE,
                      precomputed_goals = NULL,
                      individual_differences = TRUE) {

    # Extract the background from the `predped` model and determine where the 
    # agent will enter the space
    background <- object@setting

    idx <- sample(seq_len(nrow(background@entrance)), 1)
    entrance_agent <- entrance(background)[idx, ]

    # Sample a random set of parameters from the `predped` class. From this, 
    # extract the needed information and add some individual differences
    idx <- sample(1:nrow(object@parameters), 1, prob = object@weights)
    color <- object@parameters$color[idx]

    params <- draw_parameters(1, 
                              object@parameters[idx,],
                              archetype = object@parameters$name[idx],
                              individual_differences = individual_differences)    
    radius <- params$radius

    # Create this agents' goal stack
    if(is.null(precomputed_goals)) {
        goal_stack <- generate_goal_stack(goal_number, 
                                          background, 
                                          counter_generator = goal_duration,
                                          precomputed_edges = precomputed_edges,
                                          agent_position = position,
                                          precompute_goal_paths = precompute_goal_paths,
                                          space_between = space_between * radius,
                                          order_goal_stack = order_goal_stack)
    } else {
        i <- sample(1:length(precomputed_goals), 1)
        goal_stack <- precomputed_goals[[i]]
    }

    # Determine the agent's orientation. Either perpendicular to the wall in 
    # the agent enters, or directed towards the current goal of the agent.
    if(is.null(position)) {
        angle <- perpendicular_orientation(shape(background),
                                           entrance_agent)
    } else {
        co_1 <- position
        co_2 <- goal_stack[[1]]@position

        angle <- atan2(co_2[2] - co_1[2], co_2[1] - co_1[1]) * 180 / pi
    }    

    # Determine the position of the agent. Either this is at the entrance, or 
    # this is at the specified location
    if(is.null(position)) {
        position <- entrance_agent + 1.05 * radius * c(cos(angle * pi / 180), sin(angle * pi / 180))
    }
    
    # Create the agent itself
    tmp_agent <- agent(center = position,
                       radius = radius,
                       speed = standing_start * params[["preferred_speed"]],
                       orientation = angle,
                       parameters = params,
                       goals = goal_stack[-1],
                       current_goal = goal_stack[[1]],
                       color = color)

    # Create the path to walk on for the current goal and return the agent. Given 
    # that nothing new is put in the environment, we can put `reevaluate` to 
    # FALSE
    current_goal(tmp_agent)@path <- find_path(current_goal(tmp_agent), 
                                              tmp_agent, 
                                              background,
                                              precomputed_edges = precomputed_edges,
                                              reevaluate = FALSE)
    # current_goal(tmp_agent)@path <- matrix(current_goal(tmp_agent)@position, 
    #                                        nrow = 1, 
    #                                        ncol = 2)
    
    return(tmp_agent)
}

#' Create an Initial Condition
#' 
#' Create a list that contains agents at random locations within the setting.
#' 
#' @param initial_number_agents Integer denoting the number of agents that 
#' the simulation should start out with.
#' @param object The `predped` model that you want to simulate
#' @param goal_number Integer, vector of integers, or function that determines 
#' how many goals each of the agents should receive. 
#' @param goal_duration Integer or function that determines the duration of each 
#' goal. Defaults to a function that draws `x` numbers from a normal distribution 
#' with mean 10 (5 sec) and standard deviation 2 (1 sec).
#' @param standing_start Numeric denoting the speed of agents when they start 
#' moving. Defaults to `0.1`.
#' @param space_between Numeric denoting the space that should be left between 
#' an object and the created path points for the agents (in radii). Defaults to 
#' `1`, meaning a space of `1 * radius(agent)` is left between an object and the
#' path points agents use in their strategy.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to `0.5`, or half a second.
#' @param precomputed_edges List of nodes and edges that can be used by the 
#' path-finding algorithms under the hood. Defaults to `NULL`, meaning edges 
#' have not been precomputed.
#' @param precompute_goal_paths Logical denoting whether to precompute all path
#' points beforehand. This means that the agent does not only preplan the way 
#' towards their next goal, but also to all goals that follow. Defaults to 
#' `FALSE`.
#' @param order_goal_stack Logical denoting whether to order the goal stack based
#' on the distance of the agent to the goal. Defaults to `TRUE`
#' @param precomputed_goals List of goal stacks from which the agent can be 
#' assigned one. Defaults to `NULL`, meaning that goal stacks should be created
#' in the simulation.
#' @param individual_differences Logical denoting whether variety on the parameters
#' should be accounted for (even within archetypes). Defaults to `TRUE`.
#' @param group_size Matrix of size n x 2 containing the group sizes you would 
#' like to simulate (first column) and the probability of observing groups with 
#' this size (second column). Defaults to a 100% probability of observing single 
#' agents.
#' 
#' @export 
create_initial_condition <- function(initial_number_agents,
                                     object,
                                     goal_number,
                                     goal_duration = \(x) rnorm(x, 10, 2),
                                     standing_start = 0.1,
                                     space_between = 1,
                                     time_step = 0.5,
                                     precomputed_edges = NULL,
                                     precompute_goal_paths = TRUE,
                                     order_goal_stack = TRUE,
                                     precomputed_goals = NULL,
                                     individual_differences = TRUE,
                                     group_size = matrix(1, nrow = 1, ncol = 2)) {

    # Copy the setting
    setting <- object@setting

    # Make sure you have enough goal-numbers for each of the agents
    goal_number <- draw_number(goal_number, initial_number_agents)

    # If `goal_duration` is not a function, make it a function anyway (assumed
    # by the `goal` class: To be changed)
    if(typeof(goal_duration) != "closure") {
        number <- goal_duration[1]
        goal_duration <- function(x) number
    }

    # Check whether the weights in `group_size` sum to 1. If not, correct this
    if(sum(group_size[,2]) != 1) {
        group_size[,2] <- group_size[,2] / sum(group_size[,2])
    }

    if(!is.numeric(sum(group_size[,2])) | !is.finite(sum(group_size[,2]))) {
        stop("Weights of the `group_size` are not numeric. Please adjust.")
    }

    # Use `group_size` to create the group memberships of all agents that are 
    # going to be created. Here, we first simulate the group sizes that we 
    # would like. Then, we create the indices at which group assignment should 
    # change (i.e., if you start with a group of two, then the group number 
    # should change after agent 2; see below)
    if(nrow(group_size) == 1) {
        groups <- rep(group_size[,1], initial_number_agents)
    } else {
        groups <- sample(group_size[,1], 
                         initial_number_agents, 
                         replace = TRUE,
                         prob = group_size[,2])
    }
    group_indices <- cumsum(groups)

    # Loop over the agents and use `add_agent` to create an initial agent. Note
    # that we have to change some of the characteristics of these agents, 
    # namely their location and their orientation, as `add_agent` assumes that
    # agents start at the entrance walking into the setting.
    group_number <- 1
    agents <- list() ; stop <- FALSE
    for(i in seq_len(initial_number_agents)) {
        # Initial agent to create
        new_agent <- add_agent(object, 
                               goal_number[i], 
                               goal_duration = goal_duration,
                               standing_start = standing_start,
                               space_between = space_between,
                               time_step = time_step,
                               precomputed_edges = precomputed_edges,
                               precompute_goal_paths = precompute_goal_paths,
                               order_goal_stack = order_goal_stack,
                               precomputed_goals = precomputed_goals,
                               individual_differences = individual_differences)
        group(new_agent) <- group_number 

        # Update the group number for the future if `i` is in the indices that 
        # indicate a change in group membership
        if(i %in% group_indices) {
            group_number <- group_number + 1
        }

        # Extract the edges from the background. Will help in determining the locations
        # at which the agents can be gathered. Importantly, dense network created so 
        # that there are many potential positions for the agents, even when there 
        # are not many objects in the environment
        edges <- create_edges(c(0, 0), 
                              c(0, 0), 
                              setting,
                              space_between = space_between * size(new_agent),
                              many_options = TRUE)

        edges$edges <- edges$edges[!(edges$edges$from %in% c("agent", "goal")),]
        edges$edges <- edges$edges[!(edges$edges$to %in% c("agent", "goal")),]
        edges$nodes <- edges$nodes[!(edges$nodes$node_ID %in% c("agent", "goal")),]

        # Additional check to see if there are enough edges to place agents on
        if(nrow(edges$edges) == 0) {
            message(paste0("Couldn't add any new agents after ", 
                           length(agents), 
                           " due to crowdiness."))
            break
        }

        # Choose a random edge on which the agent will stand and create the 
        # exact position.
        success <- FALSE ; iter <- 0
        position <- NULL
        while(!success) {
            # Check whether you overflow the number of iterations. If so, then 
            # we stop in our tracks, break out of the loop, and give a message 
            # on this
            if(iter > 10) {
                message(paste0("Couldn't add new agent after 10 attempts. ", 
                               "Instead of creating an initial condition with ", 
                               initial_number_agents, 
                               " agents, only ", 
                               length(agents), 
                               " agents will be used in the initial condition."))
                stop <- TRUE
                break
            }

            # Sample a random edge on which the agent will stand
            idx <- sample(1:nrow(edges$edges), 1)

            # Get the coordinates of the two points that make up this edge
            co_1 <- edges$nodes[edges$nodes$node_ID == edges$edges$from[idx], c("X", "Y")]
            co_2 <- edges$nodes[edges$nodes$node_ID == edges$edges$to[idx], c("X", "Y")]

            # Generate several alternative positions along this edge on which the 
            # agent can stand and bind them into a matrix
            n_agents_fit <- sqrt((co_1$X - co_2$X)^2 + (co_1$Y - co_2$Y)^2) / size(new_agent)

            if(any(is.na(n_agents_fit))) {
                browser()
            }
            
            alternatives <- cbind(seq(co_1$X, co_2$X, length.out = floor(n_agents_fit)),
                                  seq(co_1$Y, co_2$Y, length.out = floor(n_agents_fit)))

            # Check which position are accessible for the agent
            dummy <- agent(center = c(0, 0), radius = size(new_agent))

            check <- rep(TRUE, each = nrow(alternatives))
            check <- overlap_with_objects(dummy, 
                                          setting,
                                          alternatives, 
                                          check)

            if(any(check)) {
                idx <- which(check)
                idx <- sample(idx, 1)
                
                new_position <- alternatives[idx,]

                success <- TRUE
            }

            # Increase the iteration number
            iter <- iter + 1
        }
        position(new_agent) <- new_position

        # Let the agent face the way of its goal
        co_1 <- position(new_agent)
        co_2 <- current_goal(new_agent)@path[1,]

        orientation(new_agent) <- atan2(co_2[2] - co_1[2], co_2[1] - co_1[1]) * 180 / pi

        # If you need to stop, break out of the loop
        if(stop) {
            break
        }

        # Put the agent in the `agents` list and continue
        agents[[i]] <- new_agent
        setting@objects <- append(setting@objects, new_agent)
    }    
    
    return(agents)
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