# Create the generic for updating, but make the documentation specific to each 
# type
# 
# TO DO
#   - Also update objects when they are moveable and used. To see how we do it
setGeneric("update", function(object, ...) standardGeneric("update"))

#' Update State
#' 
#' Update the current state by updating all of the agents that are contained 
#' in it. 
#' 
#' @param object Object of the \code{\link[predped]{state-class}}.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to \code{0.5}, or half a second.
#' @param stay_stopped Logical denoting whether agents will predict others that 
#' are currently not moving to remain immobile in the next iteration. Defaults 
#' to \code{TRUE}.
#' @param cpp Logical denoting whether to use the Rcpp alternatives for several
#' of the lower-level functions (\code{TRUE}) or whether to use the R alternatives
#' instead (\code{FALSE}). Defaults to \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link[predped]{update-agent}}.
#' 
#' @return Object of the \code{\link[predped]{state-class}}.
#' 
#' @docType method
#' 
#' @seealso 
#' \code{\link[predped]{create_agent_specifications}},
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{simulate,state-method}},
#' \code{\link[predped]{update,agent-method}}
#' 
#' @rdname update-state
#' 
#' @export 
setMethod("update", "state", function(object,
                                      time_step = 0.5,
                                      stay_stopped = TRUE, 
                                      cpp = TRUE,
                                      ...) {

    # Extract the components of the state
    agent_list <- agents(object)
    background <- setting(object)

    # Create agent-specifications. Are used in the utility-function and used to
    # be created there. Moved it here to reduce computational cost (which increases
    # exponentially with more agents)
    agent_specs <- create_agent_specifications(agent_list, 
                                               stay_stopped = stay_stopped, 
                                               time_step = time_step,
                                               cpp = cpp)

    # Extract objects and shape of the environment
    obj <- objects(background)
    shp <- shape(background)

    # Check whether each agent can see the current goal. This is used in "plan", 
    # "reroute", and "move". Based on (a) whether any objects can be in the way 
    # and (b) the shape of the room. With regard to the latter, we note that an 
    # agent is guaranteed to see their goal if the room is regular (rectangle or 
    # circle) and no objects are in the way. 
    #
    # This is a necessary part of the code that prevents agents from getting 
    # stuck
    # if(length(obj) == 0 & (inherits(shp, "rectangle") | inherits(shp, "circle"))) {
    if((length(obj) == 0) | (length(agent_list) == 0)) {
        seen <- rep(TRUE, length(agent_list))
    } else {
        edges <- sapply(agent_list, 
                        function(x) {
                            if(nrow(current_goal(x)@path) < 1) {
                                return(c(position(x), rep(0, 2)))
                            } else {
                                return(c(position(x), current_goal(x)@path[1, ]))
                            }
                        })
        edges <- t(edges)

        seen_1 <- sapply(agent_list, \(x) nrow(current_goal(x)@path) >= 1)
        seen_2 <- prune_edges(obj, edges)

        seen <- seen_1 & seen_2
    }

    # Loop over each agent in the simulation and update their position with the 
    # `update_agent` function
    state_copy <- object
    for(i in seq_along(agent_list)) {
        # Extract the agent to-be-updated from the state list. Importantly, also
        # remove the agent from this state list, as it should not contain this 
        # one agent: Simulation is done relative to the agent to-be-updated
        agent <- agent_list[[i]]
        agents(state_copy) <- agent_list[-i]

        # Update the utility variables slot for the agents, as we don't want 
        # carry-over effects in this variable
        agent@utility_variables <- data.frame(agent_idx = i,
                                              check = NA,
                                              ps_speed = NA, 
                                              ps_distance = NA, 
                                              gd_angle = NA, 
                                              id_distance = NA,
                                              id_check = NA,
                                              id_ingroup = NA,
                                              ba_angle = NA,
                                              ba_cones = NA,
                                              fl_leaders = NA,
                                              wb_buddies = NA,
                                              gc_distance = NA,
                                              gc_radius = NA, 
                                              gc_nped = NA,
                                              vf_angles = NA)

        # Update the agent himself
        agent_list[[i]] <- update(agent, 
                                  state_copy,
                                  background,
                                  agent_specs,
                                  seen[i],
                                  cpp = cpp,
                                  ...)
    }

    # Update the state
    agents(object) <- agent_list
    return(object)
})

#' Update Agent
#' 
#' Update the current agent by first updating its goal status (i.e., checking 
#' how and whether an agent can start interacting with a goal, has to reorient, 
#' etc) and then updating its position (i.e., using the utility functions to 
#' determine where the agent will move next).
#' 
#' @param object Object of the \code{\link[predped]{agent-class}}.
#' @param state Object of the \code{\link[predped]{state-class}}.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param agent_specifications List created by the 
#' \code{\link[predped]{create_agent_specifications}} function. Contains all 
#' information of all agents within the current \code{state} and allows for the
#' communication between the \code{predped} simulation functions and the 
#' \code{m4ma} utility functions.
#' @param seen Logical indicating whether the agent can see the path point 
#' to which they are currently moving.
#' @param velocities Numeric matrix containing the change in speed for an agent
#' whenever they move to the respective cell of this matrix. Is used to create 
#' the cell positions that the agent might move to, as performed through 
#' \code{\link[m4ma]{c_vd_rcpp}}. Currently limited to having 11 rows (direction) 
#' and 3 columns (speed). Defaults to a matrix in which the columns contain 
#' \code{1.5} (acceleration), \code{1}, and \code{0.5}.
#' @param orientations Numeric matrix containing the change in direction for an 
#' agent whenever they move to the respective cell of this matrix. Is used to 
#' create the cell positions that the agent might move to, as performed through
#' \code{\link[m4ma]{c_vd_rcpp}}. Currently limited to having 11 rows (direction)
#' and 3 columns (speed). Defaults to a matrix in which the rows contain 
#' \code{72.5}, \code{50}, \code{32.5}, \code{20}, \code{10}, code{0}, \code{350}, 
#' \code{340}, \code{327.5}, \code{310}, \code{287.5} (note that the larger 
#' angles are actually the negative symmetric versions of the smaller angles).
#' @param standing_start Numeric denoting the factor of their preferred speed 
#' that agents move when they just came from standing still. Defaults to 
#' \code{0.1}.
#' @param close_enough Numeric denoting how close (in radii) the agent needs to 
#' be to an object in order to interact with it. Defaults to \code{2}, meaning the 
#' agent can interact with objects at \code{2 * radius(agent)} distance away.
#' @param space_between Numeric denoting the space that should be left between 
#' an object and the created path points for the agents (in radii). Defaults to 
#' \code{2.5}, meaning a space of \code{2.5 * radius(agent)} is left between an 
#' object and the path points agents use in their strategy.
#' @param precomputed_edges Output of \code{\link[predped]{compute_edges}} 
#' containing the nodes and edges the agent can use to plan its path. Defauls 
#' to \code{NULL}, triggering the creation of these edges whenever they are 
#' needed.
#' @param many_nodes Logical denoting whether to use the minimal number of nodes
#' or to use many more (see \code{\link[predped]{create_edges}}). Ignored if 
#' \code{precomputed_edges} is provided. Defaults to \code{FALSE}.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to \code{0.5}, or half a second.
#' @param report Logical denoting whether to report whenever an agent is 
#' reorienting. Defaults to \code{FALSE}, and is usually not needed as feedback.
#' @param print_iteration Logical denoting whether to report each simulated
#' iteration. Defaults to \code{FALSE}, but can be switched off if desired.
#' @param cpp Logical denoting whether to use the Rcpp alternatives for several
#' of the lower-level functions (\code{TRUE}) or whether to use the R alternatives
#' instead (\code{FALSE}). Defaults to \code{TRUE}.
#' 
#' @return Object of the \code{\link[predped]{agent-class}}.
#' 
#' @seealso 
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{simulate,state-method}},
#' \code{\link[predped]{update,state-method}},
#' \code{\link[predped]{update_goal}},
#' \code{\link[predped]{update_position}}
#' 
#' @docType method
#' 
#' @rdname update-agent
#' 
#' @export 
setMethod("update", "agent", function(object,
                                      state,
                                      background,
                                      agent_specifications,
                                      seen,
                                      velocities = c(1.5, 1, 0.5) |>
                                          rep(each = 11) |>
                                          matrix(ncol = 3),
                                      orientations = c(72.5, 50, 32.5, 20, 10, 0, 
                                                       350, 340, 327.5, 310, 287.5) |>
                                          rep(times = 3) |>
                                          matrix(ncol = 3),
                                      close_enough = 2, 
                                      space_between = 1.25,
                                      precomputed_edges = NULL,
                                      many_nodes = !is.null(precomputed_edges),
                                      standing_start = 0.1,
                                      time_step = 0.5,
                                      report = FALSE,
                                      print_iteration = FALSE,
                                      cpp = TRUE) {

    # Update the goals of the agent
    object <- update_goal(object, 
                          state, 
                          background,
                          seen,
                          standing_start = standing_start,     
                          close_enough = close_enough,
                          space_between = space_between,                     
                          precomputed_edges = precomputed_edges,
                          many_nodes = many_nodes,
                          report = report,
                          print_iteration = print_iteration,
                          cpp = cpp) 

    # Update the position of the agent
    object <- update_position(object, 
                              state,
                              background,
                              agent_specifications, # Keep all agents in here: predClose makes use of own prediction as well
                              velocities = velocities,
                              orientations = orientations,
                              standing_start = standing_start,
                              time_step = time_step,
                              report = report,
                              print_iteration = print_iteration,
                              cpp = cpp) 
    
    return(object)
})

#' Update the Position of an Agent
#' 
#' @param object Object of the \code{\link[predped]{agent-class}}.
#' @param state Object of the \code{\link[predped]{state-class}}.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param agent_specifications List created by the 
#' \code{\link[predped]{create_agent_specifications}} function. Contains all 
#' information of all agents within the current \code{state} and allows for the
#' communication between the \code{predped} simulation functions and the 
#' \code{m4ma} utility functions.
#' @param velocities Numeric matrix containing the change in speed for an agent
#' whenever they move to the respective cell of this matrix. Is used to create 
#' the cell positions that the agent might move to, as performed through 
#' \code{\link[m4ma]{c_vd_rcpp}}. Currently limited to having 11 rows (direction) 
#' and 3 columns (speed). Defaults to a matrix in which the columns contain 
#' \code{1.5} (acceleration), \code{1}, and \code{0.5}.
#' @param orientations Numeric matrix containing the change in direction for an 
#' agent whenever they move to the respective cell of this matrix. Is used to 
#' create the cell positions that the agent might move to, as performed through
#' \code{\link[m4ma]{c_vd_rcpp}}. Currently limited to having 11 rows (direction)
#' and 3 columns (speed). Defaults to a matrix in which the rows contain 
#' \code{72.5}, \code{50}, \code{32.5}, \code{20}, \code{10}, code{0}, \code{350}, 
#' \code{340}, \code{327.5}, \code{310}, \code{287.5} (note that the larger 
#' angles are actually the negative symmetric versions of the smaller angles).
#' @param standing_start Numeric denoting the factor of their preferred speed 
#' that agents move when they just came from standing still. Defaults to 
#' \code{0.1}.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to \code{0.5}, or half a second.
#' @param report Logical denoting whether to report whenever an agent is 
#' reorienting. Defaults to \code{FALSE}, and is usually not needed as feedback.
#' @param print_iteration Logical denoting whether to report each simulated
#' iteration. Defaults to \code{FALSE}, but can be switched off if desired.
#' @param cpp Logical denoting whether to use the Rcpp alternatives for several
#' of the lower-level functions (\code{TRUE}) or whether to use the R alternatives
#' instead (\code{FALSE}). Defaults to \code{TRUE}.
#' 
#' @return Object of the \code{\link[predped]{agent-class}}.
#' 
#' @seealso 
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{simulate,state-method}},
#' \code{\link[predped]{update,agent-method}},
#' \code{\link[predped]{update,state-method}},
#' \code{\link[predped]{update_goal}}
#' 
#' @rdname update_position
#' 
#' @export 
#
# TO DO
#   - At this moment, the same checks happen here and in `best_angle`: Try to
#     find a way to decrease this burden (`move_options`). Happesn twice in this
#     function alone: Once for initial computation, and once when this initial
#     computation does not pan out
#   - Do we want to use the agent@busy for the different options?
#   - Allow plotGrid and printChoice to happen in an object-based way
#   - In the end, again centers computed as many times before (in the utility
#     function): Do the updating more generally and more streamlined so
#     everything becomes a lot clearer
update_position <- function(agent,
                            state,
                            background,
                            agent_specifications,
                            velocities = c(1.5, 1, 0.5) |>
                               rep(each = 11) |>
                               matrix(ncol = 3),
                            orientations = c(72.5, 50, 32.5, 20, 10, 0, 
                                             350, 340, 327.5, 310, 287.5) |>
                                rep(times = 3) |>
                                matrix(ncol = 3),
                            standing_start = 0.1,
                            time_step = 0.5,
                            report = TRUE,
                            print_iteration = TRUE,
                            cpp = TRUE) {

    standing_start <- standing_start * parameters(agent)[["preferred_speed"]]

    idx <- limit_access(background, agent)
    objects(background) <- append(objects(background), 
                                  background@precomputed_limited_access[idx])

    # Let the agent wait (cell = 0 and speed is slowest possible one) when the 
    # agent is currently interacting with another object, when they are currently 
    # rerouting, when they are currently planning a route to their goal, or when 
    # they are waiting for another agent.
    if(status(agent) %in% c("completing goal", "exit", "reroute", "plan", "wait")) { 
        cell(agent) <- 0
        speed(agent) <- standing_start * parameters(agent)[["preferred_speed"]]
                        
    # If the agent has stopped their interaction, check whether they already know
    # where to go to (i.e., whether they are oriented towards their new path
    # point). If not, let them reorient themselves towards their next goal.
    } else if(status(agent) == "reorient") {        
        orientation(agent) <- best_angle(agent, 
                                         state, 
                                         background,
                                         agent_specifications, 
                                         velocities, 
                                         orientations,
                                         cpp = cpp)

        # Report the degress that the agent is reorienting to
        turn <- paste("towards", orientation(agent), "degrees")
        if(report & print_iteration) {
            paste0("\rIteration: ", iteration(state), "; Number of agents: ", length(agents(state)) + 1, "; ", id(agent), " is turning ", turn, "\r") |>
                cat()
        } else if(report) {
            paste("\r", id(agent), "is turning", turn, paste0(rep(" ", 2, collapse = ""))) |>
            cat()
        }

        status(agent) <- "move"

    # If an agent is moving, then get the necessary centers and compute the 
    # utility of moving to a given location
    } else {
        # Define the centers of the options to move to
        centers <- m4ma::c_vd_rcpp(cells = 1:33,
                                   p1 = position(agent),
                                   v1 = speed(agent),
                                   a1 = orientation(agent),
                                   vels = velocities,
                                   angles = orientations,
                                   tStep = time_step)
        agent@cell_centers <- centers

        # Check for occlusions or blocked cells the agent cannot move to
        check <- moving_options(agent, state, background, centers, cpp = cpp)
        
        # If there are no good options available, trigger a reroutening of the 
        # agent: This will create new path points and let the agent reorient. 
        if(!any(check)) {
            # Change the agent's speed to the starting speed after waiting and 
            # indicate that the agent is choosing the stop cell
            speed(agent) <- standing_start * parameters(agent)[["preferred_speed"]]
            cell(agent) <- 0 # Not sure if needed: is more like a soft reorientation

            # Let the agent reorient to find a better way to move out of the 
            # current situation. 
            status(agent) <- "reorient" 

            # Return agent and don't evaluate utilities
            return(agent)
        }

        # Compute the utility variables
        agent@utility_variables <- compute_utility_variables(agent, 
                                                             state,
                                                             background,
                                                             agent_specifications,
                                                             centers,
                                                             check,
                                                             cpp = cpp)

        # Compute the utility of each option and transform the utilities to
        # probabilities
        V <- utility(agent@utility_variables,
                     agent@parameters,
                     cpp = cpp)

        # Check whether you have only infinite moving options. Delete the baseline
        # (cell 0) utility from this list and invoke that they should reorient.
        if(!any(is.finite(V[-1]))) {
            speed(agent) <- standing_start * parameters(agent)[["preferred_speed"]]
            status(agent) <- "reorient"
            cell(agent) <- 0
            return(agent)
        }

        V <- V - max(V)
        exp_V <- exp(V)
        Pr <- exp_V / sum(exp_V)

        # Apply the different options to the probabilities
        names(Pr) <- 0:33

        # Using the probabilities, sample the cell to which the agent will move.
        # Importantly, 1 is subtracted from the integer as 0 is also an option
        # (standing still), but `seq_along` starts at 1
        cell <- sample(seq_along(Pr), 1, prob = Pr) - 1
        cell(agent) <- cell

        # Check what to do: Either the chosen cell is 0 (stop) or something else
        # (moving to another location with a different speed and orientation).
        # If stopped, we need to reset the agent's velocity
        if(cell == 0) {
            speed(agent) <- standing_start * parameters(agent)[["preferred_speed"]]
            # status(agent) <- "reorient" # Was originally handled earlier, but made an infinite loop in current version of the code
            
        } else {
            position(agent) <- centers[cell, ]

            # Update speed to be either higher than or equal to `standing_start`
            acceleration <- velocities[cell]
            speed(agent) <- pmax(speed(agent) * acceleration, 
                                 standing_start * parameters(agent)[["preferred_speed"]])

            # Update orientation to be in degrees and relative to the current 
            # orientation of the agent
            rel_orientation <- ifelse(orientations >= 180, 
                                      orientations - 360, 
                                      orientations)[cell]
            orientation(agent) <- (orientation(agent) + rel_orientation) %% 360
        }
    }

    return(agent)

    # Commented out but left in for now to remind us that the original
    # implementation also gave back information on whether the movement was
    # ok or not

    # Change the agent characteristics based on the results of this function
    # agent@position <-
    # agent@velocity <-
    # out <- list(p = state$p[n, ], v = state$v[n], a = state$a[n], r = state$r[n],
    #             group = state$group[n], pMat = state$pMat[n, ], cell = cell,P=state$P[[n]])
    # attr(out, "ok") <- ok

    # # # Bug catch for outside total area
    # # if (!inObject(state$p[n,,drop=F],xlim=objects[[1]]$x,ylim=objects[[1]]$y,outside=FALSE))
    # #   stop(n)

    # out
}

#' Update the Goal of an Agent
#' 
#' @param object Object of the \code{\link[predped]{agent-class}}.
#' @param state Object of the \code{\link[predped]{state-class}}.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param seen Logical indicating whether the agent can see the path point 
#' to which they are currently moving.
#' @param standing_start Numeric denoting the factor of their preferred speed 
#' that agents move when they just came from standing still. Defaults to 
#' \code{0.1}.
#' @param close_enough Numeric denoting how close (in radii) the agent needs to 
#' be to an object in order to interact with it. Defaults to \code{2}, meaning the 
#' agent can interact with objects at \code{2 * radius(agent)} distance away.
#' @param space_between Numeric denoting the space that should be left between 
#' an object and the created path points for the agents (in radii). Defaults to 
#' \code{2.5}, meaning a space of \code{2.5 * radius(agent)} is left between an 
#' object and the path points agents use in their strategy.
#' @param precomputed_edges Output of \code{\link[predped]{compute_edges}} 
#' containing the nodes and edges the agent can use to plan its path. Defauls 
#' to \code{NULL}, triggering the creation of these edges whenever they are 
#' needed.
#' @param many_nodes Logical denoting whether to use the minimal number of nodes
#' or to use many more (see \code{\link[predped]{create_edges}}). Ignored if 
#' \code{precomputed_edges} is provided. Defaults to \code{FALSE}.
#' @param report Logical denoting whether to report whenever an agent is 
#' reorienting. Defaults to \code{FALSE}, and is usually not needed as feedback.
#' @param print_iteration Logical denoting whether to report each simulated
#' iteration. Defaults to \code{FALSE}, but can be switched off if desired.
#' @param cpp Logical denoting whether to use the Rcpp alternatives for several
#' of the lower-level functions (\code{TRUE}) or whether to use the R alternatives
#' instead (\code{FALSE}). Defaults to \code{TRUE}.
#' 
#' @return Object of the \code{\link[predped]{agent-class}}.
#' 
#' @seealso 
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{simulate,state-method}},
#' \code{\link[predped]{update,agent-method}},
#' \code{\link[predped]{update,state-method}},
#' \code{\link[predped]{update_position}}
#' 
#' @rdname update_goal
#' 
#' @export 
#
# TO DO
#   - At this moment, the same checks happen here and in `best_angle`: Try to
#     find a way to decrease this burden (`move_options`). Happesn twice in this
#     function alone: Once for initial computation, and once when this initial
#     computation does not pan out
#   - Do we want to use the agent@busy for the different options?
#   - Allow plotGrid and printChoice to happen in an object-based way
#   - In the end, again centers computed as many times before (in the utility
#     function): Do the updating more generally and more streamlined so
#     everything becomes a lot clearer
update_goal <- function(agent,
                        state,
                        background,
                        seen,
                        standing_start = 0.1,
                        close_enough = 2,
                        space_between = 1.25,
                        precomputed_edges = NULL,
                        many_nodes = !is.null(precomputed_edges),
                        report = FALSE,
                        print_iteration = FALSE,
                        cpp = TRUE) {  

    # Adjust the relative measures to account for the radius of the agent
    close_enough <- close_enough * radius(agent)
    space_between <- space_between * radius(agent)
    standing_start <- standing_start * parameters(agent)[["preferred_speed"]]

    # Extract objects and shape of the environment
    obj <- objects(background)
    shp <- shape(background)

    # Check whether an agent is close to their goal. If so, they can stop what 
    # they're doing and instead start completing the goal. 
    distance_path_point <- m4ma::dist1(position(agent), 
                                       matrix(current_goal(agent)@position, 
                                              nrow = 1, 
                                              ncol = 2))

    if((distance_path_point <= close_enough) & !(status(agent) %in% c("exit", "completing goal"))) {
        # Differentiate between normal goals and exit goals for the start of the
        # interaction phase.
        if(current_goal(agent)@id == "goal exit") {
            status(agent) <- "exit"
        } else {
            status(agent) <- "completing goal"                    
            co_1 <- position(agent)
            co_2 <- current_goal(agent)@position 
            orientation(agent) <- atan2(co_2[2] - co_1[2], co_2[1] - co_1[1]) * 180 / pi
        }
    }

    # Check what the status of the goal is
    if(status(agent) == "completing goal") {
        # If still completing the goal, interact with it
        current_goal(agent) <- interact(agent@current_goal)
        status(agent) <- "completing goal"
        
        # Replace goal if necessary
        if(current_goal(agent)@done) {
            # Check if there are goals left to give. If not, then give the agent
            # the task of going to the exit
            if(length(goals(agent)) > 0) {
                current_goal(agent) <- goals(agent)[[1]]
                goals(agent) <- goals(agent)[-1]
            } else {
                # When there are multiple exits, use an algorithm to make the 
                # agent decide which one to go to 
                exits <- exit(background)
                if(nrow(exits) > 1) {
                    # Check which exit is closest to the current position
                    distances <- (position(agent)[1] - exits[,1])^2 + (position(agent)[2] - exits[,2])^2
                    exits <- exits[which.min(distances),]
                } 
                
                current_goal(agent) <- goal(id = "goal exit",
                                            position = as.numeric(exits))
            }

            # Replan if the goal paths were not precomputed yet
            if(nrow(current_goal(agent)@path) == 0) {
                status(agent) <- "plan"
            } else {
                status(agent) <- "reorient"
            }

            # Left in but commented out: Changed orientation when the agent 
            # would later reorient. Not sure why
            # state$a[j] <- angle2(state$p[j, , drop = F], 
            #                 state$P[[j]][attr(state$P[[j]], "i"), 1:2, 
            #                                 drop = FALSE])
        }
    } 

    # If the agent has to plan their route to the goal, then we have to find the 
    # quickest path to the goals. Either the agent sees their goal, and can walk 
    # directly towards it, or they cannot see their goal and they have to 
    # actually plan their route.
    if(status(agent) == "plan") {
        # If the agent sees their goal, the path is just going to go directly 
        # towards the goal
        if(seen) {
            current_goal(agent)@path <- matrix(current_goal(agent)@position, ncol = 2)

        # If they cannot see their current goal, they will have to plan their 
        # route according to the path points in the environment. Given that we 
        # don't add anything new in the environment, we can safely put 
        # `reevaluate` to FALSE in this chunk of code
        } else {
            current_goal(agent)@path <- find_path(current_goal(agent), 
                                                  agent, 
                                                  background,
                                                  precomputed_edges = precomputed_edges,
                                                  many_nodes = many_nodes,
                                                  reevaluate = FALSE)
        }

        # Quick check whether the path is clearly defined. If not, then the agent 
        # will have to reroute at a later time and wait for now. 
        if(nrow(current_goal(agent)@path) == 0 | is.null(current_goal(agent)@path)) {
            status(agent) <- "plan"
            return(agent)
        }

        # After planning their next move, they need to reorient in order to move 
        # to the first path point
        status(agent) <- "reorient"
    }
    
    # If the agent has to reroute, then we have to redefine path points. 
    # Either the agent sees their goal, and can walk directly towards it, or 
    # they cannot see their goal and they have to plan their route. Importantly,
    # agents can still plan their path if they can see their goal, but other 
    # agents are in the way. This is determined by the reroute parameter
    if(status(agent) == "reroute") {
        # If the agent doesn't see their current goal, they have to reroute with
        # a probability of 100%
        if(!seen) {
            prob_rerouting <- 1

        # If they do see their goal, they only reroute with a given probability
        # defined by their rerouting parameter and the number of people that 
        # are in the way
        } else {
            reroute_param <- parameters(agent)$reroute

            if(is.finite(reroute_param)) {
                # Compute the probability of rerouting based on the number of
                # agents that are standing inbetween the `agent` and their goal
                blocking_agents <- agents_between_goal(agent, state)
                prob_rerouting <- pnorm(length(blocking_agents) - reroute_param)
            } else {
                prob_rerouting <- 0
            }
        }

        # Check whether you will reroute on this move
        old_path <- current_goal(agent)@path
        if(runif(1) < prob_rerouting) {
            # Given that you have to reroute, reroute how you will get to your 
            # goal. Add the other agents in objects to account for so you don't 
            # take the same route.
            #
            # Given that new things were added to the environment (i.e., the 
            # other agents that this agent should account for when planning),
            # we need to put `reevaluate` to TRUE so that old edges can be 
            # deleted (if necessary)
            blocking_agents <- agents_between_goal(agent, state)
            current_goal(agent)@path <- find_path(current_goal(agent), 
                                                  agent, 
                                                  background,
                                                  space_between = space_between,
                                                  new_objects = blocking_agents,
                                                  precomputed_edges = precomputed_edges,
                                                  many_nodes = many_nodes,
                                                  reevaluate = TRUE)

            # Perform a first check. If no path remains open, try to reroute 
            # without accounting for the other agents that are standing in the 
            # way. Only if this doesn't work will we move on to something else.
            if(nrow(current_goal(agent)@path) == 0 | is.null(current_goal(agent)@path)) {
                current_goal(agent)@path <- old_path
            }

            # Quick check whether the path is clearly defined. If not, 
            # then the agent will have to reroute at a later time and 
            # wait for now. 
            if(nrow(current_goal(agent)@path) == 0 | is.null(current_goal(agent)@path)) {
                status(agent) <- "plan"
                return(agent)
            }

            # Turn to the new path point and slow down
            speed(agent) <- standing_start * parameters(agent)[["preferred_speed"]]

            goal_position <- current_goal(agent)@path
            agent_position <- position(agent)
            orientation(agent) <- atan2(goal_position[1, 2] - agent_position[2],
                                        goal_position[1, 1] - agent_position[1]) * 180 / pi
    
            # If the old path was retained, then that means that there is not 
            # better way of going to the goal. This means that the agent is not 
            # able to walk through and should wait for a little bit. 
            #
            # Note that this wait state was not originally meant for this purpose
            # and might therefore be a bit "hacky".
            #
            # First checked for an equal number of rows to avoid problems in 
            # comparing both matrices because of non-comformability of the rows.
            new_path <- current_goal(agent)@path
            if(nrow(new_path) == nrow(old_path)) {
                if(all(new_path == old_path)) {
                    status(agent) <- "wait"
                    waiting_counter(agent) <- 5 # Keep at 5 
                    return(agent)
                }
            }
        } 

        # At the end of rerouting (whether it occurred or not), let them reorient
        status(agent) <- "reorient"
    }

    # If the agent is currently waiting, check the following:
    #   - Check whether the counter is lower than 0. If so, then the agent will
    #     not wait any longer, but rather reroute.
    #   - Check whether the agent blocking the way has left. If so, the agent 
    #     no longer has to wait around and can start moving again.
    if(status(agent) == "wait") {
        # Check the counter
        waiting_counter(agent) <- waiting_counter(agent) - 1

        # Draw a circle around the current goal of the agent and find out whether
        # any of the other agents intersects with this. If so, then we can assume
        # one of the agents is blocking the access to the goal, and the agent
        # cannot start the interaction phase.
        goal_circle <- circle(center = current_goal(agent)@position,
                              radius = radius(agent))
        blocking_agents <- sapply(agents(state), 
                                  \(x) intersects(goal_circle, x))

        # If no agents are blocking access to the goal, allow the agent to move
        # again
        if(!any(blocking_agents)) {
            status(agent) <- "reorient"
        }

        # If counter is low enough, the agent will have to reroute his approach
        # to the goal. To make sure agents don't get stuck easily, let them 
        # pursue another goal and come back later. Only applicable if the agent
        # still has other goals to pursue
        if(waiting_counter(agent) < 0 & status(agent) != "move") {
            if(length(goals(agent)) != 0) {
                goals(agent) <- append(current_goal(agent), 
                                       goals(agent))
                current_goal(agent) <- goals(agent)[[2]]
                goals(agent) <- goals(agent)[-2]
                status(agent) <- "plan"
            } else {
                status(agent) <- "reorient"
            }
        }
    }
        
    # Finally, it might also be that the agent is close to the goal and can 
    # start interacting with it. This is what's handled in this code block.
    if(status(agent) == "move") {  
        # Check whether agents can still see their goal. If not, then let them
        # reroute.
        if(!seen) {
            status(agent) <- "reroute"
            return(agent)
        }

        # Check whether agents can see the next path point. If so, then they 
        # can move to that one instead of the first one
        if(nrow(current_goal(agent)@path) > 1) {
            # If there are still path points left, check whether the agent can see 
            # the next path point.
            if(length(obj) == 0) {
                seen_next <- TRUE
            } else {
                seen_next <- prune_edges(obj,
                                         matrix(c(position(agent), current_goal(agent)@path[2,]), 
                                                nrow = 1))
            }

            # If a next path point is visible, the agent will switch to that 
            # path point instead
            if(seen_next) {
                current_goal(agent)@path <- current_goal(agent)@path[-1, , drop = FALSE] 
                status(agent) <- "reorient" # Keep this in! Otherwise agents get stuck

                return(agent)
            }
        }

        # We need to allow for another option in goal handling: namely the case 
        # where another agent is blocking the access of the agent to their 
        # current goal. To avoid the agent from constantly reroutening, we will 
        # use the status "wait" to indicate that they should be patient instead.
        #
        # To initiate "wait", we check whether any of the agents is currently 
        # blocking access to the current goal of the agent. We do so by drawing 
        # a circle around the current goal and checking whether another agent 
        # intersects this circle. If so, then we will let the agent wait.
        #
        # Only relevant if there is more than 1 agent and if the agent is 
        # relatively close to the goal
        goal_position <- current_goal(agent)@position
        goal_distance <- sqrt((center(agent)[1] - goal_position[1])^2 + 
            (center(agent)[2] - goal_position[2])^2)
        if((length(state@agents) > 0) & (goal_distance <= 2 * close_enough)) {
            # Find whether an agent is blocking the way
            goal_circle <- circle(center = current_goal(agent)@position,
                                  radius = radius(agent))
            blocking_agents <- sapply(state@agents, 
                                      \(x) intersects(goal_circle, x))

            # If only one agent is blocking the goal, let the agent wait. Only invoke
            # this the moment that the agent is actually in its last movement towards
            # the goal (i.e., when the position of the current goal is also the 
            # last path point) and if the agent is almost within reach of the goal
            if(any(blocking_agents) & (nrow(current_goal(agent)@path) == 1)) {
                # Find out whether that agent is actually completing a goal or not.
                # If not, then the agent will just continue business as usual.
                idx <- Position(\(x) x == TRUE, blocking_agents)

                # Check the status of all agents that are blocking the goal. If
                # they are completing the goal, then the agent will have to wait
                # its turn. If the blocking agent is not completing a goal, then 
                # the agent will just have to wait until they move on.
                stati <- lapply(state@agents[idx], \(x) c(status(x), current_goal(x)@counter))
                stati <- do.call("rbind", stati)
                stati[stati[,1] != "completing goal", 2] <- 1

                status(agent) <- "wait"
                waiting_counter(agent) <- max(as.numeric(stati[,2]) + 2)
            }
        }
    }

    return(agent)
}





#' Predict agents' movement
#' 
#' Uses the agents' current speed and orientation to determine where the agent 
#' might end up in the next step, assuming that they do not change direction or 
#' speed. This information is used by other agents to determine where (not) to 
#' go to avoid collisions.
#' 
#' @param agent Object of the \code{\link[predped]{agent-class}}.
#' @param stay_stopped Logical denoting whether agents will predict others that 
#' are currently not moving to remain immobile in the next iteration. Defaults 
#' to \code{TRUE}.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to \code{0.5}, or half a second.
#' @param cpp Logical denoting whether to use the Rcpp alternative of this 
#' function (\code{TRUE}) or the R alternative (\code{FALSE}). Defaults to 
#' \code{TRUE}.
#' 
#' @return Numeric matrix containing the predicted positions all agents if 
#' they all maintain their speed and direction.
#' 
#' @seealso 
#' \code{\link[predped]{create_agent_specifications}},
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{simulate,state-method}},
#' \code{\link[predped]{update,agent-method}},
#' \code{\link[predped]{update,state-method}}
#' 
#' @rdname predict_movement
#' 
#' @export
predict_movement <- function(agent, 
                             stay_stopped = TRUE,
                             time_step = 0.5,
                             cpp = TRUE) {

    if(cpp) {
        return(predict_movement_rcpp(agent, 
                                     stay_stopped,
                                     time_step))
    }
    
    # Compute the coordinate where the agents will end up when moving at the 
    # same speed in the same direction. Different when an agent is currently 
    # stopped vs when they are actively moving. Also different depending on 
    # whether this is considered in the first place.
    if(stay_stopped & agent@status == "stop") {
        co <- position(agent)
    } else {
        crossed_distance <- m4ma::scaleVel(agent@speed, tStep = time_step) * m4ma::aTOd(agent@orientation)
        co <- coordinate(position(agent) + crossed_distance)
    }    

    return(co)
}

#' Create agent specifications
#' 
#' This list translates the information available in the \code{agents} slot of
#' the current status of the \code{\link[predped]{state-class}} to a list 
#' with all this information in numeric vectors or matrices instead of inside 
#' objects. Allows for a translation from the object-oriented way of doing things
#' in \code{predped} to the vectorized way of doing things in \code{m4ma}.
#'
#' @param agent Object of the \code{\link[predped]{agent-class}}.
#' @param stay_stopped Logical denoting whether agents will predict others that 
#' are currently not moving to remain immobile in the next iteration. Defaults 
#' to \code{TRUE}.
#' @param time_step Numeric denoting the number of seconds each discrete step in
#' time should mimic. Defaults to \code{0.5}, or half a second.
#' 
#' @return List containing all information of all agents within the current 
#' state.
#' 
#' @seealso 
#' \code{\link[predped]{create_agent_specifications}},
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{simulate,state-method}},
#' \code{\link[predped]{update,agent-method}},
#' \code{\link[predped]{update,state-method}}
#' 
#' @rdname create_agent_specifications
#' 
#' @export
create_agent_specifications <- function(agent_list,
                                        stay_stopped = TRUE, 
                                        time_step = 0.5,
                                        cpp = TRUE) {

    if(cpp) {
        return(create_agent_specifications_rcpp(agent_list, stay_stopped, time_step))
    }
    
    # Predict where the agents will be at their current velocity and angle. Is 
    # used by other agents to change their own directions in order to avoid 
    # collisions.
    #
    # In order for this to work with m4ma, we need to transform it to a matrix 
    # and provide it rownames that are equal to the id's of the agents
    agent_predictions <- sapply(agent_list, 
                                \(x) predict_movement(x, 
                                                      stay_stopped = stay_stopped,
                                                      time_step = time_step,
                                                      cpp = cpp)) |>
        t()

    # Make the object-based arguments of predped compatible with the information
    # needed by m4ma. 
    agent_specs <- list(id = as.character(sapply(agent_list, id)),
                        size = as.numeric(sapply(agent_list, size)),
                        position = t(sapply(agent_list, position)),
                        orientation = as.numeric(sapply(agent_list, orientation)), 
                        speed = as.numeric(sapply(agent_list, speed)), 
                        group = as.numeric(sapply(agent_list, group)),
                        predictions = agent_predictions)

    # Required for utility helper functions: Add names of the agents to their
    # characteristics
    rownames(agent_specs$position) <- agent_specs$id
    names(agent_specs$size) <- agent_specs$id
    names(agent_specs$orientation) <- agent_specs$id
    names(agent_specs$speed) <- agent_specs$id
    names(agent_specs$group) <- agent_specs$id
    rownames(agent_specs$predictions) <- agent_specs$id

    return(agent_specs)
}