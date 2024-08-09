# ' Move an object
# '
# ' Move an object around the space. This function changes the positions and/or
# ' orientation of the provided object. For the class `agent`, this involves a
# ' computation of the utility of a given movement. For other classes, this
# ' involves checking whether an agent is interacting with the object before it
# ' can move.
# '
# ' @param object The object of which the position should be moved.
# ' @param ... Additional arguments that can differ for agents or environmental
# ' objects.
# '
# ' @return The provided object with a changed position and/or orientation
# '
# ' @export
# ' @docType methods
# ' @rdname move-methods
# setGeneric("move", function(object, ...){})


#' Update the current State
#' 
#' Function that is used in the simulation to update the current state. Loops
#' over all the agents and updates both agent characteristics and the setting 
#' in light of the agents actions to achieve their goals. 
#' 
#' @param state The current state of the simulation
#' @param background The setting in which agents are walking around
#' @param stay_stopped Logical denoting whether agents will predict other agents 
#' who have stopped to remain immobile. Is passed on to `predict_movement`. 
#' Defaults to `TRUE`
#' @param time_step Numeric denoting the time step taken between iterations in 
#' seconds. Defaults to `0.5` or half a second.
#' @param ... Arguments to be passed on to `update_agent`
#' 
#' @family updating-functions
#' 
#' @export 
# 
# TO DO
#   - Also update objects when they are moveable and used. To see how we do it
update_state <- function(state, 
                         background,
                         stay_stopped = TRUE, 
                         time_step = 0.5,
                         close_enough = 2,
                         space_between = 2.5,
                         standing_start = 0.1,
                         precomputed_edges = NULL,
                         precompute_goal_paths = FALSE,
                         report = FALSE,
                         interactive_report = FALSE,
                         ...) {

    # Predict where the agents will be at their current velocity and angle. Is 
    # used by other agents to change their own directions in order to avoid 
    # collisions.
    #
    # In order for this to work with m4ma, we need to transform it to a matrix 
    # and provide it rownames that are equal to the id's of the agents
    agent_predictions <- lapply(state$agents, 
                                \(x) predict_movement(x, 
                                                      stay_stopped = stay_stopped,
                                                      time_step = time_step))
    agent_predictions <- sapply(agent_predictions, \(x) x) |>
        t()
    rownames(agent_predictions) <- sapply(state$agents, id)

    # Create agent-specifications. Are used in the utility-function and used to
    # be created there. Moved it here to reduce computational cost (which increases
    # exponentially with more agents)
    agent_specs <- create_agent_specifications(state$agents, 
                                               agent_predictions)

    # Loop over each agent in the simulation and update their position with the 
    # `update_agent` function
    for(i in seq_along(state$agents)) {
        # Extract the agent to-be-updated from the state list. Importantly, also
        # remove the agent from this state list, as it should not contain this 
        # one agent: Simulation is done relative to the agent to-be-updated
        agent <- state$agents[[i]]

        tmp_state <- state
        tmp_state$agents <- tmp_state$agents[-i]

        # Update the goals of the agent
        agent <- update_goal(agent, 
                             tmp_state, 
                             background,
                             close_enough = close_enough,
                             space_between = space_between,
                             standing_start = standing_start,
                             precomputed_edges = precomputed_edges,
                             precompute_goal_paths = precompute_goal_paths,
                             report = report, 
                             interactive_report = interactive_report) 

        # Update the position of the agent
        # start_time <- Sys.time()
        agent <- update_position(agent, 
                                 tmp_state,
                                 agent_specs, # Keep all agents in here: predClose makes use of own prediction as well
                                 background,
                                 standing_start = standing_start,
                                 report = report,
                                 time_step = time_step,
                                 ...) 

        # Update the agent himself
        state$agents[[i]] <- agent
    }

    return(state)
}

#' Predict agent's movement
#' 
#' Use an agents' current speed and orientation to determine where the agent might 
#' end up in the next step. This information is used by other agents' in the 
#' simulation to determine where (not) to go to if they want to avoid collisions.
#' 
#' @param agent The agent in consideration
#' @param stay_stopped Logical denoting whether agents will predict other agents 
#' who have stopped to remain immobile. Defaults to `TRUE`.
#' @param time_step Numeric denoting the time step taken between iterations in 
#' seconds. Defaults to `0.5` or half a second.
#' 
#' @export
predict_movement <- function(agent, 
                             stay_stopped = TRUE,
                             time_step = 0.5) {
    
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

create_agent_specifications <- function(agent_list,
                                        agent_predictions) {
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



#' Move an Agent
#' 
#' @param agent The agent to move
#' @param state The current state of affairs. Importantly, the agent in `agent`
#' should not be present in that state
#' @param agent_predictions A list containing the predictions of where each agent
#' might move to
#' @param background The setting in which agents are walking around
#' @param velocities Matrix that contains the change in velocity per cell that 
#' the agent might move to. Defaults to an 11 by 3 matrix where each row contains
#' 0.5 (deceleration), 1 (same speed), and 1.5 (acceleration).
#' @param orientations Matrix that contains the change in orientation per cell 
#' that the agent might move to. Defaults to an 11 by 3 matrix where each column
#' contains 72.5, 50, 32.5, 20, 10, 0, 350, 340, 327.5, 310, and 287.5 degrees.
#' @param standing_start Numeric denoting the speed of the agent when they 
#' resume walking after stopping. Defaults to `0.1`
#' @param time_step Numeric denoting the time step taken between iterations in 
#' seconds. Defaults to `0.5` or half a second.
#' 
#' @return Updated agent
#' 
#' @family updating-functions
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
                            agent_specifications,
                            background,
                            nests = list(
                                Central = c(0, 6, 17, 28),
                                NonCentral = c(0:33)[-c(6, 17, 28)],
                                acc = c(1:11),
                                const = c(12:22),
                                dec = c(0, 23:33)
                            ),
                            alpha = list(
                                Central = rep(1/3, 4),
                                NonCentral = c(1/3, rep(0.5, 4), 1/3, rep(0.5, 9), 1/3,
                                               rep(0.5, 9), 1/3, rep(0.5, 5)),
                                acc = c(rep(0.5, 4), 1, 1/3, rep(0.5, 5)),
                                const = c(rep(0.5, 4), 1, 1/3, rep(0.5, 5)),
                                dec = c(1/3, rep(0.5, 4), 1, 1/3, rep(0.5, 5))
                            ),
                            velocities = c(1.5, 1, 0.5) |>
                               rep(each = 11) |>
                               matrix(ncol = 3),
                            orientations = c(72.5, 50, 32.5, 20, 10, 0, 
                                             350, 340, 327.5, 310, 287.5) |>
                                rep(times = 3) |>
                                matrix(ncol = 3),
                            standing_start = 0.1,
                            time_step = 0.5,
                            report = TRUE
                        #     plotGrid = FALSE,        # deprecated?
                        #     printChoice = FALSE,     # deprecated?                     
                        #     usebestAngle = FALSE     # deprecated?
                            ) {

    standing_start <- standing_start * parameters(agent)[["preferred_speed"]]

    # Let the agent wait (cell = 0 and speed is slowest possible one) when the 
    # agent is currently interacting with another object, when they are currently 
    # rerouting, when they are currently planning a route to their goal, or when 
    # they are waiting for another agent.
    if(status(agent) %in% c("completing goal", "reroute", "plan", "wait")) { 
        cell(agent) <- 0
        speed(agent) <- standing_start
                        
    # If the agent has stopped their interaction, check whether they already know
    # where to go to (i.e., whether they are oriented towards their new path
    # point). If not, let them reorient themselves towards their next goal.
    } else if(status(agent) == "reorient") {        
        orientation(agent) <- best_angle(agent, 
                                         state, 
                                         agent_specifications, 
                                         background, 
                                         velocities, 
                                         orientations)

        # Report the degress that the agent is reorienting to
        turn <- paste("to", orientation(agent), "degrees")
        if(report) {
            paste(id(agent), "turning", turn, "\n") |>
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

        # Check for occlusions or blocked cells the agent cannot move to
        check <- moving_options(agent, state, background, centers)
        
        # If there are no good options available, trigger a reroutening of the 
        # agent: This will create new path points and let the agent reorient. 
        if(!any(check)) {
            # Change the agent's speed to the starting speed after waiting
            speed(agent) <- standing_start
            status(agent) <- "reroute" # Get errors when not leaving this in
            cell(agent) <- 0 # Not sure if needed: is more like a soft reorientation
            return(agent)
        }

        # Compute the utility of of each option and transform the utilities to
        # probabilities
        V <- utility(agent, 
                     state, 
                     agent_specifications, 
                     centers, 
                     background, 
                     check)

        if(!any(is.finite(V))) {
            speed(agent) <- standing_start
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
        cell <- sample.int(length(Pr),
                           1,
                           TRUE,
                           prob = Pr) - 1

        cell(agent) <- cell

        # Check what to do: Either the chosen cell is 0 (stop) or something else
        # (moving to another location with a different speed and orientation).
        # If stopped, we need to reset the agent's velocity
        if(cell == 0) {
            speed(agent) <- standing_start
            status(agent) <- "reorient" # Was originally handled earlier, but made an infinite loop in current version of the code
            
        } else {
            position(agent) <- centers[cell,]

            # Update speed to be either higher than or equal to `standing_start`
            acceleration <- velocities[cell]
            speed(agent) <- pmax(speed(agent) * acceleration, 
                                 standing_start)

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
#' @param agent The agent to move
#' @param state The current state of affairs. Importantly, the agent in `agent`
#' should not be present in that state
#' @param background The setting in which agents are walking around
#' @param standing_start Numeric denoting the speed of the agent when they 
#' resume walking after stopping. Defaults to `0.1`
#' @param close_enough Numeric denoting the distance an agent needs to a path 
#' point or goal in order to interact with it. Defaults to `radius(agent) / 2`
#' @param report Logical denoting whether we should report the actions of the 
#' agent with regard to the goal. Defaults to `FALSE`
#' @param interactive_report Logical denoting whether these reports of `report`
#' should require user input. Defaults to `FALSE`
#' 
#' @return Updated agent
#' 
#' @family updating-functions
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
                        standing_start = 0.1,
                        close_enough = 2,
                        space_between = 2.5,
                        report = FALSE,
                        interactive_report = FALSE,
                        precomputed_edges = NULL,
                        precompute_goal_paths = FALSE) {  

    close_enough <- close_enough * radius(agent)
    space_between <- space_between * radius(agent)
    standing_start <- standing_start * parameters(agent)[["preferred_speed"]]

    # Make some placeholders for reroutening and rerouting
    reroute <- FALSE

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
            if(!precompute_goal_paths) {
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
        # Check whether the agent can see the current goal.
        seen <- all(prune_edges(objects(background), 
                                matrix(c(position(agent), current_goal(agent)@position),
                                       nrow = 1)))

        # If the agent sees their goal, the path is just going to go directly 
        # towards the goal
        if(seen) {
            current_goal(agent)@path <- matrix(current_goal(agent)@position, ncol = 2)

        # If they cannot see their current goal, they will have to plan their 
        # route according to the path points in the environment
        } else {
            current_goal(agent)@path <- find_path(current_goal(agent), 
                                                  agent, 
                                                  background,
                                                  space_between = space_between,
                                                  precomputed_edges = precomputed_edges)
        }

        # Quick check whether the path is clearly defined. If not, then the agent 
        # will have to reroute at a later time and wait for now. 
        if(nrow(current_goal(agent)@path) == 0 | is.null(current_goal(agent)@path)) {
            status(agent) <- "reroute"
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
        # Check whether the agent can see the current goal.
        # seen <- sees_location(agent, 
        #                       current_goal(agent)@position, 
        #                       objects(background))
        seen <- all(prune_edges(objects(background), 
                                matrix(c(position(agent), current_goal(agent)@position),
                                       nrow = 1)))

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
                prob_rerouting <- pnorm(blocking_agents - reroute_param)
            } else {
                prob_rerouting <- 0
            }
        }

        # Check whether you will reroute on this move
        if(runif(1) < prob_rerouting) {
            # Given that you have to reroute, reroute how you will get to your 
            # goal. Add the other agents in objects to account for so you don't 
            # take the same route.
            updated_background <- background
            objects(updated_background) <- append(objects(updated_background), 
                                                  state$agents)

            # current_goal(agent)@path <- find_path(current_goal(agent), 
            #                                       agent, 
            #                                       updated_background,
            #                                       space_between = space_between,
            #                                       precomputed_edges = NULL,
            #                                       many_options = TRUE)
            current_goal(agent)@path <- find_path(current_goal(agent), 
                                                  agent, 
                                                  updated_background,
                                                  space_between = space_between,
                                                  precomputed_edges = precomputed_edges)

            # Quick check whether the path is clearly defined. If not, 
            # then the agent will have to reroute at a later time and 
            # wait for now. 
            if(nrow(current_goal(agent)@path) == 0 | is.null(current_goal(agent)@path)) {
                status(agent) <- "reroute"
                return(agent)
            }

        } else {
            # If you don't need to reroute, but can go to the goal directly,
            # then the `path` attribute just takes in the goal's location
            current_goal(agent)@path <- matrix(current_goal(agent)@position,
                                               ncol = 2)
        }

        # Turn to the new path point and slow down
        speed(agent) <- standing_start * parameters(agent)[["preferred_speed"]]

        # After reroutening, put the status to "reorient" so that they will be
        # able to reorient in the next move
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

        # If counter is low enough, the agent will have to reroute his approach
        # to the goal. To make sure agents don't get stuck easily, let them 
        # pursue another goal and come back later. Only applicable if the agent
        # still has other goals to pursue
        if(waiting_counter(agent) < 0) {
            if(length(goals(agent)) != 0) {
                goals(agent) <- append(current_goal(agent), 
                                       goals(agent))
                current_goal(agent) <- goals(agent)[[2]]
                goals(agent) <- goals(agent)[-2]
                status(agent) <- "plan"
            } else {
                status(agent) <- "move"
            }

        # If the counter is not low enough yet, but the agent is still waiting, 
        # we can check whether other agents are still blocking his way
        } else {
            # Draw a circle around the current goal of the agent and find out whether
            # any of the other agents intersects with this. If so, then we can assume
            # one of the agents is blocking the access to the goal, and the agent
            # cannot start the interaction phase.
            goal_circle <- circle(center = current_goal(agent)@position,
                                  radius = radius(agent))
            blocking_agents <- sapply(state$agents, 
                                      \(x) intersects(goal_circle, x))

            # If no agents are blocking access to the goal, allow the agent to move
            # again
            if(!any(blocking_agents)) {
                status(agent) <- "move"
            }
        }        
    }
        
    # Finally, it might also be that the agent is close to the goal and can 
    # start interacting with it. This is what's handled in this code block.
    if(status(agent) == "move") {
        # Keep this in for debugging purposes
        if(nrow(current_goal(agent)@path) == 0 | is.null(current_goal(agent)@path)) {
            View(current_goal(agent))
            View(agent)
            print(plot(background) + plot(state$agents))
        }

        # Determine how far along the `path` they are
        distance_path_point <- m4ma::dist1(position(agent), 
                                           matrix(current_goal(agent)@path[1,], 
                                                  nrow = 1, 
                                                  ncol = 2))

        # Check whether they are "close enough" to the goal
        if((distance_path_point <= close_enough)) {
            # If the next path point is actually their goal, they can enter in an 
            # interaction state.
            #
            # Check if the goal is the exit goal. If so, the interaction state
            # is not "completing goal" but rather "exit", allowing us to delete
            # the agent
            if(nrow(current_goal(agent)@path) == 1) {
                if(current_goal(agent)@id == "goal exit") {
                    status(agent) <- "exit"
                } else {
                    status(agent) <- "completing goal"                    

                    co_1 <- position(agent)
                    co_2 <- current_goal(agent)@position 

                    orientation(agent) <- atan2(co_2[2] - co_1[2], co_2[1] - co_1[1]) * 180 / pi
                }

                return(agent)

            # If the next path point is not their goal, we have to do an 
            # additional check of whether the agent can see the next path point.
            # If so, the agent will move to that path point instead
            } else {
                seen <- all(prune_edges(objects(background), 
                                        matrix(c(position(agent), current_goal(agent)@path[2,]), 
                                               nrow = 1)))

                if(seen) {
                    current_goal(agent)@path <- current_goal(agent)@path[-1,] |>
                        matrix(ncol = 2)
                    status(agent) <- "reorient"

                    return(agent)
                }
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
        if((length(state$agents) > 0) & (goal_distance <= 2 * close_enough)) {
            # Find whether an agent is blocking the way
            goal_circle <- circle(center = current_goal(agent)@position,
                                  radius = radius(agent))
            blocking_agents <- sapply(state$agents, 
                                      \(x) intersects(goal_circle, x))

            # If only one agent is blocking the goal, let the agent wait. Only invoke
            # this the moment that the agent is actually in its last movement towards
            # the goal (i.e., when the position of the current goal is also the 
            # last path point)
            if(any(blocking_agents) & (nrow(current_goal(agent)@path) == 1)) {
                # Find out whether that agent is actually completing a goal or not.
                # If not, then the agent will just continue business as usual.
                idx <- Position(\(x) x == TRUE, blocking_agents)

                # Check the status of all agents that are blocking the goal. If
                # they are completing the goal, then the agent will have to wait
                # its turn. If the blocking agent is not completing a goal, then 
                # the agent will just have to wait until they move on.
                stati <- lapply(state$agents[idx], \(x) c(status(x), current_goal(x)@counter))
                stati <- do.call("rbind", stati)
                stati[stati[,1] != "completing goal", 2] <- 0

                status(agent) <- "wait"
                waiting_counter(agent) <- max(as.numeric(stati[,2]) + 2)
            }
        }
    }

    return(agent)
}