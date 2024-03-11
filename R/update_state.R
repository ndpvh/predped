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
#' @export 
# 
# TO DO
#   - Also update objects when they are moveable and used. To see how we do it
update_state <- function(state, 
                         background,
                         stay_stopped = TRUE, 
                         time_step = 0.5,
                         ...) {
    # Predict where the agents will be at their current velocity and angle. Is 
    # used by other agents to change their own directions in order to avoid 
    # collisions
    agent_predictions <- lapply(state$agents, 
                                \(x) predict_movement(x, 
                                                      stay_stopped = stay_stopped,
                                                      time_step = time_step))

    # Loop over each agent in the simulation and update their position with the 
    # `update_agent` function
    for(i in seq_along(state$agents)) {
        # Extract the agent to-be-updated from the state list. Importantly, also
        # removed from this state list, as it should not contain this one agent:
        # Simulation is done relative to the agent to-be-updated
        agent <- state$agents[[i]]

        # Update the position of the agent in the state list and move on to the 
        # next agent.
        state$agents[[i]] <- update_agent(agent, 
                                          state,
                                          agent_predictions,
                                          background,
                                          time_step = time_step,
                                          ...)
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
#' resume walking after stopping. Defaults to `0.2`
#' @param time_step Numeric denoting the time step taken between iterations in 
#' seconds. Defaults to `0.5` or half a second.
#' 
#' @return Updated agent
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
update_agent <- function(agent,
                         state,
                         agent_predictions,
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
                         orientations = matrix(rep(c(
                             72.5, 50, 32.5, 20, 10,
                             0, 350, 340, 327.5, 310, 287.5),
                             times = 3),
                             ncol = 3),
                         standing_start = 0.2,
                         time_step = 0.5
                        #  plotGrid = FALSE,        # deprecated?
                        #  printChoice = FALSE,     # deprecated?                     
                        #  usebestAngle = FALSE     # deprecated?
                         ) {
    # If the agent is currently interacting with another object, just continue
    if(status(agent) == "stop") { # Used to be a check of the goal state: attr(state$P[[n]], "stop") > 0, == -1, or else (interacting, reorient, going)
        cell(agent) <- 0
        check <- matrix(TRUE, 11, 3)

        # Make sure the agent interacts with their goal and are assigned a new 
        # one in case their current goal is ended.
        current_goal(agent) <- interact(agent@current_goal)
        
        if(is.null(current_goal(agent))) {
            current_goal(agent) <- goals(agent)[[1]]
            goals(agent) <- goals(agent)[-1]
        }
    } else {
        # If the agent stopped their interaction, check whether they already know
        # where the next goal is. If they don't. let them reorient themselves to
        # the next goal
        if(status(agent) == "reorient") {
            orientation(agent) <- best_angle(agent, state, agent_predictions, background, velocities, orientations)
            status(agent) <- "move"
        }

        # Define the centers of the options to move to
        centers <- m4ma::c_vd_rcpp(
            cells = 1:33,
            p1 = position(agent),
            v1 = speed(agent),
            a1 = orientation(agent),
            vels = velocities,
            angles = orientations,
            tStep = time_step
        )

        # Check for occlusions or blocked cells the agent cannot move to
        check <- moving_options(agent, state, background, centers)
        
        # If there are no good options available, or the agent wants to stop,
        # then allow him to
        if(!any(check) | cell(agent) == 0) {  # stop or will have to
            # Change the agent's speed to the starting speed after waiting
            speed(agent) <- standing_start

            # Let the agent reorient to find a better way
            orientation(agent) <- best_angle(agent, state, agent_predictions, background, velocities, orientations)

            # Report the degress that the agent is reorienting to
            turn <- paste("to", orientation(agent), "degrees")
            if(status(agent) == "move") {
                paste(id(agent), "turning", turn, "\n") |>
                    cat()
            }

            #cell <- 0

            # Define the centers of the options to move to
            centers <- m4ma::c_vd_rcpp(
                cells = 1:33,
                p1 = position(agent),
                v1 = speed(agent),
                a1 = orientation(agent),
                vels = velocities,
                angles = orientations,
                tStep = time_step
            )

            # Check for occlusions or blocked cells the agent cannot move to
            check <- moving_options(agent, state, background, centers)
        }

        # Compute the utility of of each option and transform the utilities to
        # probabilities
        V <- utility(agent, state, agent_predictions, centers, background, check)

        Pr <- sapply(1:34, function(i) m4ma::pcnl_rcpp(m4ma::get_cell_nest()[i, ], V, rep(1, length(nests)), nests, alpha, mu = 1))

        # Apply the different options to the probabilities
        names(Pr) <- 0:33

        # Check whether the simulation should be drawn. Given that this depends
        # on the information `n` (index of the agent) left out for now
        # if(!is.null(plotGrid) && plotGrid[n]) {
        # draw_grid(state$p[n, ], state$v[n], state$a[n], plotPoints = TRUE,
        #             Pr = Pr[-1])
        # }

        # Using the probabilities, sample the cell to which the agent will move.
        # Importantly, 1 is subtracted from the integer as 0 is also an option
        # (standing still), but `seq_along` starts at 1
        cell <- sample.int(length(Pr),
                           1,
                           TRUE,
                           prob = Pr) - 1

        cell(agent) <- cell

        # Check whether the choice should be printed. With the same reasoning
        # as plotGrid also ignored for now, but left in for later
        # if (!is.null(printChoice) && printChoice[n]) {
        #     if (cell == 0) {
        #         cat("Standing still\n\n")
        #     } else {
        #         cat(paste("\nPedestrian =", row.names(state$p)[n],
        #                 " Choice =", cell,
        #                 " Ring =", c("accelerate", "constant",
        #                             "slow")[ringNum(cell)],
        #                     " Cone =", coneNum(cell), "\n\n"))
        #     }
        #     cat(paste("Probability of standing still =", round(Pr[1], 3)))
        #     print(matrix(round(Pr[-1], 2), nrow = 3, byrow = TRUE,
        #                     dimnames = list(speed = c("accelerate", "constant",
        #                                             "slow"),
        #                                     cone = 1:11)))
        #     cat("\n")
        # }

        # Check what to do: Either the chosen cell is 0 (stop) or something else
        # (moving to another location with a different speed and orientation)
        if(cell == 0) {
            speed(agent) <- standing_start  # stopped, reset velocity
        } else {
            position(agent) <- as.vector(m4ma::c_vd_rcpp(
                cells = cell,
                p1 = position(agent),
                v1 = speed(agent),
                a1 = orientation(agent),
                vels = velocities,
                angles = orientations,
                tStep = time_step
            ))
            speed(agent) <- pmax(speed(agent) * c(1.5, 1, 0.5)[m4ma::ringNum(cell)], standing_start)
            orientation(agent) <- (orientation(agent) - ifelse(orientations >= 180, 360 - orientations, -orientations)[m4ma::coneNum(cell)]) %% 360
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
