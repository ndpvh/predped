#' Move an object
#'
#' Move an object around the space. This function changes the positions and/or
#' orientation of the provided object. For the class `agent`, this involves a
#' computation of the utility of a given movement. For other classes, this
#' involves checking whether an agent is interacting with the object before it
#' can move.
#'
#' @param object The object of which the position should be moved.
#' @param ... Additional arguments that can differ for agents or environmental
#' objects.
#'
#' @return The provided object with a changed position and/or orientation
#'
#' @export
#' @docType methods
#' @rdname move-methods
# setGeneric("move", function(object, ...){})


# Create the move_agent function, which will define the generic move function
# for the agents. Consists of the following arguments
#   - agent: The agent with all its information
#   - state: List with the current state of the simulation.
#   - agent_predictions: List with the predicted movements of all agents
#   - nests:
#   - alpha:
#   - iInfo:
#   - angle: Angles to consider
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
move_agent <- function(agent,
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
                       plotGrid = FALSE,
                       printChoice = FALSE,
                       sStop = 0.2, # speed to resume after stop
                       usebestAngle = FALSE,
                       vels = matrix(rep(c(1.5, 1, .5), each = 11), ncol = 3),
                       angles = matrix(rep(c(
                           72.5, 50, 32.5, 20, 10,
                           0, 350, 340, 327.5, 310, 287.5),
                           times = 3),
                           ncol = 3))
{
    # If the agent is currently interacting with another object, just continue
    if(status(agent) == "stop") { # Used to be a check of the goal state: attr(state$P[[n]], "stop") > 0, == -1, or else (interacting, reorient, going)
        cell(agent) <- 0
        check <- matrix(TRUE, 11, 3)
    } else {
        # If the agent stopped their interaction, check whether they already know
        # where the next goal is. If they don't. let them reorient themselves to
        # the next goal
        if(status(agent) == "reorient") {
            orientation(agent) <- best_angle(agent, state, agent_predictions, background, vels, angles)
            status(agent) <- "move"
        }

        # Define the centers of the options to move to
        centers <- m4ma::c_vd_rcpp(
            cells = 1:33,
            p1 = position(agent),
            v1 = speed(agent),
            a1 = orientation(agent),
            vels = vels,
            angles = angles,
            tStep = 0.5
        )

        # Check for occlusions or blocked cells the agent cannot move to
        check <- moving_options(agent, state, background, centers)
        
        # If there are no good options available, or the agent wants to stop,
        # then allow him to
        if(!any(check) | cell(agent) == 0) {  # stop or will have to
            # Change the agent's speed to the starting speed after waiting
            speed(agent) <- sStop

            # Let the agent reorient to find a better way
            orientation(agent) <- best_angle(agent, state, agent_predictions, background, vels, angles)

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
                vels = vels,
                angles = angles,
                tStep = 0.5
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
            speed(agent) <- sStop  # stopped, reset velocity
        } else {
            position(agent) <- as.vector(m4ma::c_vd_rcpp(
                cells = cell,
                p1 = position(agent),
                v1 = speed(agent),
                a1 = orientation(agent),
                vels = vels,
                angles = angles,
                tStep = 0.5
            ))
            speed(agent) <- pmax(speed(agent) * c(1.5, 1, 0.5)[m4ma::ringNum(cell)], sStop)
            orientation(agent) <- (orientation(agent) - ifelse(angles >= 180, 360 - angles, -angles)[m4ma::coneNum(cell)]) %% 360
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
