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
# #' @export
#' @docType methods
#' @rdname move-methods
# setGeneric("move", function(object, ...){})


# Create the move_agent function, which will define the generic move function 
# for the agents. Consists of the following arguments
#   - agent: The agent with all its information
#   - P_n: 
#   - p_pred: 
#   - nests: 
#   - alpha: 
#   - objects: 
#   - iInfo: 
#   - angle: Angles to consider
#
# TO DO
#   - At this moment, the same checks happen here and in `best_angle`: Try to 
#     find a way to decrease this burden (`move_options`). Happesn twice in this
#     function alone: Once for initial computation, and once when this initial 
#     computation does not pan out
#   - Do we want to use the agent@busy for the different options?
move_agent <- function(agent,
                       state,
                       P_n,
                       agent_predictions, 
                       nests, 
                       alpha,
                       iInfo, 
                       plotGrid = NULL, 
                       printChoice = NULL,
                       sStop = 0.2, # speed to resume after stop
                       usebestAngle = FALSE, 
                       angle = c(-72.5, -50, -32.5, -20, -10, 0, 
                                 10, 20, 32.5, 50, 72.5)) {
    
    # If the agent is currently interacting with another object, just continue
    if(agent@busy) { # Used to be a check of the goal state: attr(state$P[[n]], "stop") > 0, == -1, or else (interacting, reorient, going)
        agent@cell <- 0
        check <- TRUE
    } else {
        # Transform the parameters
        agent@parameters <- transform_exponentiate(agent@parameters)
        mu <- transform_mu(agent@parameters) 

        # If the agent stopped their interaction, check whether they already know
        # where the next goal is. If they don't. let them reorient themselves to 
        # the next goal
        if(!agent@reoriented) {
            agent@angle <- best_angle(agent, state, P_n, agent_predictions, iInfo)
            agent@reoriented <- TRUE
        }

        # Define the centers of the options to move to
        centers <- compute_cell_centers(1:33,
                                        agent@position, 
                                        agent@speed, 
                                        angles[i])

        # Check for occlusions or blocked cells the agent cannot move to
        check <- moving_options(agent, state, centers)

        # If there are no good options available, or the agent wants to stop, 
        # then allow him to
        if(!any(check) | state$agent[n]==0) {  # stop or will have to 
            # Change the agent's speed to the starting speed after waiting
            agent@speed <- sStop 

            # Let the agent reorient to find a better way
            agent@angle <- best_angle(agent, state, P_n, agent_predictions, iInfo)
            
            # Report the degress that the agent is reorienting to
            turn <- paste("to", agent@angle, "degrees")
            if(agent@reoriented) {
                paste(agent@id, "turning", turn, "\n") |>
                    cat()
            }
            
            #cell <- 0
            
            # Define the centers of the options to move to
            centers <- compute_cell_centers(1:33,
                                            agent@position, 
                                            agent@speed, 
                                            angles[i])

            # Check for occlusions or blocked cells the agent cannot move to
            check <- moving_options(agent, state, centers)            
        }

        # Compute the utility of of each option and transform the utilities to 
        # probabilities
        V <- utility(agent@parameters, agent, state, P_n, agent_predictions, check, iInfo)
        Pr <- pCNLs(V, mu, nests, alpha)


        names(Pr) <- 0:33
        if (!is.null(plotGrid) && plotGrid[n]) {
        draw_grid(state$p[n, ], state$v[n], state$a[n], plotPoints = TRUE, 
                    Pr = Pr[-1])
        }
    }


 
        
        cell <- sample.int(length(V), 1, TRUE, prob = Pr) - 1
        if (!is.null(printChoice) && printChoice[n]) {
        if (cell == 0) {
            cat("Standing still\n\n")
        } else {
            cat(paste("\nPedestrian =", row.names(state$p)[n], 
                    " Choice =", cell, 
                    " Ring =", c("accelerate", "constant", 
                                "slow")[ringNum(cell)],
                        " Cone =", coneNum(cell), "\n\n"))
        }
        cat(paste("Probability of standing still =", round(Pr[1], 3)))
        print(matrix(round(Pr[-1], 2), nrow = 3, byrow = TRUE,
                        dimnames = list(speed = c("accelerate", "constant", 
                                                "slow"),
                                        cone = 1:11)))
        cat("\n")
        }
        if (cell != 0) {
            state$p[n, ] <- c_vd(cell, state$p[n, ], state$v[n], state$a[n])
            state$v[n] <- pmax(state$v[n] * c(1.5, 1, .5)[ringNum(cell)], sStop)
            state$a[n] <- (state$a[n] - angle[coneNum(cell)]) %% 360
        } else {
            state$v[n] <- sStop  # stopped, reset velocity
        }
    }

    # Change the agent characteristics based on the results of this function
    agent@position <- 
    agent@velocity <- 
    out <- list(p = state$p[n, ], v = state$v[n], a = state$a[n], r = state$r[n], 
                group = state$group[n], pMat = state$pMat[n, ], cell = cell,P=state$P[[n]])
    attr(out, "ok") <- ok
    
    # # Bug catch for outside total area
    # if (!inObject(state$p[n,,drop=F],xlim=objects[[1]]$x,ylim=objects[[1]]$y,outside=FALSE))
    #   stop(n)
    
    out
}