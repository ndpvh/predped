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
setGeneric("move", function(object, ...){})

#' @rdname move-methods
#' @aliases move, agent
setMethod("move", 
          "agent",
          move_agent)

#' @rdname move-methods
#' @aliases move, object
setMethod("move", 
          "object",
          # Make sure this function has at least roughly the same arguments as 
          # move_agent. This will make documentation a bit easier to read
          function(object, ...){
              # If the object cannot be moved, do not move it
              if(!object@moveable){
                  return(object)
              }

              # Actual movement function: to create myself
          })


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
move_agent <- function(agent,
                       P_n, 
                       p_pred, 
                       nests, 
                       alpha, 
                       objects, 
                       iInfo, 
                       plotGrid = NULL, 
                       printChoice = NULL,
                       sStop = 0.2, # speed to resume after stop
                       usebestAngle = FALSE, 
                       angle = c(-72.5, -50, -32.5, -20, -10, 0, 
                                 10, 20, 32.5, 50, 72.5)){
    

    if (attr(state$P[[n]], "stop") > 0) {  # in interaction wait state
        cell <- 0 
        ok <- TRUE
    } else {  # do update
        if (attr(state$P[[n]], "stop")==-1) { # Starting after interaction, reorient
        turn_report <- FALSE
        attr(state$P[[n]], "stop") <- 0
        p <- toNatural(state$pMat[n, ])
        state$a[n] <- bestAngle(p, n, state, P_n, p_pred, objects, iInfo)
        } else turn_report <- TRUE
        p <- toNatural(state$pMat[n, ])
        muM <- getmuM(p) 
        centres <- c_vd(1:33, p1 = state$p[n, ], v1 = state$v[n], a1 = state$a[n])
        ok <- moving_options(agent, objects, centers)
        if (!any(ok) | state$cell[n]==0) {  # stop or will have to 
        state$v[n] <- sStop
        # cell <- 0
        # # Turn to current goal, sometimes does nothing which can be problematic
        # state$a[n] <- angle2(state$p[n,,drop = FALSE], 
        #   state$P[[n]][attr(state$P[[n]], "i"), 1:2,drop = FALSE])
        # centres <- c_vd(1:33, p1 = state$p[n, ], v1 = state$v[n], a1 = state$a[n])
        # cat(paste(rownames(state$p)[n], "blocked, turning to goal\n"))
        # ok <- get_ok(n, objects, state, centres,state$r[n])
        
        # state$a[n] <- (state$a[n] + 180) %% 360 # and turn around
        # cell <- -1
        best <- bestAngle(p, n, state, P_n, p_pred, objects, iInfo)
        # if (!is.na(best)) {
            state$a[n] <- best
            turn <- paste("to",state$a[n],"degrees")
            cell <- 0
        # } else {
        #   cell <- bestSide(n,centres,objects,state)
        #   if (cell==-1) { # turn 90 degrees anti-clockwise
        #     state$a[n] <- (state$a[n] + 90) %% 360
        #     turn <- "anti-clockwise"
        #   } else if (cell==-2) { # turn backwards
        #     state$a[n] <- (state$a[n] + 180) %% 360
        #     turn <- "around"
        #   } else { # cell==-3, turn 90 degrees clockwise
        #     state$a[n] <- (state$a[n] - 90) %% 360
        #     turn <- "clockwise"
        #   }
        # }
        centres <- c_vd(1:33, p1 = state$p[n, ], v1 = state$v[n], a1 = state$a[n])
        ok <- moving_options(agent, objects, centers)
        if (turn_report) cat(paste(rownames(state$p)[n], "turning",turn,"\n"))
        } 
        V <- utility(p, n, state, P_n, p_pred, centres, objects, ok, 
                    iInfo = iInfo)
        Pr <- pCNLs(V, muM, nests, alpha)
        names(Pr) <- 0:33
        if (!is.null(plotGrid) && plotGrid[n]) {
        draw_grid(state$p[n, ], state$v[n], state$a[n], plotPoints = TRUE, 
                    Pr = Pr[-1])
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