#' Scan environment for the best direction
#' 
#' This functions scans the environment in a given stepsize and checks the utility
#' of each possible angle. 
#' 
#' @param agent The agent that will reorient themselves
#' @param state The list that contains the current state of the simulation
#' @param P_n The goal to move to next
#' @param agent_predictions The predicted trajectories of the other agents
#' @param iInfo 
#' @param step The stepsize in the angles to be checked
#' 
#' @return The angle that has the highest utility to move to
#' 
#' TO DO: 
#'  - Change the goal argument
#'  - Write tests for the convergence of this function with its predecessor, 
#'    especially with regard to angles that are blocked
#' 
#' Object-oriented replacement of the function `bestAngle`.
best_angle <- function(agent, 
                       state,
                       agent_predictions, 
                       iInfo, 
                    #    cores = 1,
                       step = 45) {  
    
    # Get all possible angles with a step-size `step` inbetween them
    angles <- step * (0:(360 %/% step))

    # Loop over each of these angles and check what moving-options the agent has
    # for each direction
    utility_angles <- matrix(nrow = length(angles), ncol = 2)
    for(i in seq_along(angles)){
        # Create centers based on the proposed angle
        centers <- compute_cell_centers(1:33,
                                        agent@position, 
                                        agent@speed, 
                                        angles[i])

        # Check for occlusions or blocked cells the agent cannot move to
        check <- moving_options(agent, state, centers)

        # Compute the utility of the proposed cells to move to
        V <- utility(agent@parameters, agent, state, agent_predictions, check, iInfo)[-1]
        utility_angles[i,] <- V
    }
    
    # Return the results that were given by the angle with the greatest utility
    return(utility_angles[which.max(utility_angles[,1]), 2])
}

#' Can agent see an point in space
#' 
#' Determines whether an agent can see a point in space. Is an abstraction of the 
#' more specific functions that allowed agents to see goals (`seesGoal`) or 
#' other agents (`seesMany`). These specific cases will still exist alongside 
#' this more general alternative.
#' 
#' @param agent The agent
#' @param co A coordinate that will be tested against the agents' point of view
#' @param objects The list of objects that can obstruct the view of the agent
#' 
#' @return Boolean indicating whether the agent can see the point
#' (`TRUE`) or not (`FALSE`)
#' 
#' The original logic stems from the function `seesGoal`
sees_location <- function(agent, 
                          co, 
                          objects) {

    # Check whether the line of sight between an agent and their current goal 
    # is obstructed by any of the objects
    for(i in seq_along(objects)) {
        # Create an intersection matrix for the polygon in `objects[[i]]`
        points <- objects[[i]]@points
        intersects <- matrix(0, nrow = nrow(points) - 1, ncol = 2)

        # Loop over all different lines that are created by the polygon
        for(j in 2:nrow(points)) {
            intersects[j,] <- line.line.intersection(agent@position, 
                                                     co, 
                                                     points[j - 1,], 
                                                     points[j,], 
                                                     interior.only = TRUE)
        }

        # Check if any intersection was found, and if so, return `FALSE` as the 
        # response to the question `can the agent see their goal`
        if(any(is.finite(intersects))) {
            return(FALSE)
        }
    }

    # If no object was found to intersect with the line of sight of an agent, 
    # return `TRUE`
    return(TRUE)
}

#' Can agent see the goal
#' 
#' Determines whether an agent can see the current goal in any of the directions 
#' around them. Accounts for the occlusion of a goal by an object.
#' 
#' @param agent The agent
#' @param objects The list of objects that can obstruct the view of the agent
#' 
#' @return Boolean indicating whether the agent can see the goal of interest
#' (`TRUE`) or not (`FALSE`)
#' 
#' Original function: `seesGoal`
sees_goal <- function(agent, objects) {
    return(sees_location(agent, agent@current_goal@position, objects))
}

#' Can agent see multiple locations
#' 
#' Determines whether an agent can see not one, but multiple locations in space.
#' 
#' @param agent The agent
#' @param objects_to_see A list of objects that the agent should be able to see
#' @param objects The list of objects that can obstruct the view of the agent
#' 
#' Original function: `seesMany`
sees_multiple_locations <- function(agent, 
                                    objects_to_see,
                                    objects) {
    return(lapply(objects_to_see, 
                  function(x) sees_location(agent, x@position, objects)))
}








# seesGoal for current goal
seesCurrentGoal <- function(n, state, objects, offset = 0) {
  seesGoal(state$p[n, ], state$P[[n]][attr(state$P[[n]], "i") + offset, 1:2],
           objects)
}



# Boolean indicating if goal is visible from ok cells  
seesGoalOK <- function(n, objects, state, centres, ok) {
  if (any(ok)) {
    for (i in c(1:33)[ok]) {
      ok[i] <- seesGoal(centres[i, ], 
                        state$P[[n]][attr(state$P[[n]], "i"), 1:2], 
                        objects) 
    }
  }
  return(ok)
}