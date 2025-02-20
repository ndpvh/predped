#' Look for the best direction to head in
#'
#' This functions scans the environment and computes the utility of each 
#' possible direction. Used in \code{\link[predped]{update_position}} when 
#' the agent has to reorient itself.
#'
#' @param agent Object of the \code{\link[predped]{agent-class}}.
#' @param state Object of the \code{\link[predped]{state-class}}.
#' @param agent_specifications List containing the specifications of all agents
#' in the state. Is provided here as an argument to increase computational 
#' speed, as you only have to define these specifications once (instead of 
#' multiple times) per iteration. This list is furthermore needed to make the 
#' connection with the \code{m4ma} package.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param velocities Numerical matrix that contains the change in velocity per 
#' cell that the agent might move to.
#' @param orientations Numerical matrix that contains the change in orientation 
#' per cell that the agent might move to.
#' @param step Numeric denoting the change in angle for looking around in 
#' degrees. Defaults to \code{45}, meaning that the agent looks around 
#' exhaustively in 8 different directions (360 / 45) by default. 
#'
#' @return Numeric denoting the angle or direction that had the highest utility
#' (in degrees).
#' 
#' @seealso 
#' \code{\link[predped]{simulate-predped}},
#' \code{\link[predped]{simulate-state}},
#' \code{\link[predped]{update}},
#' \code{\link[predped]{utility}}
#' 
#' @rdname best_angle
#' 
#' @export
#
# Object-oriented replacement of the function `bestAngle`.
best_angle <- function(agent,
                       state,
                       background,
                       agent_specifications,
                       velocities,
                       orientations,
                    #    cores = 1,
                       step = 45,
                       cpp = TRUE) {

    # Get all possible angles with a step-size `step` inbetween them
    new_angles <- step * (0:(360 %/% step))

    # Loop over each of these angles and check what moving-options the agent has
    # for each direction
    utility_angles <- matrix(nrow = length(new_angles), ncol = 2)

    for(i in seq_along(new_angles)){
        orientation(agent) <- new_angles[i]
        # Create centers based on the proposed angle
        centers <- m4ma::c_vd_rcpp(cells = 1:33,
                                   p1 = position(agent),
                                   v1 = speed(agent),
                                   a1 = orientation(agent),
                                   vels = velocities,
                                   angles = orientations,
                                   tStep = 0.5)

        # Check for occlusions or blocked cells the agent cannot move to
        check <- moving_options(agent, state, background, centers)

        # Compute the utility of the proposed cells to move to
        V <- utility(agent, state, background, agent_specifications, centers, check)[-1] # Omit utility for cell 0
        utility_angles[i, ] <- c(max(V), orientation(agent))
    }

    # Return the results that were given by the angle with the greatest utility
    return(utility_angles[which.max(utility_angles[ ,1]), 2])
}

# DEPRECATED FUNCTION, BUT LEFT IN FOR LEGACY. 
#
# It's function is taken over by prune_edges
#
# #' Can agent see a point in space
# #'
# #' Determines whether an agent can see a point in space. Is an abstraction of the
# #' more specific functions that allowed agents to see goals (`seesGoal`) or
# #' other agents (`seesMany`). These specific cases will still exist alongside
# #' this more general alternative.
# #'
# #' @param agent The agent
# #' @param co A coordinate that will be tested against the agents' point of view
# #' @param objects The list of objects that can obstruct the view of the agent
# #'
# #' @return Boolean indicating whether the agent can see the point
# #' (`TRUE`) or not (`FALSE`)
# #
# # The original logic stems from the function `seesGoal`
# sees_location <- function(agent,
#                           co,
#                           objects) {

#     # Check whether the line of sight between an agent and their current goal
#     # is obstructed by any of the objects
#     for(i in seq_along(objects)) {
#         # Create an intersection matrix for the polygon in `objects[[i]]`
#         points <- objects[[i]]@points
#         intersects <- matrix(0, nrow = nrow(points) - 1, ncol = 2)

#         # Loop over all different lines that are created by the polygon
#         for(j in seq_len(nrow(points) - 1)) {
#             intersects[j,] <- m4ma::line.line.intersection(position(agent),
#                                                            co,
#                                                            points[j,],
#                                                            points[j + 1,],
#                                                            interior.only = TRUE)
#         }

#         # Check if any intersection was found, and if so, return `FALSE` as the
#         # response to the question `can the agent see their goal`
#         if(any(is.finite(intersects))) {
#             return(FALSE)
#         }
#     }

#     # If no object was found to intersect with the line of sight of an agent,
#     # return `TRUE`
#     return(TRUE)
# }

# #' Can agent see the goal
# #'
# #' Determines whether an agent can see the current goal in any of the directions
# #' around them. Accounts for the occlusion of a goal by an object.
# #'
# #' @param agent The agent
# #' @param objects The list of objects that can obstruct the view of the agent
# #'
# #' @return Boolean indicating whether the agent can see the goal of interest
# #' (`TRUE`) or not (`FALSE`)
# #
# # Original function: `seesGoal`
# sees_goal <- function(agent, objects) {
#     return(sees_location(agent, agent@current_goal@position, objects))
# }

# #' Can agent see multiple locations
# #'
# #' Determines whether an agent can see not one, but multiple locations in space.
# #'
# #' @param agent The agent
# #' @param objects_to_see A list of objects that the agent should be able to see
# #' @param objects The list of objects that can obstruct the view of the agent
# #
# # Original function: `seesMany`
# sees_multiple_locations <- function(agent,
#                                     objects_to_see,
#                                     objects) {
#     return(lapply(objects_to_see,
#                   function(x) sees_location(agent, x@position, objects)))
# }








# # seesGoal for current goal
# seesCurrentGoal <- function(n, state, objects, offset = 0) {
#   seesGoal(state$p[n, ], state$P[[n]][attr(state$P[[n]], "i") + offset, 1:2],
#            objects)
# }



# # Boolean indicating if goal is visible from ok cells
# seesGoalOK <- function(n, objects, state, centres, ok) {
#   if (any(ok)) {
#     for (i in c(1:33)[ok]) {
#       ok[i] <- seesGoal(centres[i, ],
#                         state$P[[n]][attr(state$P[[n]], "i"), 1:2],
#                         objects)
#     }
#   }
#   return(ok)
# }
