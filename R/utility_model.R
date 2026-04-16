################################################################################
# HIGH-LEVEL UTILITY FUNCTIONS

# Set up a generic for `utility`. This allows us to differentiate between the 
# function when all utility variables have been precomputed vs when they haven't.
setGeneric("utility", function(object, ...) standardGeneric("utility"))

#' Compute the utilities on the agent level
#' 
#' This function uses the operational-level utility functions to compute the 
#' utility of moving to any given potential cell in \code{centers}. Here, we 
#' assume that none of the utility variables (i.e., the variables that serve as 
#' input to the utility functions) is precomputed, so that it will first compute
#' their values. This input is then provided to 
#' \code{\link[predped]{utility-data.frame}} for the actual computation 
#' of the utility.
#' 
#' @param object Object of the \code{\link[predped]{agent-class}}.
#' @param state Object of the \code{\link[predped]{state-class}}.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param agent_specifications List created by the 
#' \code{\link[predped]{create_agent_specifications}} function. Contains all 
#' information of all agents within the current \code{state} and allows for the
#' communication between the \code{predped} simulation functions and the 
#' \code{m4ma} utility functions.
#' @param centers Numerical matrix containing the coordinates at each position
#' the object can be moved to. Should have one row for each cell.
#' @param check Logical matrix of dimensions 11 x 3 denoting whether an agent 
#' can move to a given cell (\code{TRUE}) or not (\code{FALSE}).
#' @param cpp Logical denoting whether to use the Rcpp version of the function
#' (\code{TRUE}) or the R version (\code{FALSE}). Defaults to \code{TRUE}.
#' 
#' @return Numeric vector denoting the (dis)utility of moving to each of the 
#' cells in \code{centers}.
#' 
#' @seealso 
#' \code{\link[predped]{simulate}},
#' \code{\link[predped]{simulate.state}},
#' \code{\link[predped]{update-agent}},
#' \code{\link[predped]{update}},
#' \code{\link[predped]{utility-data.frame}},
#' \code{\link[predped]{compute_utility_variables}},
#' \code{\link[predped]{update_position}}
#' 
#' @rdname utility-agent
#' 
#' @concept utility
#' 
#' @export
setMethod("utility", "agent", function(object,
                                       state,
                                       background,
                                       agent_specifications,
                                       centers,                    
                                       check,
                                       cpp = TRUE) {

    # If Rcpp alternative wanted, let them use it
    if(cpp) {
        return(utility_agent_rcpp(object,
                                  state,
                                  background,
                                  agent_specifications,
                                  centers,                    
                                  check))
    }

    # Compute the utility variables that are used as input to the utility 
    # functions.
    #
    # Name choice "uv" comes from abbreviating the more informative "utility 
    # variables", which would've otherwise made the code a bit less elegant.
    uv <- compute_utility_variables(object,
                                    state,
                                    background,
                                    agent_specifications,
                                    centers,                    
                                    check)
    uv$check <- list(check)

    # Pass down to a lower-level utility function that uses all of this 
    # information
    return(utility(uv, parameters(object), cpp = cpp))
})

#' Compute the utilities with all utility variables known
#' 
#' This function uses the values of the relevant variables used as input in the
#' utility functions to derive the utility for each of the different moving 
#' options.
#' 
#' @param object Dataframe containing all of the needed information to compute 
#' the utilities. Typically output of the 
#' \code{\link[predped]{compute_utility_variables}} function.
#' @param parameters Dataframe containing the parameters of the agent. Should 
#' conform to the naming conventions mentioned in 
#' \code{\link[predped]{params_from_csv}}.
#' @param cpp Logical denoting whether to use the Rcpp version of the function
#' (\code{TRUE}) or the R version (\code{FALSE}). Defaults to \code{TRUE}.
#' 
#' @return Numeric vector denoting the (dis)utility of moving to each of the 
#' cells.
#' 
#' @seealso 
#' \code{\link[predped]{simulate}},
#' \code{\link[predped]{simulate.state}},
#' \code{\link[predped]{update-agent}},
#' \code{\link[predped]{update}},
#' \code{\link[predped]{utility-agent}},
#' \code{\link[predped]{compute_utility_variables}},
#' \code{\link[predped]{params_from_csv}},
#' \code{\link[predped]{update_position}}
#' 
#' @rdname utility-data.frame
#' 
#' @concept utility
#' 
#' @export 
#
# TO DO
#  - I have the feeling that a lot of the computations on the lower level can
#    be solved by the object-oriented way of dealing with things. For example,
#    we can just access the position of all other agents and define which cell
#    of `agent` they occupy in a more straightforward way than is currently
#    implemented in `get_leaders` and `get_buddy`.
setMethod("utility", "data.frame", function(object,
                                            parameters,
                                            cpp = TRUE) {

    if(cpp) {
        return(utility_rcpp(object, parameters))
    }

    ############################################################################
    # COMPUTATION

    # Create an empty vector of the same size needed for the computation                            
    V <- numeric(length(object$check[[1]]))

    # Preferred speed utility: Check whether the distance to the goal is not 
    # NULL and, if not, compute the utility of deceleration, acceleration, or 
    # maintenance of speed
    if(!is.null(object$ps_distance)) {
        V <- V + m4ma::psUtility_rcpp(parameters[["a_preferred_speed"]], 
                                      parameters[["b_preferred_speed"]], 
                                      parameters[["preferred_speed"]], 
                                      parameters[["slowing_time"]], 
                                      object$ps_speed, 
                                      object$ps_distance)
    }

    # Goal direction utility: Check whether the angle to the goal is defined and,
    # if so, compute the utility of heading in a given direction relative to 
    # where the goal is located
    if (!is.null(object$gd_angle[[1]])) {
        V <- V + m4ma::gaUtility_rcpp(parameters[["b_goal_direction"]], 
                                      parameters[["a_goal_direction"]], 
                                      object$gd_angle[[1]])
    }

    # Current direction utility: Compute the utility of heading in a given 
    # direction. No other variables needed for this.
    V <- V + m4ma::caUtility_rcpp(parameters[["a_current_direction"]], 
                                  parameters[["b_current_direction"]], 
                                  parameters[["blr_current_direction"]])

    # Interpersonal distance utility: Check whether the distance to other 
    # pedestrians is defined and, if so, compute the utility
    if(!is.null(object$id_distance[[1]])) {
        # Take an average instead of a sum. Easily done through dividing by the 
        # number of people in your close vicinity
        ID <- m4ma::idUtility_rcpp(parameters[["b_interpersonal"]], 
                                   parameters[["d_interpersonal"]], 
                                   parameters[["a_interpersonal"]], 
                                   object$id_ingroup[[1]], 
                                   object$id_check[[1]],
                                   object$id_distance[[1]], 
                                   as.vector(ifelse(object$id_check[[1]], 0, -Inf))) # Add precomputed utility here with -Inf for invalid cells; necessary for estimation
        V <- V + ID / length(object$id_ingroup[[1]]) 
    } else {
        V <- V + as.vector(ifelse(object$id_check[[1]], 0, -Inf))
    }

    # Blocked angle utility: Check whether any of the angles are blocked in the 
    # first place, and if so, compute the utility
    if(!is.null(object$ba_angle[[1]])) {
        V <- V + m4ma::baUtility_rcpp(parameters[["a_blocked"]], 
                                      parameters[["b_blocked"]],
                                      pmax(object$ba_angle[[1]], 0), # Make sure all angles are >= 0; this was previously done in baUtility()
                                      object$ba_cones[[1]] - 1)
    }

    # Follow the leader utility: Check whether there are any leaders in the first 
    # place and, if so, compute the utility
    if(!is.null(object$fl_leaders[[1]])) {
        V <- V + m4ma::flUtility_rcpp(parameters[["a_leader"]], 
                                      parameters[["b_leader"]], 
                                      parameters[["d_leader"]], 
                                      object$fl_leaders[[1]][["leaders"]], 
                                      object$fl_leaders[[1]][["dists"]])
    }

    # Walk beside utility: Check whether there are any buddies and, if so, 
    # compute the utility
    if(!is.null(object$wb_buddies[[1]])) {
        V <- V + m4ma::wbUtility_rcpp(parameters[["a_buddy"]], 
                                      parameters[["b_buddy"]], 
                                      object$wb_buddies[[1]][["buddies"]], 
                                      object$wb_buddies[[1]][["dists"]])
    }

    # Group-attracted visual field utility: combine nearest in-group distance
    # attraction and visual-field disutility in one social utility term.
    if(!is.null(object$gvf_distance[[1]])) {
        V <- V + gvf_nearest_utility(parameters[["a_group_centroid"]],
                                     parameters[["b_group_centroid"]],
                                     parameters[["b_visual_field"]],
                                     object$gc_radius,
                                     object$gvf_distance[[1]],
                                     object$gvf_angles[[1]])
    }





    ############################################################################
    # TRANSFORMATION

    # Add the stopping utility to the vector and transform them according to the 
    # randomness parameter
    V_transformed <- c(-parameters[["stop_utility"]], V) / parameters[["randomness"]]

    # Robustness against NAs. Can sometimes occur when you have the difference
    # between Inf - Inf = NA. Should not occur, but might inconvenience one 
    # anyway.
    if(any(is.na(V_transformed))) {
        stop(paste0("NAs found in the utility. ", 
                    "This might occur due to Inf in the parameters: ", 
                    "Check whether parameter values are equal to the bounds. "))
    }

    return(V_transformed)
})

# Set up a generic for `utility`. This allows us to differentiate between the 
# function when all utility variables have been precomputed vs when they haven't.
setGeneric("compute_utility_variables", function(object, ...) standardGeneric("compute_utility_variables"))

#' Compute utility variables
#' 
#' This function uses the current state of the environment to determine the 
#' values of a whole range of variables that are used within the utility 
#' functions.
#' 
#' @param object Object of the \code{\link[predped]{agent-class}}.
#' @param state Object of the \code{\link[predped]{state-class}}.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param agent_specifications List created by the 
#' \code{\link[predped]{create_agent_specifications}} function. Contains all 
#' information of all agents within the current \code{state} and allows for the
#' communication between the \code{predped} simulation functions and the 
#' \code{m4ma} utility functions.
#' @param centers Numerical matrix containing the coordinates at each position
#' the object can be moved to. Should have one row for each cell.
#' @param check Logical matrix of dimensions 11 x 3 denoting whether an agent 
#' can move to a given cell (\code{TRUE}) or not (\code{FALSE}).
#' @param cpp Logical denoting whether to use the Rcpp alternative (\code{TRUE})
#' or the R alternative of this function (\code{FALSE}). Defaults to \code{TRUE}.
#' 
#' @return Data.frame containing all of the needed variables to be able to 
#' compute the values of the utility functions.
#' 
#' @seealso 
#' \code{\link[predped]{simulate}},
#' \code{\link[predped]{simulate.state}},
#' \code{\link[predped]{update-agent}},
#' \code{\link[predped]{update}},
#' \code{\link[predped]{update_position}},
#' \code{\link[predped]{update}}
#' 
#' @rdname compute_utility_variables
#' 
#' @concept utility
#' 
#' @export 
setMethod("compute_utility_variables", "agent", function(object,
                                                         state,
                                                         background,
                                                         agent_specifications,
                                                         centers,                    
                                                         check, 
                                                         cpp = TRUE) {

    # If you want Rcpp to handle everything, let it do so
    if(cpp) {
        return(compute_utility_variables_rcpp(object, 
                                              state,
                                              background,
                                              agent_specifications,
                                              centers,
                                              check))
    }

    # Create a data.frame that will contain all of the needed information in 
    # a single row. This data.frame will already contain the index of the agent
    # of interest, making sure that the single row is already defined.
    #
    # The variable name contains the first two letters of "utility variables" as
    # the more informative name (but too long to spell out here)
    uv <- data.frame(agent_idx = which(agent_specifications$id == id(object)))
    uv$check <- list(check)

    # Preferred speed utility: Required variables are the current speed and the 
    # goal distance
    goal_position <- matrix(current_goal(object)@path[1,], ncol = 2)

    uv$ps_speed <- speed(object)
    uv$ps_distance <- m4ma::dist1_rcpp(position(object), 
                                       goal_position)

    # Goal direction utility: Required variable is the angle between agent and 
    # the goal
    uv$gd_angle <- list(m4ma::destinationAngle_rcpp(orientation(object), 
                                                    position(object, return_matrix = TRUE),
                                                    goal_position) / 90)

    # Interpersonal distance utility: Required variable is the distance between 
    # agent and other agents, and whether these agents are part of the ingroup, 
    # and whether the distances are all positive.
    uv$id_distance <- list(m4ma::predClose_rcpp(uv$agent_idx, 
                                                p1 = position(object, return_matrix = TRUE), 
                                                a1 = orientation(object),
                                                p2 = agent_specifications$position, 
                                                r = agent_specifications$size, 
                                                centres = centers, 
                                                p_pred = agent_specifications$predictions, 
                                                objects = objects(background)))
    
    # Check which cells have only positive distance
    if(!is.null(uv$id_distance[[1]])) {
        uv$id_check <- list(check & apply(uv$id_distance[[1]], 
                                          2, 
                                          \(x) all(x > 0)))
    } else {
        uv$id_check <- list(check)
    }

    # Get names of ingroup agents and check whether these agents are part of the 
    # ingroup or not
    agent_groups <- agent_specifications$group[-uv$agent_idx]
    agent_names <- names(agent_groups[agent_groups == agent_specifications$group[uv$agent_idx]])

    uv$id_ingroup <- list(row.names(uv$id_distance[[1]]) %in% agent_names)

    # Blocked angle utility: Required variable is those angles that might be 
    # blocked in the near future. In other words, we are trying to predict which 
    # directions might lead to collisions in the future
    if(nrow(agent_specifications$predictions) == 1) {
        # When just deleting `agent_idx` from a single-row matrix, we get to 
        # a numeric, not a matrix. Therefore create empty matrix if there is
        # only 1 agent.
        predictions_minus_agent <- matrix(0, nrow = 0, ncol = 2)
    } else {
        # Another weird case is when you only have 2 agents, where it 
        # transforms the matrix to a numerical vector. To ensure there are 
        # no problems, we transform to a matrix and reassign the id's  in 
        # the rows
        predictions_minus_agent <- matrix(agent_specifications$predictions[-uv$agent_idx,],
                                          ncol = 2)
        rownames(predictions_minus_agent) <- agent_specifications$id[-uv$agent_idx]
    }

    uv$ba_angle <- list(m4ma::blockedAngle_rcpp(position(object, return_matrix = TRUE),
                                                orientation(object),
                                                speed(object),
                                                predictions_minus_agent,
                                                agent_specifications$size[-uv$agent_idx],
                                                objects(background)))

    if(is.null(uv$ba_angle[[1]])) {
        uv$ba_cones <- list()
    } else {
        uv$ba_cones <- list(as.integer(names(uv$ba_angle[[1]])))
    }

    # Follow the leader utility: Required variable is the potential leaders and 
    # their distances. This is all outputted in a list by getLeaders_rcpp, which
    # is why we just append it to the data.frame directly
    uv$fl_leaders <- list(m4ma::getLeaders_rcpp(uv$agent_idx,
                                                agent_specifications$position,
                                                agent_specifications$orientation,
                                                agent_specifications$speed,
                                                goal_position,
                                                agent_specifications$group,
                                                centers,
                                                objects(background)))

    # Walking besides utility: Required variable is the potential buddies that 
    # you can walk besides. A similar reasoning to follow the leader is applied 
    # here.
    uv$wb_buddies <- list(m4ma::getBuddy_rcpp(uv$agent_idx,
                                              agent_specifications$position,
                                              agent_specifications$speed,
                                              agent_specifications$group,
                                              agent_specifications$orientation,
                                              agent_specifications$predictions,
                                              centers,
                                              objects(background),
                                              pickBest = FALSE))

    # Group-attracted visual field utility: Required variables are the nearest
    # in-group distance and corresponding relative angles from each potential
    # cell.
    ingroup <- agent_specifications$group[-uv$agent_idx] == agent_specifications$group[uv$agent_idx]
    p_pred <- predictions_minus_agent[ingroup, , drop = FALSE]
    nped <- dim(p_pred)[1]    
    uv$gc_radius <- radius(object)
    
    if (nped == 0) {
        uv$gvf_distance <- list(NULL)
        uv$gvf_angles <- list(NULL)
    } else {
        nearest_member_idx <- which.min(m4ma::dist1_rcpp(position(object), p_pred))
        nearest_member <- p_pred[nearest_member_idx, , drop = FALSE]

        uv$gvf_distance <- list(m4ma::dist1_rcpp(as.numeric(nearest_member), centers))
        uv$gvf_angles <- list(get_angles(uv$agent_idx,
                                         agent_specifications$group,
                                         position(object),
                                         orientation(object),
                                         agent_specifications$predictions,
                                         centers,
                                         any_member = FALSE))
    }

    return(uv)
})

#' Compute utility variables
#' 
#' This function uses the current state of the environment to determine the 
#' values of a whole range of variables that are used within the utility 
#' functions.
#' 
#' @param object Object of the \code{\link[predped]{agent-class}}.
#' @param state Object of the \code{\link[predped]{state-class}}.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param agent_specifications List created by the 
#' \code{\link[predped]{create_agent_specifications}} function. Contains all 
#' information of all agents within the current \code{state} and allows for the
#' communication between the \code{predped} simulation functions and the 
#' \code{m4ma} utility functions.
#' @param centers Numerical matrix containing the coordinates at each position
#' the object can be moved to. Should have one row for each cell.
#' @param check Logical matrix of dimensions 11 x 3 denoting whether an agent 
#' can move to a given cell (\code{TRUE}) or not (\code{FALSE}).
#' 
#' @return Data.frame containing all of the needed variables to be able to 
#' compute the values of the utility functions.
#' 
#' @seealso 
#' \code{\link[predped]{simulate}},
#' \code{\link[predped]{simulate.state}},
#' \code{\link[predped]{update-agent}},
#' \code{\link[predped]{update}},
#' \code{\link[predped]{update_position}},
#' \code{\link[predped]{update}}
#' 
#' @rdname compute_utility_variables
#' 
#' @concept utility
#' 
#' @export 
setMethod("compute_utility_variables", "data.frame", function(object,
                                                              background) {
    # Transform the data to a trace and then back to a dataframe
    trace <- to_trace(object, background)
    return(unpack_trace(trace))
})

    ################################################################################
    # GROUP-ATTRACHED VISUAL FIELD

    #' Group-attracted visual field utility
    #'
    #' Combined social utility based on the nearest in-group member: a distance
    #' attraction term plus a visual-field penalty term.
    #'
    #' @param a_group_centroid Numeric denoting the power for the attraction term.
    #' @param b_group_centroid Numeric denoting the slope of the attraction term.
    #' @param b_visual_field Numeric denoting the visual-field penalty.
    #' @param radius Numeric denoting the radius of the agent.
    #' @param cell_distances Numeric vector with distance from each cell to the
    #' nearest in-group member.
    #' @param relative_angles Numeric vector with relative angle from each cell to
    #' the nearest in-group member.
    #'
    #' @return Numeric vector with utility contribution per cell.
    #'
    #' @rdname gvf_nearest_utility
    #'
    #' @concept utility
    #'
    #' @export
    gvf_nearest_utility <- function(a_group_centroid,
                                    b_group_centroid,
                                    b_visual_field,
                                    radius,
                                    cell_distances,
                                    relative_angles) {

        if (is.null(cell_distances) || is.null(relative_angles)) {
            return(numeric(33))
        }

        optimal_distance <- 1.5 * radius
        attraction <- -sapply(cell_distances,
                              \(x) ifelse(x < optimal_distance,
                                          0,
                                          b_group_centroid * abs(x - optimal_distance)^a_group_centroid))

        lower_angle <- 130 * pi / 180
        upper_angle <- 2 * pi - lower_angle
        visual <- -sapply(relative_angles,
                          \(x) ifelse(x > lower_angle & x < upper_angle,
                                      b_visual_field,
                                      0))

        return(attraction + visual)
    }


################################################################################
# VISUAL FIELD

#' Angle between agent and group members
#' 
#' Finds the angle at which the group members are located compared to the agent.
#' Uses the predicted positions of the group members for this.
#'
#' @param agent_idx Numeric denoting the position of the agent in the prediction 
#' matrix \code{p_pred}.
#' @param agent_group Numeric vector with the group membership of all 
#' pedestrians.
#' @param position Numeric vector denoting the current position of the agent.
#' @param orientation Numeric denoting the current orientation of the agent.
#' @param predictions Numeric matrix with shape N x 2 containing predicted positions
#' of all pedestrians that belong to the social group of the agent.
#' @param centers Numerical matrix containing the coordinates at each position
#' the object can be moved to. Should have one row for each cell.
#' @param any_member Logical denoting whether to consider the angles of all 
#' group members (\code{TRUE}) -- effectively saying that it doesn't matter 
#' which group member the agent can see, as long as they can see one -- or 
#' whether to only consider the nearest group member (\code{FALSE}). Defaults 
#' to \code{TRUE}.
#'
#' @return Numeric vector containing the relative angle of the group member(s)
#' compared to the orientation of the agent within a given cell in \code{centers}.
#' 
#' @seealso 
#' \code{\link[predped]{utility-agent}}
#' \code{\link[predped]{gvf_nearest_utility}}
#' 
#' @rdname get_angles
#' 
#' @concept utility
#' 
#' @export 
#'
get_angles <- function (agent_idx, 
                        agent_group, 
                        position, 
                        orientation,  
                        predictions, 
                        centers,
                        any_member = TRUE) {
    
    # First need to identify whether a pedestrian belongs to a social group
    predictions <- predictions[-agent_idx, , drop = FALSE]
    ingroup <- agent_group[-agent_idx] == agent_group[agent_idx]
    predictions <- predictions[ingroup, , drop = FALSE]
    nped <- dim(predictions)[1]
    
    if (nped == 0) {
        return(NULL)    
    }

    # `any_member` is TRUE, we will find the angle for which the cosine is 
    # maximal, so that we account for any member the agent can see in their 
    # visual field.
    if(any_member) {
        # Get orientations of the cells in for pedestrian `n`.
        orientations <- atan2(centers[,2] - position[2], centers[,1] - position[1])

        # Get angles from cell centers to positions of other pedestrians
        # belonging to the group of pedestrian `n`.
        rel_angles <- lapply(seq_len(nrow(predictions)),
                         \(i) atan2(predictions[i, 2] - centers[,2], predictions[i, 1] - centers[,1]) - orientations) 
        rel_angles <- do.call(cbind, rel_angles)
    
        # Get location of angle on unit circle
        rel_angles <- ifelse(rel_angles < 0, rel_angles + 2*pi, rel_angles)
    
        # Index which angles are maximal
        minimal_angle <- numeric(nrow(rel_angles))
        for (i in seq_len(nrow(rel_angles))) {
            idx <- which.max(cos(rel_angles[i,]))
            minimal_angle[i] <- rel_angles[i, idx]
        }

        rel_angles <- minimal_angle

    # If not, then we only want to account for the closest group member, who 
    # we need to select first
    } else {    
        # Find out which pedestrian will be closest to pedestrian `n` at the next iteration.
        # This means extracting the current_position and calculating the distance 
        # to other pedestrians in the group at the next time step.
        nearest_ped <- m4ma::dist1_rcpp(position, predictions)
        nearest_ped <- predictions[which.min(nearest_ped),]
        orientations <- atan2(centers[,2] - position[2], centers[,1] - position[1])

        # Get angle from cell centers of pedestrian `n`
        # to predicted position of the nearest group member.
        angles <- atan2(nearest_ped[2] - centers[,2], nearest_ped[1] - centers[,1])
    
        # Get relative angle from cell 
        rel_angles <- angles - orientations
        rel_angles <- ifelse(rel_angles < 0, rel_angles + 2*pi, rel_angles)
    }

    # Return the difference between cell centers' orientation angle
    # and angle to position randomly selected group member (in radians).
    return(rel_angles)
}



# #' Transform utility to probability


#     # Compute both kinds of probabilities and multiply to get a total
#     # probability for each of the alternatives
#     between <- between_nests(Vlist, nests, alpha, mu, muM)
#     within <- within_nests(Vlist, nests, alpha, muM)

#     for(i in seq_along(nests)) {
#         within[[i]] <- within[[i]] * between[[i]]
#     }

#     # Sum the different probabilities for a single alternative
#     P <- V
#     idx <- unlist(nests)
#     within <- unlist(within)
#     for(i in seq_along(within)) {
#         P[i] <- sum(within[idx == i])
#     }

#     return(P)
# }
