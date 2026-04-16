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

    # Unified Local Group-Attracted Visual Field (LLGVF) utility: Check whether 
    # there are any group members and, if so, compute the utility
    if(!is.null(object$llgvf_data[[1]])) {
        V <- V + local_gvf_utility(parameters[["a_llgvf"]], 
                                   parameters[["b_llgvf"]], 
                                   parameters[["e_llgvf"]], 
                                   object$llgvf_data[[1]][["distances"]], 
                                   object$llgvf_data[[1]][["rel_angles"]])
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

# Set up a generic for `compute_utility_variables`
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
#' \code{\link[predped]{create_agent_specifications}} function. 
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
#' @rdname compute_utility_variables
#' @concept utility
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

    uv <- data.frame(agent_idx = which(agent_specifications$id == id(object)))
    uv$check <- list(check)

    # Preferred speed utility
    goal_position <- matrix(current_goal(object)@path[1,], ncol = 2)

    uv$ps_speed <- speed(object)
    uv$ps_distance <- m4ma::dist1_rcpp(position(object), goal_position)

    # Goal direction utility
    uv$gd_angle <- list(m4ma::destinationAngle_rcpp(orientation(object), 
                                                    position(object, return_matrix = TRUE),
                                                    goal_position) / 90)

    # Interpersonal distance utility
    uv$id_distance <- list(m4ma::predClose_rcpp(uv$agent_idx, 
                                                p1 = position(object, return_matrix = TRUE), 
                                                a1 = orientation(object),
                                                p2 = agent_specifications$position, 
                                                r = agent_specifications$size, 
                                                centres = centers, 
                                                p_pred = agent_specifications$predictions, 
                                                objects = objects(background)))
    
    if(!is.null(uv$id_distance[[1]])) {
        uv$id_check <- list(check & apply(uv$id_distance[[1]], 2, \(x) all(x > 0)))
    } else {
        uv$id_check <- list(check)
    }

    agent_groups <- agent_specifications$group[-uv$agent_idx]
    agent_names <- names(agent_groups[agent_groups == agent_specifications$group[uv$agent_idx]])
    uv$id_ingroup <- list(row.names(uv$id_distance[[1]]) %in% agent_names)

    # Blocked angle utility
    if(nrow(agent_specifications$predictions) == 1) {
        predictions_minus_agent <- matrix(0, nrow = 0, ncol = 2)
    } else {
        predictions_minus_agent <- matrix(agent_specifications$predictions[-uv$agent_idx,], ncol = 2)
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

    # Follow the leader utility
    uv$fl_leaders <- list(m4ma::getLeaders_rcpp(uv$agent_idx,
                                                agent_specifications$position,
                                                agent_specifications$orientation,
                                                agent_specifications$speed,
                                                goal_position,
                                                agent_specifications$group,
                                                centers,
                                                objects(background)))

    # Local Group-Attracted Visual Field (Unified Group Dynamics)
    uv$llgvf_data <- list(get_nearest_member_data(uv$agent_idx,
                                                  agent_specifications$group,
                                                  position(object),
                                                  orientation(object),
                                                  agent_specifications$predictions,
                                                  centers))

    return(uv)       
})

#' Compute utility variables
#' 
#' @export 
setMethod("compute_utility_variables", "data.frame", function(object,
                                                              background) {
    # Transform the data to a trace and then back to a dataframe
    trace <- to_trace(object, background)
    return(unpack_trace(trace))
})


################################################################################
# LOCAL GROUP DYNAMICS (UNIFIED LLGVF)

#' Get Distance and Angle to Nearest Group Member
#' 
#' Finds the predicted position of the nearest group member and calculates the
#' distance and relative angle from all candidate cells to that single member.
#' This replaces the old Walk Beside, Group Centroid, and Visual Field methods.
#'
#' @param agent_idx Numeric denoting the position of the agent in the predictions.
#' @param agent_group Numeric vector with the group membership of all pedestrians.
#' @param position Numeric vector denoting the current position of the agent.
#' @param orientation Numeric denoting the current orientation of the agent.
#' @param predictions Numeric matrix with shape N x 2 containing predicted positions
#' @param centers Numerical matrix containing the coordinates at each candidate cell.
#'
#' @return A list containing the distances and relative angles to the nearest group member.
#' 
#' @seealso 
#' \code{\link[predped]{local_gvf_utility}},
#' \code{\link[predped]{utility-agent}}
#' 
#' @concept utility
#' @export
get_nearest_member_data <- function(agent_idx, 
                                    agent_group, 
                                    position, 
                                    orientation, 
                                    predictions, 
                                    centers) {
    
    # Identify in-group pedestrians
    predictions <- predictions[-agent_idx, , drop = FALSE]
    ingroup <- agent_group[-agent_idx] == agent_group[agent_idx]
    predictions <- predictions[ingroup, , drop = FALSE]
    nped <- dim(predictions)[1]
    
    if (nped == 0) {
        return(NULL)    
    }
    
    # Find the single nearest group member based on the agent's *current* position
    # dist1_rcpp returns a vector of distances when comparing a point to a matrix
    distances_to_group <- m4ma::dist1_rcpp(as.numeric(position), predictions)
    nearest_idx <- which.min(distances_to_group)
    nearest_ped <- predictions[nearest_idx, ]
    
    # Calculate distances from all candidate cells to the nearest member
    distances <- m4ma::dist1_rcpp(as.numeric(nearest_ped), centers)
    
    # Calculate relative angles from all candidate cells to the nearest member
    orientations <- atan2(centers[,2] - position[2], centers[,1] - position[1])
    angles <- atan2(nearest_ped[2] - centers[,2], nearest_ped[1] - centers[,1])
    rel_angles <- angles - orientations
    
    # Normalize angles to [-pi, pi]
    rel_angles <- ifelse(rel_angles < -pi, rel_angles + 2*pi, rel_angles)
    rel_angles <- ifelse(rel_angles > pi, rel_angles - 2*pi, rel_angles)
    
    return(list(distances = distances, rel_angles = rel_angles))
}

#' Local Logarithmic Group-Attracted Visual Field Utility (LLGVF)
#' 
#' Unifies distance attraction and visual field alignment into a single local dynamic.
#' Applies a logarithmic penalty based on distance to the nearest group member, and 
#' adds an additional penalty if that member is outside the extended visual field.
#'
#' @param a_llgvf Numeric denoting the exponent (shape) of the utility function.
#' @param b_llgvf Numeric denoting the slope (weight) of the utility function.
#' @param e_llgvf Numeric denoting the optimal comfortable distance (epsilon) to maintain.
#' @param distances Numeric vector of distances from candidate cells to the member.
#' @param rel_angles Numeric vector of relative angles from candidate cells to the member.
#' @param vf_limit Numeric denoting the visual field limit (default 135 degrees in radians).
#'
#' @return Numeric vector containing the LLGVF utility for each cell. 
#' 
#' @seealso 
#' \code{\link[predped]{get_nearest_member_data}},
#' \code{\link[predped]{utility-agent}}
#' 
#' @concept utility
#' @export
local_gvf_utility <- function(a_llgvf, 
                              b_llgvf, 
                              e_llgvf, 
                              distances, 
                              rel_angles, 
                              vf_limit = 135 * pi / 180) {
    
    if (is.null(distances) || is.null(rel_angles)) {
        return(numeric(33))
    }
    
    # Calculate base attraction utility to the comfortable distance (epsilon)
    base_util <- -b_llgvf * abs(log(distances) - log(e_llgvf))^a_llgvf
    
    # Calculate penalty for not having the member in the visual field
    in_vf <- abs(rel_angles) <= vf_limit
    penalty <- ifelse(in_vf, 0, -b_llgvf / (distances^a_llgvf))
    
    return(base_util + penalty)
}