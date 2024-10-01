################################################################################
# HIGH-LEVEL UTILITY FUNCTIONS

#' Compute the utilities
#' 
#' This function uses the operational-level utility functions to compute the 
#' utility of moving to any given potential cell in \code{centers}.
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
#' @return Numeric matrix denoting the (dis)utility of moving to each of the 
#' cells in \code{centers}.
#' 
#' @seealso 
#' \code{\link[predped]{simulate,predped-method}},
#' \code{\link[predped]{simulate,state-method}},
#' \code{\link[predped]{update,agent-method}},
#' \code{\link[predped]{update,state-method}},
#' \code{\link[predped]{update_position}}
#' 
#' @rdname utility
#' 
#' @export 
#
# TO DO
#  - Is `check` a necessary argument here, or could we just only put the checked
#    centers in them
#  - Maybe make `precomputed` a list that contains the precomputed items? Then
#    we can just simply extract them from this list instead of from `state`:
#    We should think about this, as this information is not directly available
#    from `state` anymore if this becomes a list of agents and objects
#  - Make it so so that you can just give a set of different utilities that
#    you want accounted for, making the utility function on the lower level
#    highly manipulable at the higher level
#  - I have the feeling that a lot of the computations on the lower level can
#    be solved by the object-oriented way of dealing with things. For example,
#    we can just access the position of all other agents and define which cell
#    of `agent` they occupy in a more straightforward way than is currently
#    implemented in `get_leaders` and `get_buddy`.
#  - Make each of the utilities a class of their own, with the method
#    `compute` to compute the specific utility it needs to compute. This will
#    allow for a very easy combination of utilities that can be changed at the
#    upper level
#  - Unless I am mistaken, iInfo is not used in this function and can thus be
#    deleted
utility <- function(agent,
                    state,
                    background,
                    agent_specifications,
                    centers,                    
                    check) {
                    # Deprecated
                    # precomputed = FALSE,
                    # subject = TRUE) {

    # If the different parts that will make up the utilities were not precomputed,
    # compute them here.
    #
    # Left subject-based and iteration-based relatively unchanged as we first
    # have to think about how we will save this information. Maybe just make it
    # an agent-characteristic?
    # if(!precomputed) {

    # Get the index of all other agents in the agent_specifications
    agent_idx <- seq_along(agent_specifications$id)[agent_specifications$id == id(agent)]

    # Preferred speed
    goal_position <- matrix(current_goal(agent)@path[1,],
                            ncol = 2)
    goal_distance <- m4ma::dist1_rcpp(position(agent), 
                                      goal_position)

    # Angle between agent and the goal
    direction_goal <- m4ma::destinationAngle_rcpp(orientation(agent), 
                                                  position(agent, return_matrix = TRUE),
                                                  goal_position) / 90

    # Interpersonal distance between agent and other agents
    interpersonal_distance <- m4ma::predClose_rcpp(agent_idx, 
                                                   p1 = position(agent, return_matrix = TRUE), 
                                                   a1 = orientation(agent),
                                                   p2 = agent_specifications$position, 
                                                   r = agent_specifications$size, 
                                                   centres = centers, 
                                                   p_pred = agent_specifications$predictions, 
                                                   objects = objects(background))

    # Predict which directions might lead to collisions in the future
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
        predictions_minus_agent <- matrix(agent_specifications$predictions[-agent_idx,],
                                          ncol = 2)
        rownames(predictions_minus_agent) <- agent_specifications$id[-agent_idx]
    }

    blocked_angle <- m4ma::blockedAngle_rcpp(position(agent, return_matrix = TRUE),
                                             orientation(agent),
                                             speed(agent),
                                             predictions_minus_agent,
                                             agent_specifications$size[-agent_idx],
                                             objects(background))

    # Follow the leader phenomenon
    leaders <- m4ma::getLeaders_rcpp(agent_idx,
                                     agent_specifications$position,
                                     agent_specifications$orientation,
                                     agent_specifications$speed,
                                     goal_position,
                                     agent_specifications$group,
                                     centers,
                                     objects(background))
    # leaders <- NULL

    # Walking besides a buddy
    buddies <- m4ma::getBuddy_rcpp(agent_idx,
                                   agent_specifications$position,
                                   agent_specifications$speed,
                                   agent_specifications$group,
                                   agent_specifications$orientation,
                                   agent_specifications$predictions,
                                   centers,
                                   objects(background),
                                   pickBest = FALSE)

    # Group Centroid Phenomenon                                       
    inGroup <- agent_specifications$group[-agent_idx] == agent_specifications$group[agent_idx]
    p_pred <- predictions_minus_agent[inGroup, , drop = FALSE]
    nped <- dim(p_pred)[1]    
        
    distance_centroid <- distance_group_centroid(p_pred,
                                                 centers,
                                                 nped)

    # Visual Field Phenomenon
    buddies_in_vf <- get_angles(agent_idx,
                                agent_specifications$group,
                                orientation(agent),
                                position(agent),
                                agent_specifications$predictions,
                                centers,
                                any_member = TRUE)                                                     

    # Does not work at this moment, and so is left commented out
    #
    # Subject based
    # } else if (subject) {
    #     check <- state$check
    #     goal_distance <- state$goal_distance[n]
    #     direction_goal <- state$direction_goal
    #     interpersonal_distance <- state$interpersonal_distance
    #     blocked_angle <- state$blocked_angle
    #     leaders <- state$leaders
    #     buddies <- state$buddies
    # # Iteration based
    # } else {
    #     check <- state$check[[n]]
    #     goal_distance <- state$goal_distance[n]
    #     direction_goal <- state$direction_goal[[n]]
    #     interpersonal_distance <- state$interpersonal_distance[[n]]
    #     blocked_angle <- state$blocked_angle[[n]]
    #     leaders <- state$leaders[[n]]
    #     buddies <- state$buddies[[n]]
    # }
    #}

    # Compute the utilities and sum them up
    p <- parameters(agent)

    V <- numeric(nrow(centers))

    # Always check if utility data is NULL
    if (!is.null(goal_distance)) {
        V <- V + m4ma::psUtility_rcpp(p[["a_preferred_speed"]], 
                                      p[["b_preferred_speed"]], 
                                      p[["preferred_speed"]], 
                                      p[["slowing_time"]], 
                                      speed(agent), 
                                      goal_distance)
    }

    if (!is.null(direction_goal)) {
        V <- V + m4ma::gaUtility_rcpp(p[["b_goal_direction"]], 
                                      p[["a_goal_direction"]], 
                                      direction_goal)
    }

    V <- V + m4ma::caUtility_rcpp(p[["a_current_direction"]], 
                                  p[["b_current_direction"]], 
                                  p[["blr_current_direction"]])

    if (!is.null(interpersonal_distance)) {
        # The next lines used to be in idUtility_rcpp but are not depending on parameter values therefore
        # they have been taken out to speed up the estimation
        # Get names of ingroup agents
        agent_groups <- agent_specifications$group[-agent_idx]
        names_ingroup <- names(agent_groups[agent_groups == agent_specifications$group[agent_idx]])

        # Check if agent is part of in group
        is_ingroup <- row.names(interpersonal_distance) %in% names_ingroup

        # Check which cells have only positive distance
        check <- check & apply(interpersonal_distance, 2, function(x) {
            all(x > 0)
        })

        V <- V + m4ma::idUtility_rcpp(p[["b_interpersonal"]], 
                                      p[["d_interpersonal"]], 
                                      p[["a_interpersonal"]], 
                                      is_ingroup, 
                                      check, 
                                      interpersonal_distance, 
                                      as.vector(ifelse(check, 0, -Inf))) # Add precomputed utility here with -Inf for invalid cells; necessary for estimation
    } else {
        V <- V + as.vector(ifelse(check, 0, -Inf))
    }

    if (!is.null(blocked_angle)) {
        V <- V + m4ma::baUtility_rcpp(p[["a_blocked"]], 
                                      p[["b_blocked"]],
                                      pmax(blocked_angle, 0), # Make sure all angles are >= 0; this was previously done in baUtility()
                                      as.integer(names(blocked_angle)) - 1)
    }

    if (!is.null(leaders)) {
        V <- V + m4ma::flUtility_rcpp(p[["a_leader"]], 
                                      p[["b_leader"]], 
                                      p[["d_leader"]], 
                                      leaders[["leaders"]], 
                                      leaders[["dists"]])
    }

    if (!is.null(buddies)) {
        V <- V + m4ma::wbUtility_rcpp(p[["a_buddy"]], 
                                      p[["b_buddy"]], 
                                      buddies[["buddies"]], 
                                      buddies[["dists"]])
    }

    if (!is.null(distance_centroid)) {
        V <- V + gc_utility(p[["a_group_centroid"]],
                            p[["b_group_centroid"]],
                            radius(agent),
                            distance_centroid,
                            -p[["stop_utility"]],
                            nped)
    }

    if (!is.null(buddies_in_vf)) {
        V <- V + vf_utility_discrete(p[["b_visual_field"]],
                                     buddies_in_vf)
    }

    V_transformed <- c(-p[["stop_utility"]], V) / p[["randomness"]]

    # Robustness against NAs. Can sometimes occur when you have the difference
    # between Inf - Inf = NA. Should not occur, but might inconvenience one 
    # anyway.
    if(any(is.na(V_transformed))) {
        stop(paste0("NAs found in the utility. ", 
                    "This might occur due to Inf in the parameters: ", 
                    "Check whether parameter values are equal to the bounds. "))
    }

    return(V_transformed)
}





################################################################################
# GROUP CENTROID

#' Distances to group centroid
#'
#' Compute the distance of a given agent to the group centroid. This group 
#' centroid is computed as a summary statistic of the predicted x- and y-
#' coordinates of all pedestrians belonging to the same group as the agent. The 
#' summary statistic of choice should be one of mean-tendency, but can be 
#' specified by the user through the argument \code{fx}.
#' 
#' @details 
#' Note that this function has been defined to be in line with the \code{m4ma}
#' utility functions.
#'
#' @param p_pred Numeric matrix with shape N x 2 containing predicted positions
#' of all pedestrians that belong to the social group of the agent.
#' @param centers Numerical matrix containing the coordinates at each position
#' the object can be moved to. Should have one row for each cell.
#' @param nped Numeric integer indicating number of people in pedestrian `n`'s social group. 
#' @param fx Function used to find the group centroid. Defaults to \code{mean}
#'
#' @return Numeric vector containing the distance from each cell in the `center`
#' to the group centroid. If not other agents belong to the same group as the 
#' agent, returns \code{NULL}.
#' 
#' @seealso 
#' \code{\link[predped]{gc_utility}},
#' \code{\link[predped]{utility}}
#' 
#' @rdname distance_group_centroid 
#'
#' @export 
distance_group_centroid <- function(p_pred, 
                                    centers, 
                                    nped,
                                    fx = mean) {
   
    # First need to identify whether a pedestrian belongs to a social group    
    if (nped == 0) {
        return(NULL)    
    }

    # All of the positions of all in-group pedestrians need to be averaged
    # Which represents the group centroid
    centroid <- c(fx(p_pred[,1]), fx(p_pred[,2]))

    # Euclidean distance for pedestrian_i is calculated for each cell
    return(m4ma::dist1_rcpp(centroid, centers))
}

#' Group centroid utility
#' 
#' The parameters that are used by this function -- and which are defined in 
#' \code{\link[predped]{params_from_csv}} -- are \code{a_group_centroid} and 
#' \code{b_group_centroid}.
#'
#' @param a_gc Numeric denoting the power to which to take the utility.
#' @param b_gc Numeric denoting the slope of the utility function.
#' @param radius Numeric denoting the radius of the agent.
#' @param cell_dist Numeric vector denoting the distance of each cell in the 
#' \code{centers} to the predicted group centroid.
#' @param stop_utility Numeric denoting the utility of stopping. Is used to 
#' ensure the agents do not freeze when they are too far away from each other. 
#' @param nped Numeric denoting the number of ingroup members. 
#' 
#' @return Numeric vector containing the group-centroid-related utility for each 
#' cell. 
#' 
#' @seealso 
#' \code{\link[predped]{distance_group_centroid}},
#' \code{\link[predped]{params_from_csv}},
#' \code{\link[predped]{utility}}
#' 
#' @rdname gc_utility
#' 
#' @export
gc_utility <- function(a_gc, 
                       b_gc, 
                       radius, 
                       cell_dist, 
                       stop_utility, 
                       nped) {
    
    # No utilities to be added whenever there are is no group centroid to 
    # account for (see `distance_group_centroid`).
    if (is.null(cell_dist)) {
        return(numeric(33))
    }

    optimal_distance <- 1.5 * nped * radius
    
    # Calculate centroid 
    centroid_util <- -sapply(cell_dist, 
                                \(x) ifelse(x < optimal_distance, 0, b_gc * (x - optimal_distance)^a_gc))

    if (all(centroid_util < stop_utility)) {
        return(numeric(33))
    }
    
    # return the utility
    return(centroid_util)
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
#' @param p_pred Numeric matrix with shape N x 2 containing predicted positions
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
#' \code{\link[predped]{utility}}
#' \code{\link[predped]{vf_utility_continuous}}
#' \code{\link[predped]{vf_utility_discrete}}
#' 
#' @rdname get_angles
#' 
#' @export 
#'
get_angles <- function (agent_idx, 
                        agent_group, 
                        position, 
                        orientation,  
                        p_pred, 
                        centers,
                        any_member = TRUE) {
    
    # First need to identify whether a pedestrian belongs to a social group
    inGroup <- agent_group[-agent_idx] == agent_group[agent_idx]
    p_pred <- p_pred[inGroup, , drop = FALSE]
    nped <- dim(p_pred)[1]
    
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
        rel_angles <- lapply(seq_len(nrow(p_pred)),
                         \(i) atan2(p_pred[i, 2] - centers[,2], p_pred[i, 1] - centers[,1]) - orientations) 
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
        nearest_ped <- m4ma::dist1_rcpp(position, p_pred)
        nearest_ped <- p_pred[which.min(nearest_ped),]
        orientations <- atan2(centers[,2] - positions[2], centers[,1] - position[1])

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

#' Continuous visual field utility
#' 
#' The idea of this utility function is to let the angle at which you see your 
#' group members play a role in the utility. Here, we distinguish between using 
#' a cosine for the maximization -- leading to maximum utility whenever an agent 
#' is directly looking at a group member and to minimum utility whenever the 
#' group member is directly behind the agent -- and the sine -- leading to 
#' maximum utility whenever the group members are directly besides the agent and 
#' to minimum utility whenever the agent is either directly behind the group 
#' member or the group member behind the agent.
#'
#' @param b_vf Numeric denoting the slope of the utility function. 
#' @param rel_angles Numeric vector containing the relative angle from each cell 
#' center to the predicted positions of the group members. Typically output of 
#' \code{\link[predped]{get_angle}}. 
#' @param fx Trigonometric function applied to the relative angles defining what
#' their effect is (scaled by \code{b_vf}). Defaults to \code{cos}, saying that 
#' the maximal utility stems from directly looking at a person (orientation 0).
#' Alternative could be \code{sin}, which maximizes the utility of looking at 
#' a person from right next to them (orientation 90).
#'
#' @return Numeric vector containing the utility attributed to keeping the 
#' group members within your visual field. Returns 0's if the agent does not 
#' have any additional group members.
#' 
#' @seealso 
#' \code{\link[predped]{get_angles}},
#' \code{\link[predped]{utility}},
#' \code{\link[predped]{vf_utility_discrete}}
#' 
#' @rdname vf_utility_continuous
#'
#' @export
vf_utility_continuous <- function(b_vf, 
                                  rel_angles, 
                                  fx = cos) {
     
     if (is.null(rel_angles)) {
        return(numeric(33))
    }

    # Calculate visual field utility
    visual_field_utility <- -sapply(rel_anlges, 
                                    \(x) b_vf * fx(x))

    # Return the visual field utility
    return(visual_field_utility)
}

#' Discrete visual field utility
#' 
#' The idea of this utility function is that it doesn't matter at which angle 
#' you see a group member within the visual field, as long as you see them. 
#' This translates to a discrete added disutility whenever the group member 
#' falls inside the non-visual zone behind the agent.
#'
#' @param b_vf Numeric denoting the slope of the utility function. 
#' @param rel_angles Numeric vector containing the relative angle from each cell 
#' center to the predicted positions of the group members. Typically output of 
#' \code{\link[predped]{get_angle}}. 
#'
#' @return Numeric vector containing the utility attributed to keeping the 
#' group members within your visual field. Returns 0's if the agent does not 
#' have any additional group members.
#' 
#' @seealso 
#' \code{\link[predped]{get_angles}},
#' \code{\link[predped]{utility}},
#' \code{\link[predped]{vf_utility_continuous}}
#' 
#' @rdname vf_utility_discrete
#'
#' @export
#'
vf_utility_discrete <- function(b_vf, 
                                rel_angles) {
     
     if (is.null(rel_angles)) {
        return(numeric(33))
    }

    # Calculate visual field utility
    # If in visual field: disutility = 0
    # If not in visual field: disutility = b*-1
    visual_field_angle <- 3*pi/4
    visual_field_utility <- -sapply(rel_angles, 
                                    \(x) ifelse(x > visual_field_angle & x < 2 * pi - visual_field_angle, b_vf * 1, 0))

    # Return the visual field utility
    return(visual_field_utility)
}

# #' Transform utility to probability
# #'
# #' Takes in the utility for each of the cells that an agent can decide on and
# #' transforms these utilities to probabilities. This quantifies the probability
# #' that an agent will move to a given cell in space.
# #'
# #' @param V Summed utility per candidate cell the agent might move to. Output of
# #' `utility`.
# #' @param muM Transformed nest assocation parameters that denote precision.
# #' Result of the `transform_mu` function. Defaults to `1` for each nest.
# #' @param nests Not clear yet
# #' @param alpha Not clear yet
# #'
# #' @export
# #
# # TO DO:
# #  - Nested functions in this function: Should we consider them separate?
# #  - The order of the arguments don't make sense to me: Would change them so
# #    that the defaults remain at the back
# #  - If I understand correctly, `between_nest` uses the individual probabilities
# #    of the `within_nest` function to compute the probabilities of the nests.
# #    If so, I would try to make this computation more general so that
# #    `within_nest` and `between_nest` are computed at once instead of twice
# #    in two separate functions
# pCNLs <- function(V,
#                   muM = rep(1, length(nests)),
#                   nests,
#                   alpha,
#                   mu = 1) {

#     # Create function to compute the probability of different alternatives
#     # within a nest
#     within_nest <- function(V, nests, alpha, muM) {
#         # Loop over the different nests to assess the probabilities of all
#         # alternatives within a single nest
#         for(i in seq_along(nests)) {
#             # Compute the probabilities
#             prob <- alpha[[i]] * exp(muM[i] * V[[i]])

#             # Check whether any of them are bad, and if so replace `prob` with
#             # either 1 or 0 depending on which ones are bad
#             if(any(prob == Inf)) {
#                 prob <- ifelse(prob == Inf, 1, 0)
#             }

#             # Scale the probabilities based on the total sum
#             if(all(prob == 0)) {
#                 nests[[i]] <- prob
#             } else {
#                 nests[[i]] <- prob / sum(prob)
#             }
#         }

#         return(nests)
#     }

#     # Create a function to compute the probability of the nests themselves
#     between_nest <- function(V, nests, alpha, mu, muM) {
#         # Transform the precision parameter
#         mu <- mu / muM

#         # Create probabilities for the different nests by summing the
#         # probabilities of all their alternatives
#         prob <- sapply(seq_along(nests),
#                        \(x) sum(alpha[[x]] * exp(muM[x] * V[[x]]))^mu_muM[x])

#         # Again check for any bad ones
#         if(any(prob) == Inf) {
#             prob <- ifelse(prob == Inf, 1, 0)
#         }

#         # Again normalize the probabilities
#         if(all(prob == 0)) {
#             prob <- prob
#         } else {
#             prob <- prob / sum(prob)
#         }

#         return(prob)
#     }

#     # Nest probabilities
#     if(any(unlist(nests) == 0)) {
#         nests <- lapply(nests, \(x) x + 1)
#     }

#     # Save all utilities of the different nests in a list
#     Vlist <- lapply(nests, \(x) V[x])

#     # Set largest V to zero to avoid numerical issues and apply this reduction
#     # to all other V's
#     upper_bound <- lapply(Vlist, max) |>
#         unlist() |>
#         max()

#     Vlist <- lapply(Vlist, \(x) x - upper_bound)

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
