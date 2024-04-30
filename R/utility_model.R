################################################################################
# HIGH-LEVEL UTILITY FUNCTIONS

#' Utility function
#'
#' Compute the utilities associated to several movements to be made, based on
#' the parameters of an agent.
#'
#' @param agent The agent for whom to compute the utilities
#' @param state A list with the current state of the simulation
#' @param P_n The goal to move to next
#' @param agent_predictions The predicted trajectories of the other agents
#' @param centers The centers of all the positions the agent may move to
#' @param check Boolean denoting the centers the agent can actually move to
#' (which are not blocked)
#' @param iInfo Not clear yet
#' @param precomputed Boolean denoting whether the different parts that make up
#' the utilities have already been computed. Defaults to `FALSE`.
#' @param subject Boolean denoting whether the utilities are subject-based.
#' Defaults to `TRUE`.
#'
#' @return <type> with utilities
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
                    agent_specifications,
                    centers,
                    background,
                    check,
                    precomputed = FALSE,
                    subject = TRUE) {

    # If the different parts that will make up the utilities were not precomputed,
    # compute them here.
    #
    # Left subject-based and iteration-based relatively unchanged as we first
    # have to think about how we will save this information. Maybe just make it
    # an agent-characteristic?
    if(!precomputed) {
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
    }

    # Compute the utilities and sum them up
    p <- transform_exponentiate(parameters(agent))

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
        View(p)
        V <- V + m4ma::wbUtility_rcpp(p[["a_buddy"]], 
                                      p[["b_buddy"]], 
                                      buddies[["buddies"]], 
                                      buddies[["dists"]])
    }

    V_transformed <- c(-p[["stop_utility"]], V) / p[["randomness"]]

    return(V_transformed)
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
