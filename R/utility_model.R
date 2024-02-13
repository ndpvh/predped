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
#' @param iInfo
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
                    agent_predictions,
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
        agents_id <- sapply(state$agents, id)
        agents_position <- t(sapply(state$agents, position))
        # Required for utility helper functions
        row.names(agents_position) <- agents_id
        agents_size <- sapply(state$agents, size)
        agents_orientation <- sapply(state$agents, orientation)
        agents_speed <- sapply(state$agents, speed)
        agents_group <- sapply(state$agents, group)

        agent_idx <- match(id(agent), agents_id)

        # Preferred speed
        goal_distance <- m4ma::dist1_rcpp(position(agent), agent@current_goal)

        # Angle between agent and the goal
        direction_goal <- m4ma::destinationAngle_rcpp(orientation(agent), position(agent, return_matrix = TRUE),
                           agent@current_goal) / 90

        # Interpersonal distance between agent and other agents
        interpersonal_distance <- m4ma::predClose_rcpp(agent_idx, p1 = position(agent, return_matrix = TRUE), a1 = orientation(agent),
                    p2 = agents_position, r = agents_size, centers, agent_predictions,
                    objects = objects(background))

        # Predict which directions might lead to collisions in the future
        blocked_angle <- m4ma::blockedAngle_rcpp(
            position(agent, return_matrix = TRUE),
            orientation(agent),
            speed(agent),
            agent_predictions[-agent_idx, , drop = FALSE], # change into index of agent in p_pred!
            agents_size,
            objects(background)
        )

        # Follow the leader phenomenon
        leaders <- m4ma::getLeaders_rcpp(
            agent_idx,
            agents_position,
            agents_orientation,
            agents_speed,
            current_goal(agent),
            agents_group,
            centers,
            objects(background)
        )

        # Walking besides a buddy
        buddies <- m4ma::getBuddy_rcpp(
            agent_idx,
            agents_position,
            agents_speed,
            agents_group,
            agents_orientation,
            agent_predictions,
            centers,
            objects(background),
            FALSE
        )

    # Subject based
    } else if (subject) {
        check <- state$check
        goal_distance <- state$goal_distance[n]
        direction_goal <- state$direction_goal
        interpersonal_distance <- state$interpersonal_distance
        blocked_angle <- state$blocked_angle
        leaders <- state$leaders
        buddies <- state$buddies
    # Iteration based
    } else {
        check <- state$check[[n]]
        goal_distance <- state$goal_distance[n]
        direction_goal <- state$direction_goal[[n]]
        interpersonal_distance <- state$interpersonal_distance[[n]]
        blocked_angle <- state$blocked_angle[[n]]
        leaders <- state$leaders[[n]]
        buddies <- state$buddies[[n]]
    }

    # Compute the utilities and sum them up
    p <- transform_exponentiate(parameters(agent))

    V <- numeric(nrow(centers))

    # Always check if utility data is NULL
    if (!is.null(goal_distance)) {
        V <- V + m4ma::psUtility_rcpp(p["aPS"], p["bPS"], p["sPref"], p["sSlow"], speed(agent), goal_distance)
    }

    if (!is.null(direction_goal)) {
        V <- V + m4ma::gaUtility_rcpp(p["bGA"], p["aGA"], direction_goal)
    }

    V <- V + m4ma::caUtility_rcpp(p["aCA"], p["bCA"], p["bCAlr"])
    
    if (!is.null(interpersonal_distance)) {
        V <- V + m4ma::idUtility_rcpp(p["bID"], p["dID"], p["aID"], agent_idx - 1, check, agents_group, interpersonal_distance)
    }

    if (!is.null(blocked_angle)) {
        V <- V + m4ma::baUtility_rcpp(p["aBA"], p["bBA"], blocked_angle, as.integer(names(blocked_angle))-1)
    }

    if (!is.null(leaders)) {
        V <- V + m4ma::flUtility_rcpp(p["aFL"], p["bFL"], p["dFL"], leaders[["leaders"]], leaders[["dists"]])
    }

    if (!is.null(buddies)) {
        V <- V + m4ma::wbUtility_rcpp(p["aWB"], p["bWB"], buddies[["buddies"]], buddies[["dists"]])
    }

    # Stop baseline (set by gwUtility) and scaling
    V_transformed <- c(-p["bS"], V) / p["rU"]

    return(V_transformed)
}

#' Transform utility to probability
#'
#' Takes in the utility for each of the cells that an agent can decide on and
#' transforms these utilities to probabilities. This quantifies the probability
#' that an agent will move to a given cell in space.
#'
#' @param V Summed utility per candidate cell the agent might move to. Output of
#' `utility`.
#' @param muM Transformed nest assocation parameters that denote precision.
#' Result of the `transform_mu` function. Defaults to `1` for each nest.
#' @param nests
#' @param alpha
#'
#' @export
#
# TO DO:
#  - Nested functions in this function: Should we consider them separate?
#  - The order of the arguments don't make sense to me: Would change them so
#    that the defaults remain at the back
#  - If I understand correctly, `between_nest` uses the individual probabilities
#    of the `within_nest` function to compute the probabilities of the nests.
#    If so, I would try to make this computation more general so that
#    `within_nest` and `between_nest` are computed at once instead of twice
#    in two separate functions
pCNLs <- function(V,
                  muM = rep(1, length(nests)),
                  nests,
                  alpha,
                  mu = 1) {

    # Create function to compute the probability of different alternatives
    # within a nest
    within_nest <- function(V, nests, alpha, muM) {
        # Loop over the different nests to assess the probabilities of all
        # alternatives within a single nest
        for(i in seq_along(nests)) {
            # Compute the probabilities
            prob <- alpha[[i]] * exp(muM[i] * V[[i]])

            # Check whether any of them are bad, and if so replace `prob` with
            # either 1 or 0 depending on which ones are bad
            if(any(prob == Inf)) {
                prob <- ifelse(prob == Inf, 1, 0)
            }

            # Scale the probabilities based on the total sum
            if(all(prob == 0)) {
                nests[[i]] <- prob
            } else {
                nests[[i]] <- prob / sum(prob)
            }
        }

        return(nests)
    }

    # Create a function to compute the probability of the nests themselves
    between_nest <- function(V, nests, alpha, mu, muM) {
        # Transform the precision parameter
        mu <- mu / muM

        # Create probabilities for the different nests by summing the
        # probabilities of all their alternatives
        prob <- sapply(seq_along(nests),
                       \(x) sum(alpha[[x]] * exp(muM[x] * V[[x]]))^mu_muM[x])

        # Again check for any bad ones
        if(any(prob) == Inf) {
            prob <- ifelse(prob == Inf, 1, 0)
        }

        # Again normalize the probabilities
        if(all(prob == 0)) {
            prob <- prob
        } else {
            prob <- prob / sum(prob)
        }

        return(prob)
    }

    # Nest probabilities
    if(any(unlist(nests) == 0)) {
        nests <- lapply(nests, \(x) x + 1)
    }

    # Save all utilities of the different nests in a list
    Vlist <- lapply(nests, \(x) V[x])

    # Set largest V to zero to avoid numerical issues and apply this reduction
    # to all other V's
    upper_bound <- lapply(Vlist, max) |>
        unlist() |>
        max()

    Vlist <- lapply(Vlist, \(x) x - upper_bound)

    # Compute both kinds of probabilities and multiply to get a total
    # probability for each of the alternatives
    between <- between_nests(Vlist, nests, alpha, mu, muM)
    within <- within_nests(Vlist, nests, alpha, muM)

    for(i in seq_along(nests)) {
        within[[i]] <- within[[i]] * between[[i]]
    }

    # Sum the different probabilities for a single alternative
    P <- V
    idx <- unlist(nests)
    within <- unlist(within)
    for(i in seq_along(within)) {
        P[i] <- sum(within[idx == i])
    }

    return(P)
}





################################################################################
# SINGLE UTILITY FUNCTIONS

# Computes the preferred speed utility for all cells of the agent, using the
# computed `goal_distance` to guide their preferences. If the goal is closer than
# "sSlow" seconds away, slow your speed down linearly.
#
# The repeat of 11 times is there because preferred_speed only affects the inner,
# middle, and outer ring of deceleration, constant speed, and acceleration.
#
# TO DO:
#   - It is currently unclear to me why you would multiply with 1.5, but divide
#     by 2: Should acceleration and deceleration be opposite of each other?
#     Kept in for now, but should be considered later on
#   - Make more general so that different cell specifications can be used as
#     well (instead of the now defined 33 options)
utility_preferred_speed <- function(agent,
                                    goal_distance) {

    # Determine whether one can keep their preferred speed, or whether they
    # should slow down as their goal is nearby
    slowing_factor <- goal_distance / (agent@speed * agent@parameters["sSlow"])
    slowed_speed <- agent@parameters["sPref"] * slowing_factor

    preferred_speed <- pmin(agent@parameters["sPref"], slowed_speed)

    # Change the weight on the utility of speeding up, slowing down, or
    # staying the same speed (if closer to goal, then you automatically
    # slow down to some extent: slowing down then means slowing down
    # even more than you initially thought you would)
    #
    # This works in an attractor kind of way, with preferred speed
    # working like an attractor and aPS being the power the attractor
    # is taken to (at this moment taken to be 2; simple linear model)
    V <- c(preferred_speed - agent@speed * 1.5,
           preferred_speed - agent@speed,
           preferred_speed - agent@speed * 0.5) |>
        abs() |>
        rep(each = 11)

    return(-agent@parameters["bPS"] * V^(agent@parameters["aPS"]))
}

# Computes the utility related to the direction of the goal. Importantly, the
# repeat of 3 times is there because goal_direction only affects the direction to
# choose.
#
# TO DO:
#   - Andrew asks the following question with regard to this utility:
#     No attractor? Or is GA this difference already?
utility_goal_direction <- function(agent,
                                   goal_direction) {

    V <- -agent@parameters["bGA"] * goal_direction^agent@parameters["aGA"] |>
        rep(times = 3)

    return(V)
}

# Compute the utility related to keeping your current direction and having a
# preferred side to move to. Note that there is again repetition done for each
# of the rings, as this is again a cone-utility.
utility_current_direction <- function(agent,
                                      angles = c(10, 20, 32.5, 50, 72.5) / 90) {

    # Transform some of the parameters
    product_b <- agent@parameters["bCA"] * agent@parameters["bCAlr"] |>
        rep(length(angles))
    division_b <- agent@parameters["bCA"] / agent@parameters["bCAlr"] |>
        rep(length(angles))

    # Transform the angles according to the power-parameter
    powered_angles <- angles^agent@parameters["aCA"]

    # Create the parameters for each of the different cones (11 in total).
    # First done for the beta's, then done for the powered angles
    beta <- c(product_b, 1, division_b)
    powered_angles <- c(rev(power_angles), 0, power_angles)

    # Compute the utility itself
    V <- (-1) * (beta * power_angles) |>
        rep(times = 3)

    return(V)
}

# Compute the inter-personal distance utility.
#
# TO DO
#   - Right now, utility computed only if bID != 0. However, it seems to me that
#     this if-statement should not be there in the first place, as the utility
#     will be 0 in this case anyway. Is this to make computations a bit faster?
utility_interpersonal_distance <- function(agent,
                                           interpersonal_distance,
                                           check) {

    # If noone is in front of the `agent`, return -Inf for all cells that are
    # blocked by objects. This if-statement furthermore checks if any of the
    # cells is a good alternative (`!any(ok)`)
    if(is.null(interpersonal_distance) | !any(check)) {
        V <- ifelse(check, 0, -Inf) |>
            as.vector()

        return(V)
    }

    # Create the beta's in such a way that they are group-dependent: If other
    # agents are close and in the ingroup, they receive "bID", otherwise the
    # beta becomes bigger by a value of dID
    beta <- ifelse(dimnames(interpersonal_distance)[[1]] == "ingroup",
                   agent@parameters["bID"],
                   agent@parameters["bID"] + agent@parameters["dID"])

    # Also take the interpersonal distances to a power `"aID"`
    powered_distance <- interpersonal_distance[,, drop = FALSE]^agent@parameters["aID"]

    # Adjust the check to enhance it with blocked objects (`check`) and with
    # no collistions with the other agents (`all(x > 0)`).
    check <- check & apply(interpersonal_distance, 2, \(x) all(x > 0))

    # Define the utility of the interpersonal distance as being -Inf if you are
    # about to collide with someone or something, and as being a function of the
    # interpersonal distance otherwise
    V <- ifelse(check, 0, -Inf)

    if(agent@parameters["bID"] != 0) {
        ID_per_agent <- beta / powered_distance

        V[check] <- (-1) * apply(ID_per_agent, 2, sum)
    }

    return(as.vector(V))
}

# Compute the utility associated to blocked directions, making those directions
# that may lead to blocking later on less attractive.
utility_blocked_angle <- function(agent,
                                  blocked_angle) {

    V <- numeric(33)

    # First check whether any direction are blocked, because if not, then we
    # should not change the utility based on this notion
    if(is.null(blocked_angle)) {
        return(V)
    }

    # Adjust the utility of each cell based on the blocking of an angle, using
    # the agent's parameters
    idx <- names(blocked_angle) |>
        as.numeric()

    V[idx] <- (-1) * agent@parameters["bBA"] / (pmax(blocked_angle, 0)^agent@parameters["aBA"])

    return(V)
}

# Compute the utility associated to following a leader that is going in a similar
# direction
#
# TO DO:
#   - In this one and `utility_buddy`, it is not clear yet whether the summation
#     is done per leader or per cell: Seems logical to have it per cell, so that
#     utility V is the same output everywhere. Currently, however, it seems to
#     take the sum per leader
utility_follow_leader <- function(agent,
                                  leaders) {

    # If there are no leaders, then we cannot use this utility to differentiate
    # between the different options
    if(is.null(leaders)) {
        return(numeric(33))
    }

    # Compute a beta-parameter based on the characteristics of the leaders:
    # Whether they are of the ingroup and whether they are heading in the same
    # direction
    beta <- agent@parameters["bFL"] + agent@parameters["dFL"] * leaders$leaders["inGroup",]
    beta <- beta * leaders$leaders["angleDisagree",]

    # Use this parameter to compute the utility for each of the different leaders
    # and sum the utilities per cell
    V <- sapply(seq_along(beta),
                \(x) beta[x] * leaders$dists[x,]^agent@parameters["aFL"]) |>
        apply(1, sum)

    return((-1) * V)
}

# Compute the utility associated to walking besides a buddy
#
# TO DO:
#   - In this one and `utility_follow_leader`, it is not clear yet whether the
#     summation is done per buddy or per cell: Seems logical to have it per cell,
#     so that utility V is the same output everywhere. Currently, however, it
#     seems to take the sum per leader
utility_buddy <- function(agent,
                          buddies) {

    # If there are no buddies, they cannot influence your next step
    if(is.null(buddies)) {
        return(numeric(33))
    }

    # Transform the angles to already account for the parameter "bWB"
    beta <- agent@parameters["bWB"] * buddies$buddies["angleDisagree"]

    # Transform the distances to already account for the power
    powered_distance <- buddies$dists^agent@parameters["aWB"]

    # Use this parameter to compute the utility for each of the different leaders
    # and sum the utilities per cell
    V <- sapply(seq_len(nrow(buddies$dists)),
                \(x) beta[x] * powered_distance[x,]) |>
        apply(1, sum)

    return((-1) * V)
}





################################################################################
# HELPER FUNCTIONS OF THE UTILITY FUNCTIONS

#' Compute the difference between direction of pedestrian and goal
#'
#' Computes the absolute angular difference between the different directions given
#' by `angle` (0 being the current direction of the agent) and the direction of
#' the goal the agent is walking towards.
#'
#' @param agent The agent of concern
#' @param angles The different angles that should be considered in the utility
#' function
#'
#' @return <NA>
#'
#' @export
#
# Original function was `destinationAngle`
goal_direction <- function(agent,
                           angles = c(72.5, 50, 32.5, 20, 10, 0, 350, 340,
                                      327.5, 310, 287.5)) {

    sapply((angles + a) %% 360, minAngle, a2 = angle2(agent@position, agent@current_goal@position))
}

# Matrix of predicted distance between edge of bodies from ped n to others in
# front,
# p1 = state$p[n, , drop = FALSE]; a1 = state$a[n]; r = state$r

#' Compute distance between agent and others
#'
#' Computes the predicted distance between an agent and the other agents that
#' are walking around.
#'
#' @param agent The agent of concern
#' @param state List with the current state of the simulation
#' @param agent_predictions The predicted trajectories of the other agents
#' @param centers The centers of all the positions the agent may move to
#'
#' @return Matrix with each column is a cell each row an in front ped, if none
#' returns null
#'
#' @export
#
# TO DO
#  - Make sure you don't need to separate agent from object in obstruction
#    functions like `sees_(...)`. Instead, introduce a boolean that makes the
#    difference between `obstructs_view` or not (e.g., agents may be able to
#    see their goal when a cart or agent is in their way, but not when a
#    book case is in the way)
#  - Define how `agent_predictions` are going to look like: Are they a part of
#    agent, or are they something else entirely
#  - Optimize how the distances are created. At this moment inefficient (due
#    to our own design and uncertainty around how to handle `agent_predictions`
#    and `centers` [list of coordinates or matrix; here matrix assumed])
#
# Original function was `predClose`
others_close <- function(agent,
                         state,
                         agent_predictions,
                         centers) {

    # If there are no other agents to be concerned about, return NULL
    if (dim(agent_predictions)[1] == 1) {
        return(NULL)
    }

    # Separate objects from agents, as is required for the underlying functions
    # to be run. Consider for example `sees_goal`, which assumes that agents can
    # still see their goal even if it is obstructed by another agent, but not by
    # an object.
    separated_objects <- separate_agent_object(state)
    other_agents <- separated_objects[["agents"]]
    objects <- separated_objects[["objects"]]

    # Remove the current agent from the `other_agents` list as well as the
    # `agent_predictions`
    agent_predictions[[other_agents == agent]] <- NULL
    other_agents[[other_agents == agent]] <- NULL

    # Check which of these agents is occluded, and can thus not be seen by `agent`
    # and remove them from the `other_agent` list as well as the `agent_prediction`
    # list
    idx <- sees_multiple_locations(agent, other_agents, objects)
    agent_predictions[[!idx]] <- NULL
    other_agents[[!idx]] <- NULL

    # Check whether there are no other agents to be concerned about and return
    # NULL if this is the case
    if(length(other_agents) <= 1) {
        return(NULL)
    }

    # Finally, check which other agents will remain in the field of vision after
    # they have moved
    remains_visible <- lapply(agent_predictions,
                              function(x) {
                                goal_angle <- angle2(agent@position, agent@current_goal@position)
                                agent_angle <- angle2(agent@position, x@position)

                                return((minAngle(agent@angle, goal_angle) < 85) &
                                       (minAngle(agent@angle, agent_angle) < 85))
                              })

    # If no one will remain visible according to the agent's predictions, return
    # NULL. Otherwise delete these people from the lists again
    if (!any(remains_visible)) {
        return(NULL)
    }
    agent_predictions[[!remains_visible]] <- NULL
    other_agents[[!remains_visible]] <- NULL

    # Compute the interpersonal distance between the agent and the positions that
    # have been predicted for the other agents in the room. Done by first checking
    # the distance between the center positions the agent might move to and the
    # predicted distance of the other agents to those cells, and then subtracting
    # the body size of the other agents from these distances.
    distance_to_others <- matrix(0, nrow = length(agent_predictions), ncol = nrow(centers))
    for(i in seq_along(agent_predictions)) {
        for(j in seq_len(nrow(centers))){
            d <- dist1(centers[j,], agent_predictions[[i]]@position)
            distance_to_others[i,j] <- d - agent@size - other_agents[[i]]@size
        }
    }

    # Before moving on, we have to define whether people are from the ingroup
    # or outgroup (see `utility_interpersonal_distance`). So make this comparison
    # and change the dimension-names of the matrix to account for this.
    ingroup <- lapply(other_agents,
                      \(x) ifelse(x@group == agent@group, "ingroup", "outgroup")) |>
        as.character()
    rownames(distance_to_others) <- ingroup

    return(distance_to_others)
}

#' Find angles that lead to a blocked path
#'
#' Computes the distance from each cell the agent might move to to the closest
#' agent that falls within a cone in that given direction.
#'
#' @param agent The agent of concern
#' @param state List with the current state of the simulation
#' @param agent_predictions The predicted trajectories of the other agents
#' @param velocity Vector of changes in the velocity that the agent may take.
#' Defaults to slowing down (0.5), constant speed (1), or accelerating (1.5).
#'
#' @returns Cell-named vector of distances
#'
#' @export
#
# TO DO
#  - Find a way to make this a bit more streamlined, as we again make a
#    distinction between objects and agents within this function, as we
#    do in many of these individual functions (do at a higher level, rewrite
#    these functions to get an argument `other_agents` and `objects`)
#  - Some things are hardcoded (everything related to the centers) and
#    should be changed to allow for users to determine these things for
#    themselves
#
# Original function `blockedAngle` and `iCones2Cells`
predict_blocking <- function(agent,
                             state,
                             agent_predictions,
                             velocity = c(1.5, 1, 0.5)) {

    # Separate objects from agents, as is required for the underlying functions
    # to be run (specifically, `sees_multiple_locations`).
    separated_objects <- separate_agent_object(state)
    objects <- separated_objects[["objects"]]

    # Get the distances to the closest agent within a given cone starting from the
    # agent of concern
    cones <- intersecting_cones(agent, agent_predictions, objects)

    # Scale the velocity of the `agent` and determine how fast they may go
    # according to the different options
    speeds <- scale_velocity(agent@speed) * velocity

    # Subtract distances from these velocities, allowing us to know which
    # direction they may go without colliding with the predicted paths of
    # other agents
    output <- rep(cones, times = length(velocity)) - rep(speeds, each = length(cones))

    # Add a tag that determines which center these distances belong to for
    # later processing.
    #
    # Needs to change to make it more general!
    names(output) <- rep(names(cones), times = length(velocity)) +
        rep(c(0, 11, 22), each = length(cones))
    return(output)
}

# Intersecting cones for closest p2 pedestrian profiles (circles of radius r) from
# the perspective of p1 (calculated by eObject) where the point on the profile
# at the cone midline can be seen (i.e., not occluded by objects) given heading
# angle a.
# Output NULL or cone number named vector of distances from p1 to point of intersection.
# Now returns NULL when just one pedestrian inside
# p1 = state$p[n, , drop = FALSE]; p2 = p_pred[-n, , drop = FALSE];
# a = state$a[n]; r = state$r
#
#' Intersecting cones for two agents
#'
#' Compute the intersecting cones of vision for the agent of concern `agent` and
#' the closest other agent that can be seen.
#'
#' @param agent The agent of concern
#' @param agent_predictions The predictions of where the other agents will be
#' @param objects The objects that may block the view
#'
#' @return `NULL` if there are no cones that intersect.
#'
#' @export
#
# TO DO
#  - A bug has been reported for this function: "should pass actual position
#    not p_pred to evaluate SEEN"
#  - Rewrite functions for utility so that a lot of the computations only need
#    to be done once: here you again need to differentiate agents and objects,
#    which you already did in `others_close`
#  - Another part of efficiency can be having only the centers that can be
#    evaluated provided by the agent. This way, you only have to compute potential
#    blocking once instead of multiple times
#  - And yet another one: Useful to save the distance of an agent to an object
#    as well, as this is computed here
#  - Given that this function depends heavily on the previous structure of the
#    code, it is important to do an integration test here
#
# Original function: `iCones`
intersecting_cones <- function(agent,
                               agent_predictions,
                               objects) {

    # First check whether there are any predictions to take care of: If there
    # are no agents, then we shouldn't worry about them either
    if(length(agent_predictions) == 0) {
        return(NULL)
    }

    # Compute the candidate cones that might intersect with the agent of
    # interest and return `NULL` if there appear to be no candidates
    candidates <- candidate_cones(agent, agent_predictions)
    cone_list <- candidates[["cone_list"]]
    ends <- candidates[["ends"]]

    if(length(cone_list) == 0) {
        return(NULL)
    }

    # End of unit line from the position of the agent in the direction of each
    # cone (i.e., in each possible new orientation that they might move to)
    cone_line_ends <- compute_cell_centers(1:11,
                                           agent@position,
                                           rep(1, 11),
                                           agent@orientation)

    # Compute the distances from the agent to the objects that are contained
    # within the cones of range
    cone_distances <- vector(mode = "list", length = length(cone_list))
    for(i in seq_along(cone_list)) {
        # Dispatch on whether the other agents fall within one cone or more than
        # one
        if(length(cone_list[[i]]) == 1) {
            # Check whether the other agent is visible from the `agent`'s
            # position
            visible <- sees_location(agent@position,
                                     agent_predictions[[i]]@position,
                                     objects)

            # If the agent cannot see the other, remove them from the list.
            # Otherwise compute the distance between the two points
            if(!visible) {
                cone_list[[i]] <- numeric(0) # Can this not just become NA? Or NULL? Especially given that they are deleted later on
            } else {
                cone_distances[[i]] <- dist1(agent@position,
                                             agent_predictions@position)
            }
        } else {
            N <- length(cone_list[[i]])

            # If there are multiple cones to check, we should loop over them
            for(j in seq_along(cone_list[[i]])) {
                # Compute the intersection between the cone line and the end points
                # we previously computed
                intersection <- line.line.intersection(agent@position,
                                                       cone_line_ends[cone_list[[i]][j], ],
                                                       ends[i,,1],
                                                       ends[i,,2]) |>
                    matrix(nrow = 1)

                # Check whether the other agent is visible from the `agent`'s
                # position
                visible <- sees_location(agent@position,
                                         intersection,
                                         objects)

                # If the agent cannot see the other, remove them from the list.
                # Otherwise compute the distance between the two points
                if(!visible) {
                    cone_list[[i]][j] <- NA
                } else {
                    distance <- dist1(agent@position,
                                      intersection)
                    if(j == 1) {
                        cone_distances[[i]] <- distance
                    } else {
                        cone_distances[[i]] <- c(cone_distances[[i]],
                                                 distance)
                    }
                }
            }

            # Delete all the NA's from the cone_list
            cone_list[[i]] <- cone_list[[i]][!is.na(cone_list[[i]])]
        }
    }

    # Delete those cases for which there are no distances (i.e., those that
    # could not be seen). Then check whether any remain
    idx <- lapply(cone_list,
                  \(x) (length(x) == 0))
    cone_list[[idx]] <- NULL

    if(length(cone_list) == 0) {
        return(NULL)
    }

    # Delete those distances that returned NULL
    idx <- lapply(cone_distances, is.null)
    cone_distances[[idx]] <- NULL

    # Add the cone to which a given distance belongs
    for(i in seq_along(cone_list)) {
        names(cone_distances[[i]]) <- cone_list[[i]]
    }

    # Get the minimal distance to another agent that falls within each of the
    # candidate cones
    output <- output_names <- rep(NA, 11)
    for(i in seq_len(11)) {
        # Get all distances related to a given cone number
        distance <- lapply(cone_distances,
                           \(x) x[names(x) == i]) |>
            unlist()

        # Add the minimal distance to the output of this function if they
        # exist. Furthermore keep a tab of which cone it is related to
        if(length(distance) > 0) {
            output[i] <- min(distance)
            output_names[i] <- i
        }
    }

    # Delete all NA's from the output vectors and add information on which
    # distance belongs to which cone
    output <- output[!is.na(output)]
    output_names <- output_names[!is.na(output_names)]
    names(output) <- output_names

    return(output)
}

#' Get end points of line segment
#'
#' Compute the end points of the line segments that are drawn at a right angle
#' from a given starting position to an end position. Was given the name
#' egocentric objects in a previous version of the code.
#'
#' @param agent The agent of concern
#' @param other_agents The other agents walking around
#'
#' @return A 3-dimensional array with dimensions number of agents x coordinates
#' (2) x end of the segments. Importantly, "ac" and "cw" denote anticlockwise
#' and clockwise respectively.
#'
#' @export
#
# Original function `eObjects`
egocentric_objects <- function(agent, other_agents) {
    # Compute the distances between the position of `agent` and all the other
    # agents
    distances <- lapply(other_agents,
                        \(x) dist(agent@position, x@position))

    # Compute the anti-clockwise angle with the positions of `agent` as the
    # origin to the positions of the other agents. Originally, the `angle2`
    # function was used for this, but this functione expects positions to be
    # matrices. To not fumble to much with the code yet, therefore local function
    # created that can than be applied to the `other_agents` list
    local_angle <- function(position_1, position_2) {
        # Compute angle in radians
        angle <- atan2(position_2[2] - position_1[2],
                       position_2[1] - position_1[1])

        # Convert to degrees
        angle <- angle * 180 / pi

        # Round to the nearest ten and scale back to a number between 0 and 360
        return(round(angle) %% 360)
    }
    angles <- lapply(other_agents,
                     \(x) local_angle(agent@position, x@position))

    # Compute clockwise and anticlockwise points starting at the origin
    end_points <- array(dim = c(length(other_agents, 2, 2)))
    for(i in seq_along(other_agents)) {
        # Compute the arctan of the distance to an agent divided by their radians.
        # Not entirely clear to me what it is that this computation does, but no
        # time to delve into it for too long either.
        theta <- atan(other_agents[[i]]@radius / distances[i]) * 180 / pi

        # Compute the end points and put them in the array (first anti-clockwise,
        # then clockwise)
        end_points[i,,1] <- angle_to_L2((angles[i] + theta) %% 360) * distances[i]
        end_points[i,,2] <- angle_to_L2((angles[i] - theta) %% 360) * distances[i]
        end_points[i,,] <- end_points + agent@position
    }

    # Normally, this array should receive some names, but we have to see whether
    # we still want to do this. For now, I just return the array as is (see
    # pp_utility_extra.R lines 72-83)
    return(end_points)
}

# Compute the candidate cones that the agent might intersect with on their path.
# Was originally a part of the function `iCones`
candidate_cones <- function(agent, agent_predictions) {
    # One end in, one out, fill in extreme cone for out
    fix <- function(x) {
        if (all(is.na(x))) {
            return(NA)
        }
        if (all(is.na(x) == c(FALSE, TRUE))) {
            x <- c(x[1],11)  # cw out
        }
        if (all(is.na(x) == c(TRUE, FALSE))) {
            x <- c(1,x[2])  # ac out
        }
        if (length(x) > 1) {
            x <- x[1]:x[2]  # fill in intervening
        }
        return(x)
    }

    # Compute an array of end points of the line segments between the current
    # agent and where he predicts the other agents might end up
    ends <- egocentric_objects(agent, agent_predictions)

    # Get the number of the cone (at a given angle) that corresponds to the
    # position provided starting from the position of `agent` at the angle they
    # are heading
    end_cones <- apply(ends,
                       3,
                       \(x) Iangle(agent@position, agent@orientation, x))

    # If there is only one other agent to consider, the `endCones` should be
    # rescaled
    if(nrow(ends) == 1) {
        end_cones <- matrix(end_cones, nrow = 1)
    }

    # Put the results in a list and fix the entries of this list with the
    # predefined `fix` function
    cone_list <- vector(mode = "list", length = nrow(end_cones))
    for(i in seq_along(cone_list)) {
        cone_list[[i]] <- fix(end_cones[i,])
    }

    # Delete all entries in the list that are missing or NULL
    idx <- lapply(cone_list,
                  \(x) !any(is.na(x)))
    cone_list[[unlist(idx)]] <- NULL

    idx <- lapply(cone_list,
                  \(x) !is.null(x))
    cone_list[[unlist(idx)]] <- NULL

    return(list("cone_list" = cone_list, "ends" = ends))
}

#' Get potential leaders to follow
#'
#' This function finds directions in which potential "leaders" can be found,
#' which the agent can then follow.
#'
#' @param agent The agent of concern
#' @param state List with the current state of the simulation
#' @param centers The centers of all the positions the agent may move to
#' @param only_group Boolean denoting whether only people from the ingroup
#' should be considered as leaders. Defaults to `FALSE`.
#' @param prefer_group Boolean denoting whether to prefer people from the ingroup
#' to be a leader. Defaults to `TRUE`
#' @param pick_best Boolean denoting whether to pick the leader that follows your
#' own objective most closely (i.e., is heading in a similar direction)
#'
#' @return List containing the distances between the agent and the potential
#' leaders (`"dists"`) and information on the leaders themselves (`"leaders"`),
#' that is information on the cells they occupy, the angle of disagreement between
#' their direction and the orientation of the agent, and whether they are a part
#' of the ingroup.
#'
#' @export
#
# TO DO
#  - Create a function that makes a named list of centers, having a matrix in
#    that denotes the velocities, angles, positions, and blocking. In the
#    blocking matrix, we can then include the type of blocking: "free", "agent",
#    "object", "moveable object". That way, we can just compute the centers
#    variable once and make inference based on this one-time computation
#  - Streamline again, once finding out whether any agents are closeby so that
#    they may serve as leaders (as done in `predict_blocking`)
#  - At some point, the velocity is scaled for the agent. However, one of the
#    numbers in this scaling is 5 instead of 2 (to create the outer ring):
#    check whether this is correct or whether this is a bug
#  - If the output list is retained, make the contents of this list more
#    explicit than they are right now
#
# Original function: `getLeaders`
get_leaders <- function(agent,
                        state,
                        centers,
                        only_group = FALSE,
                        prefer_group = TRUE,
                        pick_best = FALSE) {

    # Separate objects from agents, as is required for the underlying functions
    # to be run (specifically, `sees_multiple_locations`).
    separated_objects <- separate_agent_object(state)
    other_agents <- separated_objects[["agents"]]
    objects <- separated_objects[["objects"]]

    # Remove all the `other_agents` that the `agent` cannot see
    idx <- sees_multiple_locations(agent, other_agents, objects)
    other_agents[[!idx]] <- NULL

    # If no pedestrians remain, return `NULL`
    if(length(other_agents) == 0) {
        return(NULL)
    }

    # Get the goal of the agent. This is what I believe happens under the hood,
    # but might be worthwhile to check out in pp_utility_extra.R lines 254-258
    goal <- agent@current_goal

    # Check in which cones other agents are walking around when starting from
    # the position of `agent`
    cones <- lapply(other_agents,
                    \(x) Iangle(agent@position, agent@orientation, x@position))

    # If there are none, then we should just return `NULL`
    check <- is.na(cones)
    if(length(cones) == sum(check)) {
        return(NULL)
    }

    # Create a list of candidate leaders and delete the `NA` cones
    candidate_leaders <- other_agents
    candidate_leaders[[!check]] <- NULL
    cones <- cones[!check]

    # Compute the distances from the `agent` to the candidate leaders
    distances <- lapply(candidate_leaders,
                        \(x) dist(agent@position, x@position))

    # Given these distances, subset the inner, middle, and upper ring created
    # by the given velocities of the agent and give the different rings labels
    ring <- cut(distances,
                scale_velocity(agent@speed) * c(0, 0.5, 1, 5),  # Check whether this is correct: Shouldn't 5 be 2?
                as.character(3:1)) |>
        as.character() |>
        as.numeric()

    # Delete any `NA` rings and if there are none left, just return `NULL`
    ring <- ring[!is.na(ring)]
    if(length(ring) == 0) {
        return(NULL)
    }

    # Rework the cones a bit so that we may select between the candidates.
    # More specifically, create a vector of all centers that correspond to a
    # given direction or cone, allowing us to select which centers are more
    # of interest under the follow the leader principle.
    candidate_cells <- cones + 11 * (ring - 1)

    # Find out which candidate leaders belong to the same group as the `agent`
    ingroup <- lapply(candidate_leaders,
                      \(x) x$group == agent$group) |>
        as.logical()

    # Find out what to do based on the group to which the `agent` belongs.
    # If the agent only wants to follow their own ingroup, they will either not
    # follow at all (if noone is present) or select on the candidate leaders.
    # If not, then they might prefer to select on the candidate leaders but only
    # if they are currently present.
    if(only_group) {
        if(!any(ingroup)) {
            return(NULL)
        } else {
            candidate_cells <- candidate_cells[ingroup]
            candidate_leaders[[ingroup]] <- NULL
        }
    } else if(prefer_group & any(ingroup)) {
        candidate_cells <- candidate_cells[ingroup]
        candidate_leaders[[ingroup]] <- NULL
    }

    # Compute the difference in the angle the leader is heading towards and the
    # destination the `agent` should be heading towards
    angles <- lapply(candidate_leaders,
                     \(x) minAngle(agent@orientation, x@orientation))

    # Only select those candidates who show a difference in orientation of less
    # than 90 degrees
    idx <- angles < 90

    if(!any(idx)) {
        return(NULL)
    }

    angles <- angles[idx]
    ingroup <- ingroup[idx]
    candidate_cells <- candidate_cells[idx]
    candidate_leaders[[idx]] <- NULL

    # Define the cones that seem most reasonable to follow based on whether a
    # leader is present or not. If there are no duplicate cones, then this
    # just comes down to the current state of affairs. If there are duplicates,
    # then we should be able to differentiate between multiple leaders within
    # a same cone.
    #
    # Right now, I commented out this code because I believe this is a problem
    # that does not pose itself in the object-oriented way of dealing with this
    # if (!any(duplicated(candidates))) {
    #     leaders <- candidates
    # } else {
    #     leaders <- unique(candidates)
    #     for (i in 1:length(leaders)) {
    #         names(leaders)[i] <- names(candidates)[candidates == leaders[i]][
    #             which.min(angles[candidates == leaders[i]])]
    #     }
    #     angles <- angles[names(leaders)]
    # }

    # Compute the distances between each of the centers and the cells where the
    # leaders are currently residing
    distances <- array(dim = c(length(candidate_cells), 33),
                       dimnames = list(NA, 1:33))
    for(i in seq_along(candidate_cells)) {
        distances[i,] <- dist1(centers[candidate_cells[i],], centers)
    }

    # If you want to pick the best one, then define an index that lets you pick
    # the best one
    if(pick_best) {
        idx <- which.min(angles)
    } else {
        idx <- seq_along(angles)
    }

    # Create a list containing the output and return it
    output <- list("dists" = distances[best,,drop = FALSE],
                   "leaders" = rbind(cell = candidate_cells,
                                     angleDisagree = angles / 90,
                                     inGroup = ingroup[,best, drop = FALSE]))

    return(output)
}

#' Get potential buddy to walk beside
#'
#' This function finds directions in which potential "buddies" can be found,
#' which the agent can then walk with together.
#'
#' @param agent The agent of concern
#' @param state List with the current state of the simulation#'
#' @param agent_predictions The predictions of where the other agents will be
#' @param centers The centers of all the positions the agent may move to
#' @param pick_best Boolean denoting whether to pick the leader that follows your
#' own objective most closely (i.e., is heading in a similar direction)
#'
#' @return List containing the distances between the agent and the potential
#' buddies (`"dists"`) and information on the buddies themselves (`"buddy"`),
#' that is information on the cells they occupy and the angle of disagreement
#' between their direction and the orientation of the agent.
#'
#' @export
#
# TO DO
#  - This function is very similar to `get_leaders`, and might thus benefit
#    from the same TO DO's.
#  - A lot of hardcoding going on here: Streamline these functions at some point
#
# Original function: `getBuddy`
get_buddy <- function(agent,
                      state,
                      agent_predictions,
                      centers,
                      pick_best = FALSE) {

    # Separate objects from agents, as is required for the underlying functions
    # to be run (specifically, `sees_multiple_locations`).
    separated_objects <- separate_agent_object(state)
    other_agents <- separated_objects[["agents"]]
    objects <- separated_objects[["objects"]]

    # Remove all the `agent_predictions` that the `agent` cannot see
    idx <- sees_multiple_locations(agent, agent_predictions, objects)
    agent_predictions[[!idx]] <- NULL
    other_agents[[!idx]] <- NULL

    # If no pedestrians remain, return `NULL`
    if(length(agent_predictions) == 0) {
        return(NULL)
    }

    # Check the group membership of the agents that remain and select based on
    # whether the buddy is ingroup or outgroup
    #
    # Additional comment by Andrew that I am not understanding at the moment:
    #
    # NB: Not checking in front as assume you know where your group members are.
    #     If they are behind this formulation will tend to slow you down so they
    #     can catch up.
    ingroup <- lapply(other_agents,
                      \(x) x@group == agent@group) |>
        as.logical()
    agent_predictions[[!idx]] <- NULL
    other_agents[[!idx]] <- NULL

    if(length(agent_predictions) == 0) {
        return(NULL)
    }

    # Compute the angular differences between the different directions the
    # agent may take and the direction the buddies are heading. This is then
    # transformed to a matrix with all angular differences per potential direction
    # (cone) in the rows and the buddies in the columns
    differences <- lapply(other_agents,
                          \(x) angular_difference(agent@orientation,
                                                  x@orientation)) |>
        unlist() |>
        matrix(ncol = length(agent_predictions))

    # Compute which of the potential cones lies most in line with each of the
    # buddies (separately)
    closest_cones <- apply(differences, 2, which.min)

    # Compute the distances from the predicted buddy positions to the inner,
    # middle, and outer rings of movememt created by changes in the velocity of
    # the agent
    distances <- lapply(seq_along(agent_predictions),
                        \(x) dist1(agent_predictions[[x]]@position,
                                   centers[closest_cones + c(0, 11, 22),]))

    # From these distances, get the ring that is closest to the buddy, allowing
    # us to define the cell that comes closest to the buddy. For this, we create
    # a matrix of cell indices, of which we then select a given one based on
    # the value of `ring` for each of the potential buddies separately
    ring <- apply(distances, 2, which.min)

    idx <- cbind(seq_along(agent_predictions), ring)
    cell <- cbind(closest_cones,
                  closest_cones + 11,
                  closest_cones + 22)[idx]
    names(cell) <- names(closest_cones)

    # Select the angular difference for each buddy and the agent given the
    # closest_cones
    idx <- cbind(closest_cones, seq_along(closest_cones))
    angles <- differences[idx] / 90

    # Compute the distances to each of the buddy cells.
    #
    # Currently unclear to me why you would use `names(angles)`, as these don't
    # have any names (not in this reworked version nor in the original one)
    distances_cell <- array(dim = c(length(agent_predictions, 33),
                            dimnames = list(names(angles), 1:33)))
    for(i in seq_along(agent_predictions)) {
        distances_cell[i,] <- dist1(centers[cell[i],], centers)
    }

    # If you want to pick the best one, then choose the one that is closest to
    # where you're heading
    if(pick_best) {
        idx <- which.min(angles)
    } else {
        idx <- seq_along(agent_predictions)
    }

    return(list("dists" = distances_cell[idx,, drop = FALSE],
                "buddies" = rbind(cell, angles)[, idx, drop = FALSE]))
}

