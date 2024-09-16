#' Check whether an object is blocked
#'
#' When moving around, an agent or an object can get blocked by other agents or
#' state. This function checks whether this is the case.
#'
#' @param object The object for which blocking should be checked.
#' @param ... Additional arguments that can differ for agents or environmental
#' state.
#'
#' @return list
#' 
#' @docType methods
#' @rdname blocked-methods
#'
#' @export
setGeneric("moving_options", function(agent, ...){})

# Create the blocked_agent function.
#
# TO DO:
#   - Get rid of all these reverse logicals and try to find a way to make it
#     more comprehensible.
#   - Replace the `all` with `any`, as this is most likely what we
#     actually want.
#   - Reduce the number of `errored_out`: I think just changing `bad` at the end
#     should do about the same thing.
#   - Some parts of the code seem like they could be deleted, but are kept in:
#     Check these at a later time.
#   - Rewrite the `seesGoalOK` function, which is still in its original format.
#   - Change the state argument to contain all things that are in the
#     environment: agents and state! When starting to implement this, we can
#     change the `okBodyBody` and `overlap_with_object` functions, as these would
#     in effect do the same thing but more generalized (see TO DO in the latter
#     function)
#   - Put the error check in a test function, so that we can reproduce such
#     errors
#
# Replacement of `get_ok`, which was nested within `move` in `pp_simulate.R`
moving_options_agent <- function(agent, 
                                 state, 
                                 background, 
                                 centers){

    # Add the other agents to the background objects. This will allow us to 
    # immediately test whether cells are occupied by other agents instead of 
    # doing this check only later.
    objects(background) <- append(objects(background), 
                                  state$agents)

    # If there are any roads that the agents cannot walk into, check which of the 
    # roads the agent can and cannot walk into. Account for this in the background
    # objects
    if(length(background@limited_access) != 0) {
        # Get all objects that may limit the accessibility for the agents. 
        # Also instantiate another list that contains all of the instances in 
        # which this is indeed the case
        to_test <- limited_access(background)
        not_accessible <- list() ; f <- 1

        for(i in seq_len(length(to_test))) {
            # First check whether the agent intersects the segment of choice. 
            # If so, we need to delete it from the list anyway (we only want it 
            # to be non-walkthrough once they passed this bound already)
            if(intersects(to_test[[i]], agent)) {
                next
            }

            # Compute the relative angle of the agent compared to the start of 
            # the segment and subtract the orientation of the line.
            angle <- atan2(center(agent)[2] - from(to_test[[i]])[2],
                           center(agent)[1] - from(to_test[[i]])[1])
            angle <- angle - orientation(to_test[[i]])

            # Compute the sine of the angle and flag the segment as being non-
            # accessible if the sine is positive (i.e., when the angles are 
            # between 0 and pi)
            if(sin(angle) < pi & sin(angle) > 0) {
                # If this is indeed the case, we will create a very small 
                # polygon that cannot be passed through by the agent in all of 
                # the underlying functions. This polygon is then added to the 
                # list of not_accessible items
                coords <- points(to_test[[i]])

                alpha <- orientation(to_test[[i]]) + pi / 2
                R <- matrix(c(cos(alpha), sin(alpha), -sin(alpha), cos(alpha)), nrow = 2, ncol = 2)
                new_coords <- coords[2:1,] + rep(R %*% c(1e-2, 0), each = 2)

                not_accessible[[f]] <- polygon(points = rbind(coords, new_coords))
                f <- f + 1
            }
        }

        # Once done, we can add these polygons to the objects in the background
        objects(background) <- append(objects(background), 
                                      not_accessible)
    }

    # Use the `free_cells` function to get all free cells to which the agent
    # might move. Specifically look at whether a cell lies within the background
    # and whether the agent has a direct line of sight to that cell.
    check <- m4ma::free_cells_rcpp(agent, background, centers)

    # If there are still cells free, check whether an agent would intersect with
    # an object if it were to move to a given cell. Given that the function
    # `overlap_with_object` only checks those cells that are free, the output
    # of this function can overwrite the local `check` variable without any
    # issues
    if(!all(!check)){
        # check <- m4ma::bodyObjectOK_rcpp(size(agent), centers, objects(background), check) # Original
        check <- overlap_with_objects(agent, background, centers, check)

        # If something blocks the way in the previous column, then it should also 
        # block the way on the columns
        check[!check[,3],2] <- FALSE
        check[!check[,2],1] <- FALSE
    }

    # If there are still cells free, check whether the goal can still be seen
    # or whether an agent should re-plan
    if(!all(!check)){
        # Weird bug: running seesGoalOK_rcpp changes `check` or copy of `check`.
        # Keep copy of the opposite of `check` for later use
        opposite_check <- !check

        # Function to rewrite! New arguments are already provided to this one,
        # but not the original one
        # goal_position <- current_goal(agent)@position # This was the original one
        goal_position <- current_goal(agent)@path[1,]
        goal_list <- list(matrix(goal_position, ncol = 2))
        attr(goal_list[[1]], "i") <- 1
        state_dummy <- list(P = goal_list)
        local_check <- m4ma::seesGoalOK_rcpp(1, objects(background), state_dummy, centers, check)

        # Here, change `check` based on the results of the function. Importantly,
        # agent should still move even if it cannot see their goal (hence the
        # if-statement), otherwise the agent will get stuck
        check <- if(!all(!local_check)) local_check else !opposite_check
    }

    # Finally, return the cells that are free to move to
    return(check)
}

#' @rdname blocked-methods
#' @aliases blocked, agent
setMethod("moving_options",
          "agent",
          moving_options_agent)

# Number of pedestrians in the goal direction (goal cone or the cones on either
# side of it).
# returns number and if more than one attribute "ends" specifying blocking
# profiles from current perspective
#
# Deprecated?
#
# TO DO:
#   - The purpose of `cone_set` is not entirely clear and might have to change if 
#     we wish to consider more or less than 33 choices
#   - There might be an easier, object-oriented way of finding out how many agents
#     are blocking the goal
#
# Original function: `nBlock`
agents_between_goal <- function(agent, 
                                state, 
                                agent_predictions = NULL) {

    # If no predicted positions are delivered to the function, we will use the 
    # current positions to find out whether agents are standing inbetween `agent`
    # and its goal. Otherwise, we will use the predicted positions.
    if(is.null(agent_predictions)) {
        agent_positions <- lapply(state$agents, position)
        agent_positions <- do.call("rbind", 
                                   agent_positions)
    } else {
        agent_positions <- agent_predictions[-id(agent),] |>
            matrix(ncol = 2)
    }

    # If there are no other agents to consider, we can safely exit the function
    if(length(agent_positions) == 0) {
        return(0)
    }
    colnames(agent_positions) <- c("x", "y")

    # Create a cone from the agent to the goal location
    goal_cone <- m4ma::Iangle(matrix(position(agent), 
                                     nrow = 1, 
                                     ncol = 2), 
                              orientation(agent), 
                              matrix(current_goal(agent)@position,
                                     nrow = 1, 
                                     ncol = 2))

    if(is.na(goal_cone)) {
        return(0)
    }

    # The discrete cone bin in which the goal is contained is the same for each
    # of the velicities. Furhermore retain only those goal cones that fall 
    # within view of the agent.
    cone_set <- c(goal_cone - 1, goal_cone, goal_cone + 1)
    cone_set <- cone_set[cone_set > 0 & cone_set < 12]

    # Compute the end points of the orthogonal line segments to the line between
    # the agent and the other agents with a width equal to the radius of the 
    # agent. 
    #
    # These lines can be used to check the intersection with the cone drawn from 
    # the agent to their goal. If there is such an intersection, we can assume 
    # that agent is blocking the goal.
    #
    # Important note: Only extract the anticlockwise cone (`ac`), which is also 
    # the direction that Iangle works in
    ends <- m4ma::eObjects(matrix(position(agent),
                                  nrow = 1, 
                                  ncol = 2),
                           agent_positions, 
                           size(agent))$ac

    # Create several cones from the agent to these end points
    end_cones <- apply(ends,
                       1, 
                       \(x) m4ma::Iangle(matrix(position(agent), ncol = 2), 
                                         orientation(agent),
                                         matrix(x, ncol = 2)))

    # Check whether any of the coordinates fall inside of the cone_set
    blocking <- sapply(end_cones, 
                       function(x) {
                           if(any(is.na(x))) {
                               return(FALSE)
                           } else {
                               return(any(x %in% cone_set))
                           }
                       })

    # Get the number of blocking agents
    n_agents <- sum(blocking)

    # Give them the names of the agents
    # if(n_agents > 0) {
    #     attr(n_agents, "ends") <- ends[names(blocking), , , drop = FALSE]
    # }

    return(n_agents)
}

# Temporary function to see if this one actually works
#
# TO DO
#   - Assumes the agent is a circle: Change this
#' Check whether an agent overlaps with an object
#' 
#' @param agent The agent for whom to compute the utilities
#' @param background The setting in which agents are walking around
#' @param centers The centers of all the positions the agent may move to
#' @param check Boolean denoting the centers the agent can actually move to
#' (which are not blocked)
#' 
#' @export
overlap_with_objects <- function(agent, 
                                 background, 
                                 centers, 
                                 check) {
    
    # If the centers are not provided, return an empty logical
    if(length(centers) == 0) {
        return(logical(0))
    }

    # Transform all background-objects into one big matrix of segments. This will
    # allow us to vectorize the search for intersections between the agent and 
    # an object, hopefully speeding up the search.
    coords <- lapply(objects(background), 
                     \(x) nodes_on_circumference(x, space_between = 5e-2))
    coords <- do.call("rbind", coords)

    # Do the same for the background and bind this together with the other 
    # coordinates. Saves a lot of time, as the original function that was used 
    # (`intersects`) is quite time-inefficient.
    coords <- rbind(coords, 
                    nodes_on_circumference(shape(background), 
                                           space_between = 5e-2))

    # Only retain those coordinates that are within a certain range around the 
    # agent.
    agent_position <- position(agent)

    cutoff <- max(((centers[,1] - agent_position[1])^2 + (centers[,2] - agent_position[2])^2))
    cutoff <- sqrt(cutoff) + radius(agent)
    distances <- sqrt((coords[,1] - agent_position[1])^2 + (coords[,2] - agent_position[2])^2)

    coords <- coords[distances <= cutoff, ] |>
        matrix(ncol = 2)

    # Small check: If none of the objects is even within the same region, we 
    # can just return the check as is
    if(length(coords) == 0) {
        return(check)
    }

    # Loop over the centers
    tryCatch(local_check <- matrix(TRUE, nrow = nrow(centers), ncol = nrow(coords)),
             error = function(e) {browser()})
    for(i in seq_len(nrow(centers))) {
        # If that center is already out of the running, we don't need to do 
        # an additional check
        if(!check[i]) {
            local_check[i,] <- TRUE

        # If the center is still in the running, we need to check whether the 
        # coordinates of the objects lie within the agent's new potential 
        # position.
        } else {
            # Change the center of the agent
            center(agent) <- centers[i,]

            # First check whether there is an intersection with the shape of the 
            # background. Idea is that none of the coordinates should be within the 
            # agent, in which case the `any` statement returns FALSE. However, the 
            # check should read TRUE when one of the coordinates is interpreted as 
            # being okay, hence the reversal of the logical.
            # check[i] <- !any(in_object(agent, coords, outside = FALSE))
            local_check[i,] <- in_object(agent, coords, outside = FALSE)
        }
    }

    if(is.null(ncol(check))) {
        return(!(rowSums(local_check) > 0))
    } else {
        return(matrix(!(rowSums(local_check) > 0), ncol = ncol(check)))
    }
}

