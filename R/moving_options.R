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
    
    shp <- shape(background)
    obj <- objects(background)

    # Transform all background-objects into one big matrix of segments. This will
    # allow us to vectorize the search for intersections between the agent and 
    # an object, hopefully speeding up the search.
    coords_obj <- matrix(0, nrow = 0, ncol = 2)
    for(i in obj) {
        coords_obj <- rbind(coords_obj, 
                            nodes_on_circumference(i))
    }

    # Do the same for the background. Saves a lot of time, as the original 
    # function that was used (`intersects`) is quite time-inefficient.
    coords_shp <- nodes_on_circumference(shp)

    # Loop over the centers
    for(i in seq_len(nrow(centers))) {
        # If that center is already out of the running, continue
        if(!check[i]) {
            next
        }

        # Change the center of the agent
        center(agent) <- centers[i,]

        # First check whether there is an intersection with the shape of the 
        # background
        if(any(in_object(agent, coords_shp, outside = FALSE))) {
            check[i] <- FALSE
            next
        }

        # And now whether there is an intersection with the objects of the 
        # background
        if(any(in_object(agent, coords_obj, outside = FALSE))) {
            check[i] <- FALSE
        }
    }

    return(check)
}

# Helper function to create dots on the circumference of objects, to be used for
# overlap_with_object
nodes_on_circumference <- function(object) {
    if(inherits(object, "circle")) {
        nodes <- to_polygon(object, length.out = ceiling(2 * pi * radius(object) / 5e-2))
    } else {
        corners <- object@points 
        n <- nrow(corners)

        x_changes <- cbind(corners[,1], corners[c(2:n, 1), 1])
        y_changes <- cbind(corners[,2], corners[c(2:n, 1), 2])

        len_x <- ceiling(abs((x_changes[,2] - x_changes[,1]) / 5e-2))
        len_y <- ceiling(abs((y_changes[,2] - y_changes[,1]) / 5e-2))

        len <- matrixStats::rowMaxs(cbind(len_x, len_y))

        nodes <- cbind(as.numeric(unlist(multi_seq(x_changes[,1], 
                                                   x_changes[,2],
                                                   length.out = len))),
                       as.numeric(unlist(multi_seq(y_changes[,1], 
                                                   y_changes[,2],
                                                   length.out = len))))
    }
    
    return(nodes)
}