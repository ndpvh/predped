#' Check where an object can be moved to
#'
#' This method checks where an object can be moved to. It returns a logical 
#' matrix that codes \code{TRUE} for the cells that are available and 
#' \code{FALSE} for those that aren't.
#' 
#' @details
#' In general, this method works as follows. First, it checks whether any of the 
#' provided cell centers are freely available, in the sense that they are not 
#' contained inside any objects or fall outside of the setting. This is a crude
#' measure of whether a particular spot is available and is handled by the 
#' \code{\link[m4ma]{free_cells_rcpp}} function of the \code{m4ma} package.
#' 
#' Second, we check whether the object itself can be moved to this space, or 
#' whether it would intersect with any of the objects and/or the outline of the 
#' setting. This is a more direct measure of availability, as it doesn't only 
#' account for whether a specific spot can be reached theoretically, but also 
#' accounts for the size of the object that is being moved there. This is 
#' handled by the \code{\link[predped]{overlap_with_objects}} function in 
#' \code{predped}.
#' 
#' Finally, if the object is an instance of the \code{\link[predped]{agent-class}}, 
#' we also check whether the agent can still see there current goal or path-point
#' when they move to the open spots. They will not move to the spots from which
#' they cannot see their goal/path-point. This is handled by the 
#' \code{\link[m4ma]{seesGoalOK_rcpp}} function in the \code{m4ma} package.
#' 
#' WARNING: Due to its reliance on the \code{m4ma} package, centers needs to be 
#' of length 33 x 2. This corresponds to the 3 (change in speed) x 11 
#' (change in orientation) options that are inherent to M4MA.
#'
#' @param object Object of the \code{\link[predped]{agent-class}} or the 
#' \code{\link[predped]{object-class}} (latter not yet supported).
#' @param state Object of the \code{\link[predped]{state-class}} containing the 
#' current state.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param centers Numerical matrix containing the coordinates at each position
#' the object can be moved to. Should have one row for each cell.
#' @param cpp Logical denoting whether to use the Rcpp alternative (\code{TRUE})
#' or the R alternative of this function (\code{FALSE}). Defaults to \code{TRUE}.
#'
#' @return Logical matrix containing availabilities of the centers.
#' 
#' @examples 
#' # Initialize all objects that you need
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(6, 6)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 2)))
#' my_agent <- agent(center = c(-2.75, 0), 
#'                   radius = 0.25, 
#'                   speed = 1, 
#'                   orientation = 0,
#'                   current_goal = goal(position = c(-2.01, 0)))
#' 
#' my_state <- state(iteration = 1,
#'                   setting = my_background, 
#'                   agents = list())
#' 
#' # Generate several locations the agent can move to
#' centers <- m4ma::c_vd_r(1:33, 
#'                         position(my_agent), 
#'                         speed(my_agent), 
#'                         orientation(my_agent))
#' 
#' # Use moving_options to see which of these possibilities is sound
#' moving_options(my_agent, 
#'                my_state, 
#'                my_background,
#'                centers)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{background-class}},
#' \code{\link[predped]{object-class}},
#' \code{\link[predped]{state-class}},
#' \code{\link[predped]{overlap_with_objects}} 
#'
#' @docType methods
#' 
#' @rdname moving_options-methods
#'
#' @export
#
# TO DO:
#   - Change so that it can just take in agent and state: No need to have 
#     background in there
#   - Make none-reliant on m4ma if we want to extend the possibilities of the 
#     centers
setGeneric("moving_options", function(object, ...) standardGeneric("moving_options"))

#' @rdname moving_options-methods
#' 
#' @export
setMethod("moving_options", "agent", function(object, 
                                              state, 
                                              background, 
                                              centers,
                                              cpp = TRUE){

    # If the user wants to use the cpp version, allow them to do so
    if(cpp) {
        return(moving_options_rcpp(object, state, background, centers))
    }

    # Add the other agents to the background objects. This will allow us to 
    # immediately test whether cells are occupied by other agents instead of 
    # doing this check only later.
    # 
    # Differentiate between a list that does contain the other agents 
    # (slot in background) and a list that does not (obj). This distinction is 
    # used later when checking the seeing of goals 
    ids <- sapply(agents(state), id)
    agents_minus_agent <- agents(state)[ids != id(object)]

    obj <- objects(background)
    objects(background) <- append(objects(background), 
                                  agents_minus_agent)   

    # Use the `free_cells` function to get all free cells to which the agent
    # might move. Specifically look at whether a cell lies within the background
    # and whether the agent has a direct line of sight to that cell.
    check <- m4ma::free_cells_rcpp(object, background, centers)

    # Additional thing to check: Make sure none of these centers lies within an 
    # object. Apparently, this is not automatically checked in `free_cells_rcpp`!
    check_in <- lapply(objects(background), 
                       \(x) in_object(x, centers))
    check <- check & !Reduce("|", check_in)

    # If something blocks the way in the previous column, then it should also 
    # block the way on the columns. Note that we do this twice: Once here and 
    # once after `overlap_with_objects`. Apparently, the model is sensitive to 
    # both in their own right: Moving this check to once at the end changes how
    # the pedestrians move around!
    check[!check[,3],2] <- FALSE
    check[!check[,2],1] <- FALSE

    # If there are still cells free, check whether an agent would intersect with
    # an object if it were to move to a given cell. Given that the function
    # `overlap_with_object` only checks those cells that are free, the output
    # of this function can overwrite the local `check` variable without any
    # issues
    if(!all(!check)) {
        # check <- m4ma::bodyObjectOK_rcpp(size(agent), centers, objects(background), check) # Original
        check <- overlap_with_objects(object, background, centers, check, cpp = FALSE)
        # check <- bodyObjectOK(size(object), centers, objects(background), as.vector(check))

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
        goal_position <- current_goal(object)@path[1,]
        goal_list <- list(matrix(goal_position, ncol = 2))
        attr(goal_list[[1]], "i") <- 1
        state_dummy <- list(P = goal_list)
        
        # local_check <- m4ma::seesGoalOK_rcpp(1, objects(background), state_dummy, centers, check)
        local_check <- m4ma::seesGoalOK_rcpp(1, obj, state_dummy, centers, check)

        # Here, change `check` based on the results of the function. Importantly,
        # agent should still move even if it cannot see their goal (hence the
        # if-statement), otherwise the agent will get stuck
        check <- if(!all(!local_check)) local_check else !opposite_check
    }

    # Make sure check is always in matrix format
    if(!is.matrix(check)) {
        check <- matrix(check, ncol = 3)
    }

    # Finally, return the cells that are free to move to
    return(check)
})

#' Find number of agents blocking agent
#' 
#' This function computes the number of other agents in a particular state who 
#' are blocking the way between the current agent and their goal. This is done 
#' so that the current agent can reevaluate whether they want to keep pursuing 
#' this way to their goal, or whether they want to reroute. 
#' 
#' @param agent Object of the \code{\link[predped]{agent-class}}.
#' @param state Object of the \code{\link[predped]{state-class}}.
#' @param agent_predictions Matrix containing the predicted positions of all 
#' agents in the simulation, where the id's  of the agents serve as the 
#' rownames. Is typically handled under the hood. Defaults to \code{NULL}, 
#' triggering this function to consider where the other agents are right now 
#' (instead of where they will be in the next iteration).
#' 
#' @return List containing all of the agents that are blocking the path
#' 
#' @examples 
#' # Create an agent and a state
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(6, 6)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 1)))
#' my_agent <- agent(center = c(-2.75, 0), 
#'                   radius = 0.25, 
#'                   speed = 1, 
#'                   orientation = 0,
#'                   current_goal = goal(position = c(-1.01, 0)))
#' 
#' my_state <- state(iteration = 1,
#'                   setting = my_background, 
#'                   agents = list(agent(center = c(-2, 0), radius = 0.25), 
#'                                 agent(center = c(-1.5, 0), radius = 0.25), 
#'                                 agent(center = c(2, 0), radius = 0.25), 
#'                                 agent(center = c(1.5, 0), radius = 0.25)))
#' 
#' # Find out how many agents are blocking the way for my_agent
#' agents_between_goal(my_agent, my_state)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{state-class}},
#' \code{\link[predped]{moving_options}}
#' 
#' @rdname agents_between_goal
#' 
#' @export
#
# TO DO:
#   - The purpose of `cone_set` is not entirely clear and might have to change if 
#     we wish to consider more or less than 33 choices
#   - There might be an easier, object-oriented way of finding out how many agents
#     are blocking the goal
agents_between_goal <- function(agent, 
                                state, 
                                agent_predictions = NULL) {

    # Extract the agent list and delete the agent in question from this list.
    agent_list <- agents(state)
    agent_list <- agent_list[sapply(agent_list, id) != id(agent)]

    # If no predicted positions are delivered to the function, we will use the 
    # current positions to find out whether agents are standing inbetween `agent`
    # and its goal. Otherwise, we will use the predicted positions.
    if(is.null(agent_predictions)) {
        agent_positions <- lapply(agent_list, position)
        agent_positions <- do.call("rbind", agent_positions)
    } else {
        agent_positions <- agent_predictions[-id(agent),] |>
            matrix(ncol = 2)
    }

    # If there are no other agents to consider, we can safely exit the function
    if(length(agent_positions) == 0) {
        return(list())
    }
    colnames(agent_positions) <- c("x", "y")

    # Create a cone from the agent to the goal location
    goal_cone <- m4ma::Iangle(matrix(position(agent), 
                                     nrow = 1, 
                                     ncol = 2), 
                              orientation(agent), 
                              matrix(current_goal(agent)@path[1,],
                                     nrow = 1, 
                                     ncol = 2))

    if(is.na(goal_cone)) {
        return(list())
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

    # Return those agents that are blocking the agent
    return(agent_list[blocking])
}

#' Check agent and object overlap
#' 
#' This function checks whether there is an overlap between a given agent and 
#' the objects in the environment, provided that the agent would move to the 
#' locations in \code{centers}. Returns a logical matrix as needed in 
#' \code{\link[predped]{moving_options-method}}.
#' 
#' @details
#' In this function, we can only approximately check the intersection of agent 
#' and object. Specifically, we use the following method. First, we sample 
#' nodes on the circumference of each of the objects in the setting that is 
#' provided to this function. For this, we depend on the function 
#' \code{\link[predped]{nodes_on_circumference}} and we currently take these 
#' nodes to be 5cm. 
#' 
#' In the next step, we bind all these coordinates together in a single matrix. 
#' This matrix thus consists of nodes that should not be embedded in the agents:
#' Whenever one of these points is included in the agents, we can conclude that
#' the agents and objects intersect. [Note, however, that if these points are 
#' not included in the agents, that we cannot with certainty conclude that agent 
#' and object do not intersect]
#' 
#' This check is then performed by looping over all the centers, changing the 
#' agents position to the position of this center, and using the 
#' \code{\link[predped]{in_object-method}} to do the test. This is a vectorized 
#' test: For each position in \code{centers} we have a logical \code{TRUE} or 
#' \code{FALSE} for each of the nodes in the coordinate matrix, resulting in a 
#' logical matrix with an equal number of rows as \code{centers} and an equal 
#' number of columns as nodes in the coordinate matrix. In a last step, 
#' we aggregate over the columns in this matrix so that we have a single logical
#' for each center.
#' 
#' The reason why we use this approximate method is because of time efficiency. 
#' Using the \code{\link[predped]{intersects-method}} takes a longer time than 
#' using the \code{\link[predped]{in_object-method}}, especially as the number 
#' of objects in the environment increases.
#' 
#' @param agent Object of the \code{\link[predped]{agent-class}}.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param centers Numerical matrix containing the coordinates at each position
#' the object can be moved to. Should have one row for each cell.
#' @param check Logical matrix of dimensions 11 x 3 denoting whether an agent 
#' can move to a given cell (\code{TRUE}) or not (\code{FALSE}).
#' @param space_between Numeric denoting the space to leave between the nodes 
#' put on the circumference of the objects in the space (used for checking the
#' overlap with an agent). Defaults to \code{0.05} or 5cm.
#' @param cpp Logical denoting whether to use the Rcpp alternative (\code{TRUE})
#' or the R alternative of this function (\code{FALSE}). Defaults to \code{TRUE}.
#' 
#' @return Logical matrix containing availabilities of the centers (\code{TRUE}
#' if available, \code{FALSE} if not).
#' 
#' @examples 
#' # Initialize all objects that you need
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(6, 6)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 2)))
#' my_agent <- agent(center = c(-2.75, 0), 
#'                   radius = 0.25, 
#'                   speed = 1, 
#'                   orientation = 0,
#'                   current_goal = goal(position = c(-2.01, 0)))
#' 
#' # Generate several locations the agent can move to
#' centers <- m4ma::c_vd_r(1:33, 
#'                         position(my_agent), 
#'                         speed(my_agent), 
#'                         orientation(my_agent))
#' check <- matrix(TRUE, nrow = 11, ncol = 3)
#' 
#' # Use moving_options to see which of these possibilities is sound
#' overlap_with_objects(my_agent, 
#'                      my_background,
#'                      centers,
#'                      check)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{background-class}},
#' \code{\link[predped]{in_object}},
#' \code{\link[predped]{intersects}},
#' \code{\link[predped]{moving_options}},
#' \code{\link[predped]{nodes_on_circumference}}
#' 
#' @rdname overlap_with_objects
#' 
#' @export
#
# TO DO 
#   - Assumes the agent is circular, but we don't want to depend on this.
overlap_with_objects <- function(agent, 
                                 background, 
                                 centers, 
                                 check,
                                 space_between = 5e-2,
                                 cpp = FALSE) {
    
    # If the centers are not provided, return an empty logical
    if(length(centers) == 0) {
        return(logical(0))
    }

    # If you want to use the cpp version. Allow the person to do so
    if(cpp) {
        return(overlap_with_objects_rcpp(agent, background, centers, check, space_between))
    }

    # Transform all background-objects into one big matrix of segments. This will
    # allow us to vectorize the search for intersections between the agent and 
    # an object, hopefully speeding up the search.
    coords <- lapply(objects(background), 
                     \(x) nodes_on_circumference(x, space_between = space_between))
    coords <- do.call("rbind", coords)

    # Do the same for the background and bind this together with the other 
    # coordinates. Saves a lot of time, as the original function that was used 
    # (`intersects`) is quite time-inefficient.
    coords <- rbind(coords, 
                    nodes_on_circumference(shape(background), 
                                           space_between = space_between))

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
            local_check[i,] <- in_object(agent, coords)
        }
    }

    if(is.null(ncol(check))) {
        result <- (!(rowSums(local_check) > 0))
    } else {
        result <- (matrix(!(rowSums(local_check) > 0), ncol = ncol(check)))
    }

    return(result)
}