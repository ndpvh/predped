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
#' @export
#' @docType methods
#' @rdname blocked-methods
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
moving_options_agent <- function(agent, state, centers){
    # Create a local function that will be useful for debugging
    check_try_error <- function(x, error_number){
        e <- !all(!x) |>
            try() |>
            class()

        if(e){
            return(list(error_location = error_location, 
                        agent_id = agent@id, 
                        state = state, 
                        agent = agent, 
                        centers = centers))
        } else {
            return(NULL)
        }
    }

    # Use the `free_cells` function to get all free cells to which the agent 
    # might move and check whether it does not provide an error
    check <- free_cells(agent, state, centers)

    errored_out <- check_try_error(check, "after `free_cells`")
    if(!is.null(errored_out)){
        bad <<- errored_out
    }

    # If there are still cells free, check whether an agent would intersect with
    # an object if it were to move to a given cell. Given that the function
    # `overlap_with_object` only checks those cells that are free, the output
    # of this function can overwrite the local `check` variable without any 
    # issues
    if(!all(!check)){
        check <- overlap_with_object(agent, state, centers, check)
        # Not sure why this is done, but I'll leave it in just to be certain 
        check[!check[,3],2] <- FALSE
        check[!check[,2],1] <- FALSE
    }

    errored_out <- check_try_error(check, "after `overlap_with_objects`")
    if(!is.null(errored_out)){
        bad <<- errored_out
    }

    # If there are still cells free, check whether the goal can still be seen 
    # or whether an agent should re-plan
    if(!all(!check)){
        # Weird bug: running seesGoalOK_rcpp changes `check` or copy of `check`. 
        # Keep copy of the opposite of `check` for later use
        opposite_check <- !check 

        # Function to rewrite! New arguments are already provided to this one, 
        # but not the original one
        local_check <- seesGoalOK(agent, state, centers, check)

        # Here, change `check` based on the results of the function. Importantly,
        # agent should still move even if it cannot see their goal (hence the 
        # if-statement), otherwise the agent will get stuck
        check <- ifelse(!all(!local_check), 
                        local_check, 
                        !opposite_check)
    }

    errored_out <- check_try_error(check, "after `seesGoalOK`")
    if(!is.null(errored_out)){
        bad <<- errored_out
    }
    
    # If there are still cells free, check whether there are cells in which another
    # agent is currently standing
    if(!all(!check)){
        check <- okBodyBody(agent, state, centers, check)
    }

    # Finally, return the cells that are free to move to
    return(check)
}

#' @rdname blocked-methods
#' @aliases blocked, agent
setMethod("moving_options", 
          "agent",
          moving_options_agent)

#' Find cells that are not blocked
#' 
#' This function checks whether all cells that the agent or the object can move 
#' to are currently in the room. Furthermore establish whether these same cells
#' are not blocked by any other state. 
#' 
#' @param agent An agent's possibilities to be checked
#' @param state A list of the current state in the simulation.
#' @param centers A matrix containing the location of the centers of the cells
#' an agent may move to in two dimensions (x and y)
#' 
#' @return Logical indicating whether object is in the room and not blocked 
#' by any state. 
# 
# Replacement of `okObject`
free_cells <- function(agent, state, centers) {
    # Select the background from the current state
    background <- find_class("background", state) 

    # Check whether the cells are all within the environment (currently defined 
    # by state[[1]])
    check <- apply(centers, 1, \(x) in_object(background@shape, x, outside = FALSE)) 
                
    # Check whether there are other state than only the environment in the 
    # state list. If so, then we need to adjust the `check` variable so that 
    # it accounts for blocking of some options due to state being in the way, 
    # for which we call the `blocked_cells` function
    check <- ifelse(length(state) > 1, 
                    check & !blocked_cells(agent, state, centers),
                    check)
    
    # Transform the check back into a matrix that can be used for cell 
    return(matrix(check, ncol = 3))
}

# Check whether cells are blocked
#
# This checks whether a given potential cell to which the agent can move is in 
# reality blocked by an object.  
#
# @param agent An agent's possibilities to be checked
# @param state A list of the current state in the simulation.
# @param centers A matrix of cell locations in two dimensions (x and y) for a 
# given agent.
# @param check A vector of logicals that denotes which cells are fine and which 
# are not. Defaults to all cells being `FALSE` 
#
# @returns A logical vector denoting whether the cells positions are deemed okay 
# or not
#
# TO DO: 
#   - Make sure that logical depends on the number of centers, not on a 
#     hard-coded value. 
#   - Make sure that centers itself depends on the provided angles, which again 
#     should not be hardcoded in there. 
#   - Make robust against the drop-argument being a different size than `idx`. 
#   - Test whether you can unreverse the logical that is given back, and 
#     unreverse it in the `free_cells` function as well (another reverse is 
#     found in `blocked_agent`).
#
# Replacement of `isBlocked`
blocked_cells <- function(agent, state, centers, check = logical(33)) {
    # Create a nested function that will check the line of side for each of the
    # cells in center within a specific range
    apply_check <- function(idx, drop = !logical(length(idx))){
        # Create a `check` variable that will contain the cells that are visible
        local_check <- logical(length(idx))

        # Use the `drop` argument to determine which indices should stay. 
        # To adjust the code for both cases, make an `idy` that will go from 
        # `1:length(idx)`, allowing different indexing of the `check` and the 
        # actual `centers`
        idy <- seq_along(idx)[drop]
        idx <- idx[drop]

        # Create the logical vector that will contain the blocking code and 
        # adjust it according to the results of `seesGoal`        
        local_check[idy] <- apply(centers[idx,, drop = FALSE], 1,
                                  \(x) seesGoal(x, agent@position, state))

        return(local_check)
    }

    # Determine the positions of the inner ring that can be used. Then do the 
    # same for the middle and outer ring using the results of the inner ring as 
    # a basis (if the inner ring is blocked, this block should proceed to )
    check[23:33] <- apply_check(23:33)
    check[12:22] <- apply_check(12:22, drop = check[23:33])
    check[1:11] <- apply_check(1:11, drop = check[12:22])

    # Question, why is this reversed?
    return(!check)
}

#' Find cells for which body overlaps with an object
#' 
#' This function checks whether the cells that an agent can move do not make 
#' the agent's body overlap with an object. In effect, this tests whether the 
#' size of an agent is not too big for the cells that the agent wants to move 
#' to. 
#' 
#' @param agent An agent to be checked
#' @param state A list of the current state in the simulation.
#' @param centers A matrix containing the location of the centers of the cells
#' an agent may move to in two dimensions (x and y)
#' 
#' @return Logical indicating whether object is in the room and not blocked 
#' by any state. 
# 
# TO DO: 
#   - Delete the extra check at the beginning of this function. 
#   - Allow for agent to become rectangle as well (see agent specifications): 
#     At this moment, still assumed to be circular.
#   - Make more general so that overlap with agents is immediately covered as 
#     well (same generality TO DO exists for `object_intersection` and 
#     `intersection`)
# 
# Replacement of `bodyObjectOK`
overlap_with_object <- function(agent, state, centers, free) {
    # Keeping this in here, but I believe it may be deleted afterwards, as this
    # kind of check is already done in `blocked_agent`
    if (!any(ok)) {
        return(NULL)
    }

    # Create an output logical vector that will indicate whenever there is 
    # overlap between agent and object
    overlap <- !logical(nrow(centers))

    # For each of the state get all the lines with which the agent might 
    # intersect
    object_lines <- lapply(state, object2lines)
    
    # Check whether the overlap exists for the centers that can still be used.
    # First apply the intersection function, then transform the result to a 
    # matrix and check for each row (for each object) whether any of the lines 
    # of the object intersected with the agent
    free_centers <- centers[as.vector(free),, drop = FALSE]
    overlap[free] <- lapply(object_lines, 
                            \(x) object_intersection(x, 
                                                     agent@size,
                                                     free_centers)) |>
        unlist() |>
        matrix(nrow = sum(free)) |>
        apply(1, any)
    
    # If it doesn't overlap it is OK
    return(matrix(!overlap, nrow = 11))
}

# Do agent and object lines intersect 
#
# This checks on the lower level whether the lines of a given object intersect 
# with an agent whether a given potential cell to which the agent can move is in 
# reality blocked by an object.  
#
# @param lines A list containing the lines of an object
# @param radius A list of state in the surroundings that might hamper or aid 
# the agent.
# @param centers A matrix of cell locations in two dimensions (x and y) that the 
# agent might move to. In practice, this is limited to only the cells that 
# remain after another check.
#
# @returns A logical vector denoting whether overlap was found (`TRUE`) or not
# (`FALSE`)
#
# TO DO: 
#   - Replace this function to work with the state themselves, allowing 
#     more than one type of object (e.g., circular). 
#   - Make a more general function that just looks for overlap in a dependent 
#     object (y; agent or moveable object) and independent state (x; other 
#     agents or state).
#
# Replacement of `bodyObjectOverlap`
object_intersection <- function(lines, radius, centers) {
    # Get the angles of the lines
    angles <- angle2(p1 = t(lines[, , "P1"]), p2 = t(lines[, , "P2"])) |>
        (`+`) (90) |>
        (`%%`) (180) |>
        unique()
    
    # dx and dy to move along line
    x <- radius * sin(angles * pi / 180)
    y <- radius * cos(angles * pi / 180)
    
    # For each centre check for overlap
    return(apply(centers, 1, \(points) intersection(points, x, y, lines)))
}

# Line to line intersection
#
# @params point A numeric denoting the center point
# @params x A numeric denoting the deviation from this center point in the 
# x direction
# @params y A numeric denoting the deviation from this center point in the 
# y direction
# @params lines  A list containing the lines of an object
#
# @returns A logical that denotes whether the provided lines intersect (TRUE) or
# not (FALSE)
#
# TO DO: 
#   - Just like for its original parent `object_intersection`, make this 
#     very general.
intersection <- function(point, x, y, lines){ 
    # Create the segments that you want to know the intersection of. This is 
    # just adding and subtracting x and y from the point that was provided. Then 
    # transform this to an three-dimensional array with a given format
    xy <- rbind(x, y)
    segments <- c(point - xy, point + xy) |>
        array(dim = c(2, length(x), 2),
              dimnames = list(c("x", "y"), NULL, c("P1", "P2")))

    
    # For each of the segments, determine whether they intersect with the agent
    intersect <- FALSE
    for (j in seq_len(dim(segments)[2])){
        intersect <- sapply(seq_len(dim(lines)[2]), 
                            \(i) function(i){
                                intersect <- line.line.intersection(segments[,j,"P1"], 
                                                                    segments[,j,"P2"], 
                                                                    lines[,i,"P1"], 
                                                                    lines[,i,"P2"], 
                                                                    TRUE) |>
                                    is.finite() |>
                                    all()

                                return(intersect)
                            }) |>
            any()
    }

    return(intersect)
}

# Is the cell OK or does body radius r positioned at centers overlap with 
# another body?   
okBodyBody <- function(n, state, centers, ok) {
    if (!any(ok)) {
        return(NULL)
    }
    d <- state$r[n] + state$r[-n]
    out <- !logical(33)
    
    # Any overlap?
    out[ok] <- apply(centers[ok, , drop = FALSE], 1, function(x) {
        any(dist1(matrix(x, nrow = 1), state$p[-n, , drop = FALSE]) < d)
    })
    
    # If it doesnt overlap it is OK
    matrix(!out, nrow = 11)
}

# Number of pedestrians in the goal direction (goal cone or the cones on either 
# side of it).
# returns number and if more than one attribute "ends" specifying blocking
# profiles from current perspective 
nBlock <- function(n, state, r, usePredictedPos = FALSE) {
    p1 <- state$p[n, , drop = FALSE]
    P <- state$P[[n]][attr(state$P[[n]], "i"), 1:2, drop = FALSE]
    
    if (usePredictedPos) {
        p2 <- predictPed(state$p, state$v, state$a, state$cell)[-n, , drop = FALSE] 
    } else {
        p2 <- state$p[-n, , drop = FALSE]
    }
    
    if (dim(p2)[1] == 0) {
        return(0)
    }
    
    goalCone <- Iangle(p1, state$a[n], P)
    if (is.na(goalCone)) {
        return(0)
    }
    coneSet <- c(goalCone - 1, goalCone, goalCone + 1)
    coneSet <- coneSet[coneSet > 0 & coneSet < 12]
    
    ends <- estate(p1, p2, r)
    endCones <- apply(ends, 3, function(x) { 
        Iangle(p1, state$a[n], x)
    })
    if (dim(ends)[1] == 1) { 
        endCones <- matrix(endCones, nrow = 1, 
                        dimnames = list(dimnames(p2)[1], NULL))
    }
    
    blockers <- apply(endCones, 1, function(x) {
        if (any(is.na(x))) {
        FALSE 
        } else {
        any(x %in% coneSet)
        }
    })
    
    blockers <- blockers[blockers]
    nBlockers <- length(blockers)
    if (nBlockers > 0) {
        attr(nBlockers, "ends") <- ends[names(blockers), , , drop = FALSE]
    }
    return(nBlockers)
}
