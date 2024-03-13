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
moving_options_agent <- function(agent, state, background, centers){
    # Create a local function that will be useful for debugging
    check_try_error <- function(x, error_number){
        e <- !all(!x) |>
            try() |>
            class()

        if(e){
            return(list(error_location = error_location,
                        agent_id = id(agent),
                        state = state,
                        agent = agent,
                        centers = centers))
        } else {
            return(NULL)
        }
    }

    # Get the index of the agent in the state$agent list
    agent_id <- sapply(state$agents, id)
    agent_idx <- which(id(agent) == agent_id)

    # Use the `free_cells` function to get all free cells to which the agent
    # might move and check whether it does not provide an error
    check <- m4ma::free_cells_rcpp(agent, background, centers)

    # errored_out <- check_try_error(check, "after `free_cells`")
    # if(!is.null(errored_out)){
    #     bad <<- errored_out
    # }

    # If there are still cells free, check whether an agent would intersect with
    # an object if it were to move to a given cell. Given that the function
    # `overlap_with_object` only checks those cells that are free, the output
    # of this function can overwrite the local `check` variable without any
    # issues
    if(!all(!check)){
        # check <- overlap_with_object(agent, state, centers, check)
        check <- m4ma::bodyObjectOK_rcpp(size(agent), centers, objects(background), check)
        # Not sure why this is done, but I'll leave it in just to be certain
        check[!check[,3],2] <- FALSE
        check[!check[,2],1] <- FALSE
    }
    # errored_out <- check_try_error(check, "after `overlap_with_objects`")
    # if(!is.null(errored_out)){
    #     bad <<- errored_out
    # }

    # If there are still cells free, check whether the goal can still be seen
    # or whether an agent should re-plan
    if(!all(!check)){
        # Weird bug: running seesGoalOK_rcpp changes `check` or copy of `check`.
        # Keep copy of the opposite of `check` for later use
        opposite_check <- !check

        # Function to rewrite! New arguments are already provided to this one,
        # but not the original one
        goal_position <- current_goal(agent)@position@.Data
        goal_list <- list(matrix(goal_position, ncol = 2))
        attr(goal_list[[1]], "i") <- 1
        state_dummy <- list(P = goal_list)
        local_check <- m4ma::seesGoalOK_rcpp(1, objects(background), state_dummy, centers, check)
        # Here, change `check` based on the results of the function. Importantly,
        # agent should still move even if it cannot see their goal (hence the
        # if-statement), otherwise the agent will get stuck
        check <- if(!all(!local_check)) local_check else !opposite_check
    }
    
    # errored_out <- check_try_error(check, "after `seesGoalOK`")
    # if(!is.null(errored_out)){
    #     bad <<- errored_out
    # }

    # If there are still cells free, check whether there are cells in which another
    # agent is currently standing
    if(!all(!check)){
        # Originally deleted the agent from the state$agent list. However, not 
        # necessary anymore! The state$agents list already does not contain the 
        # `agent` anymore
        #
        # Additional condition added: If there are not other agents, then we 
        # don't need to do this check
        if(length(state$agents[-agent_idx]) > 0) {
            check <- m4ma::bodyObjectOK_rcpp(size(agent), centers, state$agents[-agent_idx], check)
        }
    }

    # Finally, return the cells that are free to move to
    return(check)
}

#' @rdname blocked-methods
#' @aliases blocked, agent
setMethod("moving_options",
          "agent",
          moving_options_agent)

