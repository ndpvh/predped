#' Transform trace to time-series
#' 
#' @param trace List of objects of the \code{\link[predped]{state-class}}
#' @param time_step Numeric denoting the time between each iteration. Defaults 
#' to \code{0.5} (the same as in \code{\link[predped]{simulate,predped-method}}).
#' 
#' @examples
#' # This is my example
#'
#' @rdname time_series
#' 
#' @export
time_series <- function(x, 
                        time_step = 0.5) {

    # Create a function that will extract all details of the agents from a 
    # particular state.
    extract_state <- function(y) {
        y <- lapply(y@agents, 
                    \(a) data.frame(iteration = y@iteration,
                                    time = 0,
                                    id = id(a),
                                    x = position(a)[1], 
                                    y = position(a)[2], 
                                    speed = speed(a), 
                                    orientation = orientation(a), 
                                    cell = cell(a), 
                                    group = group(a), 
                                    status = status(a),
                                    goal_x = current_goal(a)@position[1], 
                                    goal_y = current_goal(a)@position[2],
                                    radius = radius(a)))
        return(do.call("rbind", y))
    }

    # Iterate over each object in the list and extract the state. 
    x <- lapply(x, extract_state)
    x <- do.call("rbind", x)
    rownames(x) <- NULL

    # Create a continuous time-variable in seconds
    x$time <- x$iteration * time_step
    return(x)
}