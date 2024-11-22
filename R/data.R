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
                                    time = y@iteration * time_step,
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

    return(x)
}

#' Transform trace to comprehensive data.frame
#' 
#' This function will take a trace and return a data.frame containing all 
#' information contained within a typical time-series (cfr. 
#' \code{\link[predped]{time_series}}) and with all the input that should be 
#' provided to the utility functions. This is therefore the primary function to 
#' use if you want to go from a trace to a data.frame that can be used in 
#' M4MA-based estimations.
#' 
#' @param trace List of objects of the \code{\link[predped]{state-class}}
#' @param velocities Numeric matrix containing the change in speed for an agent
#' whenever they move to the respective cell of this matrix. Is used to create 
#' the cell positions that the agent might move to, as performed through 
#' \code{\link[m4ma]{c_vd_rcpp}}. Currently limited to having 11 rows (direction) 
#' and 3 columns (speed). Defaults to a matrix in which the columns contain 
#' \code{1.5} (acceleration), \code{1}, and \code{0.5}.
#' @param orientations Numeric matrix containing the change in direction for an 
#' agent whenever they move to the respective cell of this matrix. Is used to 
#' create the cell positions that the agent might move to, as performed through
#' \code{\link[m4ma]{c_vd_rcpp}}. Currently limited to having 11 rows (direction)
#' and 3 columns (speed). Defaults to a matrix in which the rows contain 
#' \code{72.5}, \code{50}, \code{32.5}, \code{20}, \code{10}, code{0}, \code{350}, 
#' \code{340}, \code{327.5}, \code{310}, \code{287.5} (note that the larger 
#' angles are actually the negative symmetric versions of the smaller angles).
#' @param stay_stopped Logical denoting whether agents will predict others that 
#' are currently not moving to remain immobile in the next iteration. Defaults 
#' to \code{TRUE}.
#' @param time_step Numeric denoting the time between each iteration. Defaults 
#' to \code{0.5} (the same as in \code{\link[predped]{simulate,predped-method}}).
#' 
#' @examples
#' # This is my example
#'
#' @rdname unpack_trace
#' 
#' @export
unpack_trace <- function(x, 
                         velocities = c(1.5, 1, 0.5) |>
                            rep(each = 11) |>
                            matrix(ncol = 3),
                         orientations = c(72.5, 50, 32.5, 20, 10, 0, 
                                          350, 340, 327.5, 310, 287.5) |>
                             rep(times = 3) |>
                             matrix(ncol = 3),
                         stay_stopped = TRUE,
                         time_step = 0.5) {

    # Create a function that will extract all details of the agents from a 
    # particular state.
    extract_state <- function(y) {
        # Create the agent-specifications for this state
        agent_specifications <- create_agent_specifications(y@agents, 
                                                            stay_stopped = stay_stopped, 
                                                            time_step = time_step)

        # Loop over all of the agents and create their own row in the dataframe.
        # This will consist of all variables included in the time_series function
        # and the utility variables that are used as an input to the utility 
        # functions.
        y <- lapply(y@agents, 
                    function(a) {
                        # Simple time-series such as the one defined in the 
                        # designated function
                        time_series <- data.frame(iteration = y@iteration,
                                                  time = y@iteration * time_step,
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
                                                  radius = radius(a))

                        # Get the centers for this participant, given their 
                        # current position, speed, and orientation
                        centers <- m4ma::c_vd_rcpp(cells = 1:33,
                                                   p1 = position(a),
                                                   v1 = speed(a),
                                                   a1 = orientation(a),
                                                   vels = velocities,
                                                   angles = orientations,
                                                   tStep = time_step)

                        # Do an initial check of which of these centers can be 
                        # reached and which ones can't
                        check <- moving_options(a, y, y@setting, centers)
                        
                        # Compute the utility variables for this agent under the
                        # current state
                        utility_variables <- compute_utility_variables(a,
                                                                       y,
                                                                       y@setting,
                                                                       agent_specifications,
                                                                       centers,                    
                                                                       check)

                        # Bind them all together in one dataframe and return 
                        # the result
                        return(cbind(time_series, utility_variables))
                    })

        return(do.call("rbind", y))
    }

    # Iterate over each object in the list and extract the state. 
    x <- lapply(x, extract_state)
    x <- do.call("rbind", x)
    rownames(x) <- NULL

    # Create a continuous time-variable in seconds
    return(x)
}