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
                                    goal_id = current_goal(a)@id,
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
                                                  goal_id = current_goal(a)@id,
                                                  goal_x = current_goal(a)@position[1], 
                                                  goal_y = current_goal(a)@position[2],
                                                  radius = radius(a))

                        # If the agent is not moving, then you cannot compute 
                        # the utility variables. We should therefore fill it 
                        # with values that make sense and otherwise with NULLs
                        # (in hopes utility will be okay with this).
                        #
                        # If the agent is moving, however, we will compute the 
                        # utility variables for that move.
                        if(status(a) != "move") {
                            utility_variables <- data.frame(agent_idx = which(agent_specifications$id == id(a)),
                                                            ps_speed = NA, 
                                                            ps_distance = NA, 
                                                            gd_angle = NA, 
                                                            id_distance = NA,
                                                            id_check = NA,
                                                            id_ingroup = NA,
                                                            ba_angle = NA,
                                                            fl_leaders = NA,
                                                            wb_buddies = NA,
                                                            gc_distance = NA,
                                                            gc_radius = NA, 
                                                            gc_nped = NA,
                                                            vf_angles = NA)

                        } else {
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
                        }
    
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

#' Transform data to a trace
#' 
#' This function does the opposite of \code{\link[predped]{unpack_trace}}. It
#' takes in a data.frame and return a trace according to \code{predped}s 
#' requirements. The data.frame should at least have the column names "x", "y", 
#' "time", and "id", containing the coordinates, times at which the data were 
#' gathered (in seconds), and the id-number of the person whose data it is.
#' Additionally, dataframe needs information on the goals that agents were 
#' doing
#' 
#' @param data Instance of a data.frame containing the data you want to transform.
#' @param background Instance of the \code{\link[predped]background-class} 
#' containing the setting in which the data were gathered.
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
#' @rdname to_trace
#' 
#' @export
to_trace <- function(data, 
                     background,
                     velocities = c(1.5, 1, 0.5) |>
                        rep(each = 11) |>
                        matrix(ncol = 3),
                     orientations = c(72.5, 50, 32.5, 20, 10, 0, 
                                      350, 340, 327.5, 310, 287.5) |>
                         rep(times = 3) |>
                         matrix(ncol = 3),
                     stay_stopped = TRUE,
                     time_step = 0.5) {

    # Define the times at which the simulation ran and define the bins and 
    # iterations that come with it
    time_max <- diff(range(data$time))

    steps <- seq(0, time_max, by = time_step)
    iterations <- seq(0, length(steps), by = 1)

    # Get the unique individuals in the data so that you can loop over them.
    agents <- unique(data$id)
    per_agent <- list()
    for(i in agents) {
        # Select the data for this agent
        agent_data <- data[data$id == i, ]

        browser()

        # Approximate the positions of the agent by taking a mean for every 
        # binned interval the length of "time_step".
        positions <- sapply(
            2:length(steps), 
            function(i) {
                idx <- data$time < steps[i] & data$time >= steps[i - 1]
                return(data.frame(
                    "iteration" = iterations[i - 1],
                    "time" = steps[i - 1],
                    "x" = mean(data$x[idx]),
                    "y" = mean(data$y[idx]),
                    "goal_id" = data$goal_id[idx][1],
                    "goal_x" = data$goal_x[idx][1],
                    "goal_y" = data$goal_y[idx][1]
                ))
            }
        )
        positions <- as.data.frame(t(positions))

        # Create a speed and orientation vector for these data

    }

}