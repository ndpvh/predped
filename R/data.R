################################################################################
# STARTING WITH A TRACE
################################################################################

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
                        time_step = 0.5,
                        cpp = TRUE) {

    if(cpp) {
        return(time_series_rcpp(x, time_step))
    }

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
                         time_step = 0.5,
                         cpp = TRUE) {

    # If Rcpp alternative requested, then let them use it
    if(cpp) {
        return(unpack_trace_rcpp(x, 
                                 velocities,
                                 orientations,
                                 stay_stopped,
                                 time_step))
    }

    # Create a function that will extract all details of the agents from a 
    # particular state.
    extract_state <- function(y) {
        # Create the agent-specifications for this state
        agent_specifications <- create_agent_specifications(y@agents, 
                                                            stay_stopped = stay_stopped, 
                                                            time_step = time_step,
                                                            cpp = FALSE)

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

                        # Access the utility variables slot of the agents and 
                        # bind them together with the time_series data
                        return(cbind(time_series, a@utility_variables))
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





################################################################################
# STARTING WITH A DATA.FRAME
################################################################################

#' Transform data to a trace
#' 
#' This function does the opposite of \code{\link[predped]{unpack_trace}}. It
#' takes in a data.frame and return a trace according to \code{predped}s 
#' requirements. The data.frame should at least have the column names "x", "y", 
#' "time", and "id", containing the coordinates, times at which the data were 
#' gathered (in seconds), and the id-number of the person whose data it is.
#' Additionally, data.frame needs information on the goals that agents were 
#' trying to achieve at each timepoint, of which their positions should be 
#' saved under "goal_x" and "goal_y", and their id to "goal_id".
#' 
#' @param data Instance of a data.frame containing the data you want to transform.
#' @param background Instance of the \code{\link[predped]background-class} 
#' containing the setting in which the data were gathered.
#' @param ... Arguments passed to \code{\link[predped]{add_cells}}.
#' 
#' @examples
#' # This is my example
#'
#' @rdname to_trace
#' 
#' @export
to_trace <- function(data, 
                     background,
                     ...) {

    # Add the information needed to transform the data to a collection of states.
    data <- add_motion_variables(data, ...)

    # Create some dummy states and agents. These will be adjusted within the 
    # loop. Reason for making them here and adjusting them later is for speed, 
    # where the creation of a new object would take too long. 
    dummy_state <- state(
        iteration = 0, 
        setting = background
    )
    dummy_agent <- agent(
        center = c(0, 0),
        radius = 0.25
    )
    dummy_goal <- goal(
        position = c(0, 0),
        counter = 1
    )

    # Loop over each of the iterations and add the agents to the states of the 
    # trace.
    trace <- list()
    N <- max(data$iteration)
    for(i in seq_len(N)) {
        # Add state to the trace and adjust the iteration number
        trace[[i]] <- dummy_state
        trace[[i]]@iteration <- i

        # Select the data for that iteration
        iter_data <- data[data$iteration == i, ]

        # If no data is available, then we cannot add any agents to the current 
        # iteration. Otherwise, we can continue
        if(nrow(iter_data) == 0) {
            next
        }

        # If there are agents walking around at that time, we create an agents
        # list and add it to the state
        trace[[i]]@agents <- lapply(
            seq_len(nrow(iter_data)),
            function(j) {
                dummy_agent@id <- iter_data$id[j]
                dummy_agent@center <- as.numeric(iter_data[j, c("x", "y")])
                dummy_agent@speed <- iter_data$speed[j]
                dummy_agent@orientation <- iter_data$orientation[j]
                dummy_agent@cell <- iter_data$cell[j]

                dummy_goal@id <- iter_data$goal_id[j]
                dummy_goal@position <- coordinate(
                    as.numeric(
                        iter_data[j, c("goal_x", "goal_y")]
                    )
                )
                dummy_agent@current_goal <- dummy_goal

                return(dummy_agent)
            }
        )
    }

    return(trace)
}

#' Add motion variables to data 
#' 
#' This function adds several motion variables to an already existing dataset. 
#' These motion variables are then used by \code{predped} to compute utilities, 
#' allowing for estimations in the long run. The variables that are added are 
#' speed, orientation, and the cell to which a person moved (as defined by the
#' M4MA).
#' 
#' The provided dataset should at least have the following columns:
#'  - \code{x}, \code{y}: Coordinates at which a person was standing at a given 
#'    time
#'  - \code{time}: A continuous variable that denotes the time at which the 
#'    measurement took place.
#'  - \code{id}: The identifier given to the person whose position was measured.
#'  - \code{goal_id}: The identifier given to the goal the person had to move 
#'    towards while their position was being measured.
#'  - \code{goal_x}, \code{goal_y}: The position of the goal the person had to 
#'    move to while their position was being measured.
#' 
#' @param data Instance of a data.frame containing the data you want to transform.
#' @param velocities Numeric vector denoting the changes in speeds as assumed by 
#' the M4MA. Defaults to \code{1.5} (acceleration), \code{1}, and \code{0.5}.
#' @param orientations Numeric vector denoting the changes in orientation as 
#' assumed by the M4MA. Defaults to \code{72.5}, \code{50}, \code{32.5}, 
#' \code{20}, \code{10}, code{0}, \code{350}, \code{340}, \code{327.5}, 
#' \code{310}, \code{287.5} (note that the larger angles are actually the 
#' negative symmetric versions of the smaller angles).
#' @param time_step Numeric denoting the time between each iteration. Defaults 
#' to \code{0.5} (the same as in \code{\link[predped]{simulate,predped-method}}).
#' @param treshold Numeric denoting under which observed value for speed the 
#' cell to which an agent has moved should be put to `0`. Defaults to a value 
#' based on the observed measurement error in our system.
#' 
#' @examples
#' # This is my example
#'
#' @rdname add_motion_variables
#' 
#' @export
add_motion_variables <- function(data, 
                                 velocities = c(1.5, 1, 0.5) ,
                                 orientations = c(-72.5, -50, -32.5, -20, -10, 0, 
                                                  10, 20, 32.5, 50, 72.5),
                                 time_step = 0.5,
                                 threshold = qnorm(0.975, 2 * 0.035, 4 * 0.035^4) / time_step) {

    # Define the times at which the simulation ran and define the bins and 
    # iterations that come with it
    time_max <- diff(range(data$time))

    steps <- seq(0, time_max + time_step, by = time_step) + min(data$time)
    iterations <- seq(0, length(steps), by = 1)

    # Get the unique individuals in the data so that you can loop over them.
    agents <- unique(data$id)
    per_agent <- list()
    for(i in agents) {
        # Select the data for this agent
        agent_data <- data[data$id == i, ]

        # Approximate the positions of the agent by taking a mean for every 
        # binned interval the length of "time_step".
        positions <- sapply(
            2:length(steps), 
            function(j) {
                idx <- agent_data$time < steps[j] & agent_data$time >= steps[j - 1]
                return(c(
                    "iteration" = iterations[j - 1],
                    "time" = steps[j - 1],
                    "id" = i,
                    "x" = mean(agent_data$x[idx]),
                    "y" = mean(agent_data$y[idx]),
                    "goal_id" = agent_data$goal_id[idx][1],
                    "goal_x" = agent_data$goal_x[idx][1],
                    "goal_y" = agent_data$goal_y[idx][1]
                ))
            }
        )
        positions <- as.matrix(positions) |>
            t() |>
            as.data.frame()

        # Change all numerics to numeric
        for(j in c("iteration", "time", "x", "y", "goal_x", "goal_y")) {
            positions[, j] <- as.numeric(positions[, j])
        }

        # Create a speed and orientation vector for these data. The speed is 
        # defined as the distance traveled between two consecutive iterations 
        # divided by the time step. The orientation is defined as the angle 
        # between two consecutive positions in the data. This angle is then 
        # made positive and transformed to degrees
        positions$speed <- c(
            NA,
            sqrt(diff(positions$x)^2 + diff(positions$y)^2) / time_step
        )

        positions$orientation <- c(
            NA, 
            atan2(
                positions$y[2:nrow(positions)] - positions$y[2:nrow(positions) - 1],
                positions$x[2:nrow(positions)] - positions$x[2:nrow(positions) - 1]
            )
        )
        positions$orientation <- ifelse(
            positions$orientation < 0,
            positions$orientation + 2 * pi, 
            positions$orientation
        )
        positions$orientation <- positions$orientation * 180 / pi

        # Make some derived changes in speeds and orientation. These will combine
        # into the cells that are chosen. Make sure that the difference in 
        # orientation falls within (-180, 180), thus making an angle relative to 
        # the current direction
        d_speed <- c(
            NA,
            positions$speed[2:nrow(positions) - 1] / positions$speed[2:nrow(positions)]
        )
        d_orientation <- c(NA, diff(positions$orientation))
        d_orientation <- ifelse(
            d_orientation > 180, 
            d_orientation - 360,
            ifelse(
                d_orientation < -180, 
                d_orientation + 360,
                d_orientation
            )
        )

        ring <- rowSums(
            cbind(
                rep(TRUE, each = length(d_speed)),  # Outer ring
                d_speed < mean(velocities[1:2]),    # Middle ring
                d_speed < mean(velocities[2:3])     # Inner ring
            )
        )
        cone <- rowSums(
            cbind(
                rep(TRUE, each = length(d_orientation)), 
                d_orientation > mean(orientations[1:2]),       
                d_orientation > mean(orientations[2:3]),      
                d_orientation > mean(orientations[3:4]),      
                d_orientation > mean(orientations[4:5]),      
                d_orientation > mean(orientations[5:6]),      
                d_orientation > mean(orientations[6:7]),      
                d_orientation > mean(orientations[7:8]),      
                d_orientation > mean(orientations[8:9]),      
                d_orientation > mean(orientations[9:10]),     
                d_orientation > mean(orientations[10:11])
            )   
        )

        cells <- matrix(1:33, nrow = length(orientations), ncol = length(velocities))
        positions$cell <- sapply(
            seq_along(ring), 
            \(j) ifelse(
                positions$speed[j] <= threshold,
                0,
                cells[cone[j], ring[j]]
            )
        )

        # Delete rows with NA and add the agent-specific information to the 
        # agent list
        positions <- positions[!is.na(positions$cell), ]
        per_agent[[i]] <- positions
    }

    # Bind all data together and order according to iterations
    new_data <- do.call("rbind", per_agent)
    new_data <- new_data[order(new_data$iteration), ]

    return(new_data)
}