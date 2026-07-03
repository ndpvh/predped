################################################################################
# STARTING WITH A TRACE
################################################################################

#' Transform trace to time-series
#'
#' @param trace Object of the \code{\link[predped]{trace-class}}
#' @param cpp Logical denoting whether to use the Rcpp (\code{TRUE}) or R
#' (\code{FALSE}) version of this function. Defaults to \code{TRUE}.
#'
#' @examples
#' # This is my example
#'
#' @rdname time_series
#'
#' @concept data
#'
#' @export
time_series <- function(trace,
                        cpp = TRUE) {

    if(cpp) {
        return(time_series_rcpp(trace))
    }

    # Extract the time step from the trace
    time_step <- trace@time_step

    # Create a function that will extract all details of the agents from a
    # particular state.
    extract_state <- function(agents, iteration) {
        y <- lapply(agents,
                    \(a) data.frame(iteration = iteration,
                                    time = iteration * time_step,
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
    x <- lapply(seq_along(trace@states), 
                function(i) extract_state(trace@states[[i]], i - 1))
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
#' @param trace List of objects of the \code{\link[predped]{trace-class}}
#' @param velocities Numeric matrix containing the change in speed for an agent
#' whenever they move to the respective cell of this matrix. Is used to create
#' the cell positions that the agent might move to. Defaults to a matrix in
#' which the columns contain \code{1.5} (acceleration), \code{1} (maintenance
#' of speed), and \code{0.5} (deceleration).
#' @param orientations Numeric matrix containing the change in direction for an
#' agent whenever they move to the respective cell of this matrix. Is used to
#' create the cell positions that the agent might move to. Defaults to a matrix
#' in which the rows contain \code{72.5}, \code{50}, \code{32.5}, \code{20},
#' \code{10}, \code{0}, \code{350}, \code{340}, \code{327.5}, \code{310},
#' \code{287.5} (note that the larger angles are actually the negative symmetric
#' versions of the smaller angles).
#' @param stay_stopped Logical denoting whether agents will predict others that
#' are currently not moving to remain immobile in the next iteration. Defaults
#' to \code{TRUE}.
#' @param cpp Logical denoting whether to use the Rcpp (\code{TRUE}) or R
#' (\code{FALSE}) version of this function. Defaults to \code{TRUE}.
#'
#' @examples
#' # This is my example
#'
#' @rdname unpack_trace
#'
#' @concept data
#'
#' @export
unpack_trace <- function(trace,
                         velocities = c(1.5, 1, 0.5) |>
                            rep(each = 11) |>
                            matrix(ncol = 3),
                         orientations = c(72.5, 50, 32.5, 20, 10, 0,
                                          350, 340, 327.5, 310, 287.5) |>
                             rep(times = 3) |>
                             matrix(ncol = 3),
                         stay_stopped = TRUE,
                         cpp = TRUE) {

    # If Rcpp alternative requested, then let them use it
    if(cpp) {
        return(unpack_trace_rcpp(trace,
                                 velocities,
                                 orientations,
                                 stay_stopped))
    }

    # Extract the time step from the trace
    time_step <- trace@time_step

    # Create a function that will extract all details of the agents from a
    # particular state.
    extract_state <- function(agents, iteration) {
        # Loop over all of the agents and create their own row in the dataframe.
        # This will consist of all variables included in the time_series function
        # and the utility variables that are used as an input to the utility
        # functions.
        y <- lapply(agents,
                    function(a) {
                        # Simple time-series such as the one defined in the
                        # designated function
                        time_series <- data.frame(iteration = iteration,
                                                  time = iteration * time_step,
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
    x <- lapply(seq_along(trace@states), 
                function(i) extract_state(trace@states[[i]], i - 1))
    x <- do.call("rbind", x)
    rownames(x) <- NULL

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
#' @param background Instance of the \code{\link[predped]{background-class}}
#' containing the setting in which the data were gathered.
#' @param b_turning,a_turning Numeric denoting the values of the parameters
#' \eqn{b} and \eqn{a} for the relationship between orientation and velocity.
#' For more information, see the documentation of
#' \code{\link[predped]{compute_centers}}. Defaults to \code{NULL}, meaning that
#' this relationship takes on the default values of \code{predped}.
#' @param velocities Numeric vector denoting the changes in speeds as assumed by
#' the M4MA. Defaults to \code{1.5} (acceleration), \code{1}, and \code{0.5}
#' (deceleration).
#' @param orientations Numeric vector denoting the changes in orientation as
#' assumed by the M4MA. Defaults to \code{72.5}, \code{50}, \code{32.5},
#' \code{20}, \code{10}, \code{0}, \code{350}, \code{340}, \code{327.5},
#' \code{310}, \code{287.5} (note that the larger angles are actually the
#' negative symmetric versions of the smaller angles).
#' @param time_step Numeric denoting the time between each iteration. Defaults
#' to \code{NULL}, in which case the time step is derived by the 
#' \code{\link[predped]{get_time_step}} function with summarizing function 
#' \code{fx}.
#' @param standing_start Numeric denoting the speed below which the cell is
#' set to \code{0} (stopped). Matches the \code{standing_start} parameter in
#' \code{\link[predped]{update_position}}. Defaults to \code{0.25}.
#' @param stay_stopped Logical denoting whether agents will predict others that
#' are currently not moving to remain immobile in the next iteration. Is needed
#' to compute the utility variables accurately. Defaults to \code{TRUE}.
#' @param fx A summarizing function that should be used to derive the time step
#' if it is not provided through the \code{time_step} argument. 
#' @param cpp Logical denoting whether to use the Rcpp (\code{TRUE}) or R
#' (\code{FALSE}) version of this function. Defaults to \code{TRUE}.
#' @param ... Arguments passed to \code{\link[predped]{find_path}}.
#' 
#' @return Object of the \code{\link[predped]{trace-class}}.
#'
#' @examples
#' # This is my example
#'
#' @rdname to_trace
#'
#' @concept data
#'
#' @export
to_trace <- function(data,
                     background,
                     b_turning = NULL,
                     a_turning = NULL,
                     velocities = c(1.5, 1, 0.5),
                     orientations = c(72.5, 50, 32.5, 20, 10, 0,
                                      -10, -20, -32.5, -50, -72.5),
                     time_step = NULL,
                     standing_start = 0.25,
                     stay_stopped = TRUE,
                     fx = mean,
                     cpp = TRUE,
                     ...) {

    # If the time step is not provided by the user, use the average time between
    # each observation, averaging within participants and across participants.
    # Note that this may not be realistic when there is a great deviation in the 
    # sampling rate over time.
    if(is.null(time_step)) {
        time_step <- get_time_step(data, fx = fx)
    }

    # Check whether a_turning and b_turning are specified. If not, take on the 
    # defaults of predped
    if(is.null(a_turning)) {
        a_turning <- 2
    }
    if(is.null(b_turning)) {
        b_turning <- 0.2
    }

    # Save group mapping before add_motion_variables: it rebuilds positions from
    # scratch (via sapply), dropping every column not explicitly included — group
    # among them.  If we don't save it here, to_trace always falls through to the
    # factor(id) fallback below, producing a different numbering.
    if ("group" %in% colnames(data)) {
        first_occ <- !duplicated(data$id)
        group_map <- list(id = data$id[first_occ],
                          group = data$group[first_occ])

    } else {
        group_map <- NULL
    }

    # Add the information needed to transform the data to a collection of states.
    data <- add_motion_variables(
        data,
        velocities = velocities,
        orientations = orientations,
        time_step = time_step,
        standing_start = standing_start,
        initial_conditions = TRUE,
        a_turning = a_turning,
        b_turning = b_turning
    )

    # Change the speed of the agents when they fall below the threshold of the 
    # standing_start. In actual data, it's useful to keep it at 0, but once you 
    # want to go back to a trace, we need to make it predped-foolproof. We also
    # need to do this for speed0, as this is what the computation of the centers
    # is based on
    data$speed[data$speed < standing_start] <- standing_start
    data$speed0[data$speed0 < standing_start] <- standing_start

    # Assign each person to a group: If provided in the data, use that value.
    # If not, make a new one.  Use named-vector lookup to avoid merge() which
    # reorders rows and breaks the iteration loop below.
    if (!is.null(group_map)) {
        idx <- match(as.character(data$id), as.character(group_map$id))
        data$group <- as.numeric(group_map$group[idx])
    } else if (!("group" %in% colnames(data))) {
        data$group <- data$id |>
            factor() |>
            as.numeric()
    }

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

    # Make sure agent_specifications is defined
    agent_specifications <- NULL

    # Loop over each of the iterations and add the agents to the states of the
    # trace.
    output <- trace(setting = background, 
                    time_step = time_step, 
                    states = list())
    N <- max(data$iteration)
    for(i in seq_len(N)) {
        # Select the data for that iteration
        iter_data <- data[data$iteration == i, ]

        # If no data is available, then we cannot add any agents to the current
        # iteration. Otherwise, we can continue
        if(nrow(iter_data) == 0) {
            output@states[[i]] <- list()
            next
        }

        # If there are agents walking around at that time, we create an agents
        # list and add it to the state. Add the agents that were already in the
        # room to the dummy state, will allow us to be more accurate in checks
        # etc.
        if(i > 1) {
            dummy_state@agents <- output@states[[i - 1]]
        }

        output@states[[i]] <- lapply(
            seq_len(nrow(iter_data)),
            function(j) {
                # General agent characteristics
                dummy_agent@id <- iter_data$id[j]
                dummy_agent@center <- as.numeric(iter_data[j, c("x", "y")])
                dummy_agent@speed <- iter_data$speed[j]
                dummy_agent@orientation <- iter_data$orientation[j]
                dummy_agent@cell <- iter_data$cell[j]
                dummy_agent@group <- iter_data$group[j]
                dummy_agent@status <- iter_data$status[j]

                # Goal characteristics
                dummy_goal@id <- iter_data$goal_id[j]
                dummy_goal@position <- coordinate(as.numeric(iter_data[j, c("goal_x", "goal_y")]))
                dummy_goal@path <- find_path(dummy_goal,
                                             dummy_agent,
                                             background,
                                             ...)

                dummy_agent@current_goal <- dummy_goal

                # Cell centers
                copy <- dummy_agent

                copy@center <- as.numeric(iter_data[j, c("x0", "y0")])
                copy@speed <- as.numeric(iter_data$speed0[j])
                copy@orientation <- as.numeric(iter_data$orientation0[j])

                if(!is.null(b_turning)) {
                    copy@parameters$b_turning <- if (length(b_turning) > 1)
                        b_turning[[as.character(iter_data$id[j])]] else b_turning
                }
                if(!is.null(a_turning)) {
                    copy@parameters$a_turning <- if (length(a_turning) > 1)
                        a_turning[[as.character(iter_data$id[j])]] else a_turning
                }

                dummy_agent@cell_centers <- compute_centers(copy,
                                                            velocities = velocities |>
                                                                rep(each = length(orientations)) |>
                                                                matrix(ncol = 3),
                                                            orientations = orientations |>
                                                                rep(times = length(velocities)) |>
                                                                matrix(ncol = 3),
                                                            time_step = time_step,
                                                            cpp = cpp)

                # If possible, also compute an agent's utility variables. Only
                # possible if one is able to predict the other's movements
                #
                # Note that we use the copy for this computation. Is done to
                # ensure that the utility variables are computed while accounting
                # for the previous, not the current state
                if(!is.null(agent_specifications) & copy@status == "move") {
                    # You can only include those agents that are actually in the
                    # specifications. If not included, we cannot include them in
                    # the computation (this is the case if this is the first
                    # iteration that the agent is present in the room)
                    if(id(copy) %in% agent_specifications$id) {
                        # Perform a preliminary check of the different cell positions
                        # and whether an agent can move there.
                        #
                        # Importantly, assumed that almost all positions can be moved
                        # to (except for those blocked by an object):
                        #
                        # Reasoning is that we wish to estimate a model and that we
                        # don't know exactly which cell positions are blocked, even
                        # not when we simulated the data (updating happens sequentially,
                        # but it is not certain in which sequence)
                        check <- moving_options(copy,
                                                dummy_state,
                                                background,
                                                dummy_agent@cell_centers,
                                                cpp = cpp)

                        # Compute the utility variables themselves
                        uv <- compute_utility_variables(copy,
                                                        dummy_state,
                                                        background,
                                                        agent_specifications,
                                                        dummy_agent@cell_centers,
                                                        check,
                                                        cpp = FALSE)
                        dummy_agent@utility_variables <- uv
                    }
                }

                # Update the list of agents. Obsolete if we would implement random
                # order updating of agents
                dummy_state@agents[[j]] <- dummy_agent

                return(dummy_agent)
            }
        )

        # Update agent_specifications
        agent_specifications <- create_agent_specifications(output@states[[i]],
                                                            stay_stopped = stay_stopped,
                                                            time_step = output@time_step,
                                                            cpp = cpp)
    }
    
    return(output)
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
#' the M4MA. Defaults to \code{1.5} (acceleration), \code{1}, and \code{0.5}
#' (deceleration).
#' @param orientations Numeric vector denoting the changes in orientation as
#' assumed by the M4MA. Defaults to \code{72.5}, \code{50}, \code{32.5},
#' \code{20}, \code{10}, \code{0}, \code{350}, \code{340}, \code{327.5},
#' \code{310}, \code{287.5} (note that the larger angles are actually the
#' negative symmetric versions of the smaller angles).
#' @param time_step Numeric denoting the time between each iteration. Defaults
#' to \code{NULL}, in which case the time step is derived by the 
#' \code{\link[predped]{get_time_step}} function with summarizing function 
#' \code{fx}.
#' @param standing_start Numeric denoting the speed below which the cell is
#' set to \code{0} (stopped). Matches the \code{standing_start} parameter in
#' \code{\link[predped]{update_position}}. Defaults to \code{0.25}.
#' @param initial_conditions Logical denoting whether the added columns should
#' include the initial conditions (that is, speed, orientation, and position at
#' the previous time point) alongside their current alternatives. Useful when
#' one wants to compute the values of the utility-related variables from the
#' data. Defaults to \code{FALSE}.
#' @param fx A summarizing function that should be used to derive the time step
#' if it is not provided through the \code{time_step} argument. 
#'
#' @return A data.frame with predped-relevant motion variables derived from the 
#' provided data, including the speeds and orientations, cells that the agents 
#' moved to, the status of the agents, and the original positions, ids, and goals
#' that were included in the original dataset.
#' 
#' @examples
#' # Create a minimal working example dataset from scratch
#' data <- data.frame(
#'   "time" = seq(0, 10, 1),
#'   "id" = rep("participant", 11),
#'   "x" = cos(seq(0, 2 * pi, length.out = 11)),
#'   "y" = sin(seq(0, 2 * pi, length.out = 11)),
#'   "goal_id" = rep("goal", 11),
#'   "goal_x" = rep(0, 11),
#'   "goal_y" = rep(0, 11)
#' )
#' 
#' # Add the motion variables to this dataset
#' add_motion_variables(data)
#' 
#'
#' @rdname add_motion_variables
#'
#' @concept data
#'
#' @export
add_motion_variables <- function(data,
                                 velocities = c(1.5, 1, 0.5),
                                 orientations = c(72.5, 50, 32.5, 20, 10, 0,
                                                  -72.5, -50, -32.5, -20, -10),
                                 time_step = NULL,
                                 standing_start = 0.25,
                                 initial_conditions = FALSE,
                                 a_turning = 2,
                                 b_turning = 0.2, 
                                 fx = mean) {

    # Check whether all needed columns are part of the data.frame already. If so, 
    # then we move on immediately
    if(initial_conditions) {
        cols <- c("iteration", "time", "id", "x", "y", "speed", "orientation", 
                  "speed0", "orientation0", "x0", "y0", "cell", "group", 
                  "status", "goal_id", "goal_x", "goal_y", "radius")

    } else {
        cols <- c("iteration", "time", "id", "x", "y", "speed", "orientation", 
                "cell", "group", "status", "goal_id", "goal_x", "goal_y", "radius")
    }

    if(all(cols %in% colnames(data))) {
        return(data)
    }

    # If the time step is not provided by the user, use the average time between
    # each observation, averaging within participants and across participants.
    # Note that this may not be realistic when there is a great deviation in the 
    # sampling rate over time.
    if(is.null(time_step)) {
        time_step <- get_time_step(data, fx = fx)
    }

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
                    "iteration" = iterations[j],
                    "time" = steps[j],
                    "id" = i,
                    "x" = mean(agent_data$x[idx], na.rm = TRUE),
                    "y" = mean(agent_data$y[idx], na.rm = TRUE),
                    "goal_id" = agent_data$goal_id[idx][1],
                    "goal_x" = agent_data$goal_x[idx][1],
                    "goal_y" = agent_data$goal_y[idx][1]
                ))
            }
        )
        positions <- as.matrix(positions) |>
            t() |>
            as.data.frame()

        # Remove all NAs: Imposed in the sapply above by assuming all agents 
        # have been in the data at all times. If not taken care of, the NAs 
        # will propagate
        positions <- positions[!is.na(positions$goal_id), ]

        # Change all numerics to numeric
        for(j in c("iteration", "time", "x", "y", "goal_x", "goal_y")) {
            positions[, j] <- as.numeric(positions[, j])
        }

        # Add ending positions to the data. This will allow us to define the
        # initial position, speed, orientation, and ending position at each
        # time point
        positions[, c("x0", "y0")] <- rbind(
            matrix(NA, nrow = 1, ncol = 2),
            positions[2:nrow(positions) - 1, c("x", "y")] |>
                as.matrix()
        )
        positions <- positions[-1, ]

        # Create a speed and orientation vector for these data. The speed is
        # defined as the distance traveled between two consecutive iterations
        # divided by the time step. The orientation is defined as the angle
        # between two consecutive positions in the data. This angle is then
        # made positive and transformed to degrees
        positions$speed <- sqrt((positions$x - positions$x0)^2 + (positions$y - positions$y0)^2) / time_step

        positions$orientation <- atan2(
            positions$y - positions$y0,
            positions$x - positions$x0
        )
        positions$orientation <- ifelse(
            positions$orientation < 0,
            positions$orientation + 2 * pi,
            positions$orientation
        )
        positions$orientation <- positions$orientation * 180 / pi

        # For the orientations, agents are always assumed to have the orientation 
        # 0 when they don't move, as assumed by the atan2 function. However, this 
        # is unlikely. Unfortunately, however, we have no way of going from data 
        # to an informed guess of what the actual orientation may be (e.g., are 
        # agents looking around?), so our actual best guess is to keep their 
        # previous orientation. 
        #
        # Additionally, we want to make a guess as to what the agent is doing at 
        # a particular time, differentiating between moving and waiting/completing
        # a goal/... For this, the current speed needs to be equal to the 
        # standing_start as well as at least one of the speeds next to it. If
        # so, we'll add a "wait" status. Note that this may not reflect what the 
        # agent is actually doing, but this shouldn't matter for our purposes 
        # (that is, deriving utilities from data).
        #
        # Perform both adjustments in a loop.
        check <- logical(nrow(positions))
        for(j in seq_len(nrow(positions))) {
            # Adjustmemt of the orientation
            if(is.na(positions$speed[j])) {
                next
            }

            if((positions$speed[j] == 0) & (j != 1)) {
                positions$orientation[j] <- positions$orientation[j - 1]
            }

            # Adjustment of the status of the agent
            idx <- seq(j - 1, j + 1, by = 1)
            summed <- sum(positions$speed[idx] == 0, na.rm = TRUE)
            check[j] <- (summed >= 2) & (positions$speed[j] == 0)
        }
        positions$status <- ifelse(check, "wait", "move")

        # Adjust so you have initial speeds and orientations coupled to initial
        # positions. Is needed in order to accurately compute cell centers
        positions$speed0 <- c(NA, positions$speed[2:nrow(positions) - 1])
        positions$orientation0 <- c(NA, positions$orientation[2:nrow(positions) - 1])

        # Make some derived changes in speeds and orientation. These will combine
        # into the cells that are chosen. Make sure that the difference in
        # orientation falls within (-180, 180), thus making an angle relative to
        # the current direction
        d_speed <- positions$speed / positions$speed0
        d_orientation <- positions$orientation - positions$orientation0
        d_orientation <- ifelse(
            d_orientation > 180,
            d_orientation - 360,
            ifelse(
                d_orientation < -180,
                d_orientation + 360,
                d_orientation
            )
        )

        # Per-agent turning parameters (scalar or named vector)
        a_t <- if (length(a_turning) > 1) a_turning[[as.character(i)]] else a_turning
        b_t <- if (length(b_turning) > 1) b_turning[[as.character(i)]] else b_turning

        # Slowing factor: mirrors compute_centers where the actual displacement
        # is speed0 * slow * vel_ring * dt, so observed d_speed = slow * vel_ring.
        # Dividing by slow recovers the ring-velocity ratio for threshold comparison.
        slow <- pmax(1e-6, 1 - b_t * sin(abs(d_orientation * pi / 180) / 2)^a_t)
        d_speed_adj <- d_speed / slow

        ring <- rowSums(
            cbind(
                rep(TRUE, each = length(d_speed_adj)),  # Outer ring
                d_speed_adj < mean(velocities[1:2]),    # Middle ring
                d_speed_adj < mean(velocities[2:3])     # Inner ring
            )
        )
        cone <- rowSums(
            cbind(
                rep(TRUE, each = length(d_orientation)),
                d_orientation < mean(orientations[1:2]),
                d_orientation < mean(orientations[2:3]),
                d_orientation < mean(orientations[3:4]),
                d_orientation < mean(orientations[4:5]),
                d_orientation < mean(orientations[5:6]),
                d_orientation < mean(orientations[6:7]),
                d_orientation < mean(orientations[7:8]),
                d_orientation < mean(orientations[8:9]),
                d_orientation < mean(orientations[9:10]),
                d_orientation < mean(orientations[10:11])
            )
        )

        cells <- matrix(1:33, nrow = length(orientations), ncol = length(velocities))
        positions$cell <- sapply(
            seq_along(ring),
            \(j) ifelse(
                positions$speed[j] < standing_start,
                0,
                cells[cone[j], ring[j]]
            )
        )

        # Delete rows with NA and add the agent-specific information to the
        # agent list
        positions <- positions[!is.na(positions$cell), ]
        positions <- positions[!is.na(positions$speed0), ]
        per_agent[[i]] <- positions
    }

    # Bind all data together and order according to iterations
    new_data <- do.call("rbind", per_agent)
    new_data <- new_data[order(new_data$iteration), ]

    # If the person does not need initial information, delete the 0-columns
    if(!initial_conditions) {
        new_data <- new_data[, !(colnames(new_data) %in% c("x0", "y0", "speed0", "orientation0"))]
    }

    return(new_data)
}
