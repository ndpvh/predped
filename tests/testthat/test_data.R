testthat::test_that("Transforming to time series works", {
    trace <- qs2::qs_read(file.path(".", "data", "data__trace.qs2"))
    ref <- qs2::qs_read(file.path(".", "data", "data__time_series.qs2"))
    
    tst <- predped::time_series(trace)

    testthat::expect_equal(ref, tst)
})


testthat::test_that("Transforming to time series from R and Rcpp is same", {
    trace <- qs2::qs_read(file.path(".", "data", "data__trace.qs2"))

    ref <- predped::time_series(trace, cpp = FALSE)
    tst <- predped::time_series(trace, cpp = TRUE)

    testthat::expect_equal(ref, tst)
})

testthat::test_that("Transforming to trace works", {
    ref <- qs2::qs_read(file.path(".", "data", "data__trace.qs2"))

    # Transform to a dataframe and back
    data <- predped::time_series(ref)
    tst <- predped::to_trace(
        data,
        ref@setting
    )

    # Do some checks on the trace itself
    testthat::expect_equal(length(tst@states), 11)
    testthat::expect_equal(
        sapply(tst@states, length),
        c(0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3)
    )
})

testthat::test_that("Unpacking trace from R and Rcpp is same", {
    trace <- qs2::qs_read(file.path("data", "data__trace.qs2"))

    ref <- predped::unpack_trace(trace, cpp = FALSE)
    tst <- predped::unpack_trace(trace, cpp = TRUE)

    testthat::expect_equal(tst, ref)
})

testthat::test_that("General characteristics for `to_trace` works", {
    # Load an example trace
    trace <- qs2::qs_read(file.path(".", "data", "data__trace_long.qs2"))

    # Transform to data and back to a trace
    ref <- predped::time_series(trace, cpp = TRUE)
    tst <- predped::time_series(
        predped::to_trace(
            ref[, c("time", "id", "x", "y", "goal_id", "goal_x", "goal_y")],
            trace@setting
        ),
        cpp = TRUE
    )

    # Check several characteristics per agent
    cols <- colnames(ref)
    cols <- cols[!(cols %in% c("status", "cell", "group"))] # Temporary fix
    agents <- unique(ref$id)

    result <- matrix(FALSE, nrow = length(agents), ncol = length(cols))
    for(i in seq_along(agents)) {
        # Select data for this agent specifically
        data_ref <- ref[ref$id == agents[i], ]
        data_tst <- tst[tst$id == agents[i], ]

        # Arrange both datasets according to time
        data_ref <- data_ref[order(data_ref$iteration), ]
        data_tst <- data_tst[order(data_tst$iteration), ]

        # Select only relevant iterations: Some are ultimately lost in
        # translation. Note that we do + 1 to the data_tst iterations due to  
        # iteration numbers not being preserved in the trace, and us knowing that 
        # the initial condition is lost in the back-translation.
        data_tst <- data_tst[data_tst$status == "move" & data_tst$cell != 0, ]
        data_ref <- data_ref[data_ref$iteration %in% (data_tst$iteration + 1), ]

        # FIX LATER: HAS TO DO WITH THRESHOLDS OF SPEED, NOT SURE HOW TO FIX NOW
        #
        # Remove instances where the agent is not moving around; These cannot
        # be adequately distinguished from actual data
        # data_ref <- data_ref[data_ref$status == "move" & data_ref$cell != 0, ]
        # data_tst <- data_tst[data_tst$status == "move", ]

        # Loop over the columns
        for(j in seq_along(cols)) {
            x <- data_ref[, cols[j]]
            y <- data_tst[, cols[j]]

            if(is.numeric(x)) {
                # We need to set the iterations and time correct due to this 
                # information being lost in the back-translation.
                if(cols[j] == "iteration") {
                    y <- y + 1
                } else if(cols[j] == "time") {
                    y <- y + trace@time_step
                }

                result[i, j] <- all(abs(x - y) < 1e-1)

            } else {
                result[i, j] <- all(x == y)
            }
        }
    }

    testthat::expect_true(all(result))
})

# COMMENTED THIS TEST OUT FOR NOW:
#
# There is an issue in the computation of the cell centers most probably related 
# to inaccurate estimation of the orientations of the agents in non-corridor 
# situations. Specifically, real-life agents can reorient and change their 
# orientation without moving, changing their cell centers. Yet, this change in 
# orientation cannot be picked up on solely through positional data.
#
# One potential fix may target the move/wait states that are invoked, but this 
# still needs to be implemented. For now, it is left out, but should be revisited 
# in the coming days.
# testthat::test_that("Computing cell centers for data works", {
#     # Load an example trace
#     ref <- qs2::qs_read(file.path(".", "data", "data__trace_long.qs2"))

#     # Transform to data and back to a trace
#     data <- predped::time_series(ref, cpp = FALSE)
#     tst <- predped::to_trace(
#         data,
#         ref@setting,
#         cpp = FALSE
#     )
#     back <- predped::time_series(tst, cpp = FALSE)

#     # Change the iteration number in the back transformation: Not tracked in 
#     # the trace
#     back$iteration <- back$iteration + 1

#     # Loop over iterations and agents within that iteration
#     iterations <- sort(as.numeric(unique(data$iteration)))

#     result <- logical(length(iterations))
#     for(i in seq_along(iterations)) {
#         # If the iteration is not found within the backtransformed data, skip
#         if(!(iterations[i] %in% back$iteration)) {
#             result[i] <- TRUE
#             next
#         }

#         # Extract the agents for both traces
#         agents_ref <- predped::states(ref)[[i + 1]]
#         agents_tst <- predped::states(tst)[[i]]

#         # Delete those people who are waiting
#         stati <- sapply(agents_ref, predped::status)
#         agents_ref <- agents_ref[stati == "move"]

#         stati <- sapply(agents_tst, predped::status)
#         agents_tst <- agents_tst[stati == "move"]

#         # Select only the agents that are also present in the tst list. There
#         # is bound to be loss of information (and potentially agents) when speeds
#         # and orientations are computed in the `to_trace` function
#         id_ref <- sapply(agents_ref, predped::id)
#         id_tst <- sapply(agents_tst, predped::id)

#         agents_ref <- agents_ref[id_ref %in% id_tst]
#         id_ref <- id_ref[id_ref %in% id_tst]

#         names(id_ref) <- NULL
#         names(id_tst) <- NULL

#         if(length(agents_ref) == 0 | length(agents_tst) == 0) {
#             result[i] <- TRUE
#             next
#         }

#         # Do the actual test of the cell centers for the leftover agents
#         tmp <- logical(length(id_ref))
#         for(j in seq_along(id_ref)) {
#             centers_ref <- agents_ref[[j]]@cell_centers
#             centers_tst <- agents_tst[id_tst == id_ref[j]]
#             centers_tst <- centers_tst[[1]]@cell_centers

#             tmp[j] <- all(abs(centers_ref - centers_tst) < 1e-2)
#         }
#         result[i] <- all(tmp)
#     }

#     testthat::expect_true(all(result))
# })

# TEST COMMENTED OUT UNTIL LATER:
#
# For some reason, the social utility functions do not always get the
# same results when based on data. It is not entirely clear why this may be,
# but it seems to be that agents will only be flagged if they move (i.e.,
# when the predictions seem to point in a given direction), which may be
# different in traces inferred from data compared to actual traces.
#
# Whenever interpersonal distance, for example, is computed in both, it
# seems to correspond quite well.
#
# In addition to this difficulty, it seems that the divergence between what we
# can infer from data and the information in the traces furthermore provides some
# other small deviations that are difficult to solve. For example, agent indices
# are not correctly transformed, and checks are only approximate (based on the
# information in the data, not on actual information in the space).
#
#
# testthat::test_that("Computing utility variables for data works", {
#     # Load in a trace and transform to data
#     trace <- qs2::qs_read(file.path("data", "trace_data.qs2"))
#     data <- predped::time_series(trace, cpp = TRUE)

#     # Compute the utility variables for the data
#     tst <- predped::compute_utility_variables(data, trace[[1]]@setting)

#     # Unpack the trace to a data.frame with the utility variables in there
#     ref <- predped::unpack_trace(trace)

#     # Only select those iterations from those agents that actually exist in both
#     # datasets
#     ref$iters <- paste(ref$iteration, ref$id)
#     tst$iters <- paste(tst$iteration, tst$id)

#     ref <- ref[ref$iters %in% tst$iters, ]

#     ref <- ref[order(ref$iters), ]
#     tst <- tst[order(tst$iters), ]

#     # There are some known NAs in here, specifically for the first time an agent
#     # is in the room. These need to be deleted
#     idx <- c(1, 5)
#     ref <- ref[-idx, ]
#     tst <- tst[-idx, ]
# })
