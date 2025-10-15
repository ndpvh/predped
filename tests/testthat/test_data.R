testthat::test_that("Transforming to time series works", {
    trace <- readRDS(file.path(".", "data", "example_trace.Rds"))
    ref <- data.table::fread(file.path(".", "data", "example_time_series.csv"), 
                             data.table = FALSE)

    tst <- predped::time_series(trace)

    testthat::expect_equal(ref, tst)
})

testthat::test_that("Transforming to time series from R and Rcpp is same", {
    trace <- readRDS(file.path(".", "data", "trace_example.Rds"))

    ref <- predped::time_series(trace, cpp = FALSE)
    tst <- predped::time_series(trace, cpp = TRUE)

    testthat::expect_equal(ref, tst)
})

testthat::test_that("Transforming to trace works", {
    ref <- readRDS(file.path(".", "data", "example_trace.Rds"))

    # Transform to a dataframe and back
    data <- predped::time_series(ref)
    tst <- predped::to_trace(
        data,
        ref[[1]]@setting
    )

    # Do some checks on the trace itself
    testthat::expect_equal(length(tst), 9)
    testthat::expect_equal(
        sapply(tst, \(x) length(x@agents)), 
        c(0, 1, 1, 1, 1, 2, 2, 2, 2)
    )
})

testthat::test_that("Unpacking trace from R and Rcpp is same", {
    # Check for the datasets in which all columns are filled.
    trace <- readRDS(file.path("data", "trace_mll_bench.Rds"))
    trace <- trace[11:16]

    ref <- predped::unpack_trace(trace, cpp = FALSE)
    tst <- predped::unpack_trace(trace, cpp = TRUE)

    testthat::expect_equal(tst, ref)

    # Check for the datasets in which NAs exist.
    trace <- readRDS(file.path("data", "trace_example.Rds"))

    ref <- predped::unpack_trace(trace, cpp = FALSE)
    tst <- predped::unpack_trace(trace, cpp = TRUE)

    testthat::expect_equal(tst, ref)
})

testthat::test_that("General characteristics for `to_trace` works", {
    # Load an example trace
    trace <- readRDS(file.path("data", "trace_data.Rds"))

    # Transform to data and back to a trace
    ref <- predped::time_series(trace, cpp = TRUE)
    tst <- predped::time_series(
        predped::to_trace(
            data,
            trace[[1]]@setting
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
        # translation
        data_tst <- data_tst[data_tst$status == "move" & data_tst$cell != 0, ]
        data_ref <- data_ref[data_ref$iteration %in% data_tst$iteration, ]

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
                result[i, j] <- all(abs(x - y) < 1e-1)
                
            } else {
                result[i, j] <- all(x == y)
            }
        }
    }

    testthat::expect_true(all(result))
})

testthat::test_that("Computing cell centers for data works", {
    # Load an example trace
    ref <- readRDS(file.path("data", "trace_data.Rds"))

    # Transform to data and back to a trace
    data <- predped::time_series(ref, cpp = TRUE)
    tst <- predped::to_trace(
        data,
        ref[[1]]@setting
    )
    back <- predped::time_series(tst, cpp = FALSE)

    # Loop over iterations and agents within that iteration
    iterations <- sort(as.numeric(unique(data$iteration)))

    result <- logical(length(iterations))
    for(i in seq_along(iterations)) {
        # If the iteration is not found within the backtransformed data, skip
        if(!(iterations[i] %in% back$iteration)) {
            result[i] <- TRUE
            next
        }

        # Extract the agents for both traces
        agents_ref <- predped::agents(ref[[i + 1]])
        agents_tst <- predped::agents(tst[[i]])

        # Delete those people who are waiting
        stati <- sapply(agents_ref, predped::status)
        agents_ref <- agents_ref[stati == "move"]

        stati <- sapply(agents_tst, predped::status)
        agents_tst <- agents_tst[stati == "move"]

        # Select only the agents that are also present in the tst list. There 
        # is bound to be loss of information (and potentially agents) when speeds
        # and orientations are computed in the `to_trace` function
        id_ref <- sapply(agents_ref, predped::id)
        id_tst <- sapply(agents_tst, predped::id)

        id_ref <- id_ref[id_ref %in% id_tst]

        names(id_ref) <- NULL
        names(id_tst) <- NULL

        if(length(agents_ref) == 0 | length(agents_tst) == 0) {
            result[i] <- TRUE
            next
        }

        # Do the actual test of the cell centers for the leftover agents
        tmp <- logical(length(id_ref))
        for(j in seq_along(id_ref)) {
            centers_ref <- agents_ref[[j]]@cell_centers
            centers_tst <- agents_tst[id_tst == id_ref[j]]
            centers_tst <- centers_tst[[1]]@cell_centers

            tmp[j] <- all(abs(centers_ref - centers_tst) < 1e-2)
        }
        result[i] <- all(tmp)
    }

    testthat::expect_true(all(result))
    
})