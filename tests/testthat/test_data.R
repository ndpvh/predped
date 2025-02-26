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
    trace <- readRDS(file.path("data", "trace_mll.Rds"))
    trace <- trace[101:105]

    ref <- predped::unpack_trace(trace, cpp = FALSE)
    tst <- predped::unpack_trace(trace, cpp = TRUE)

    testthat::expect_equal(tst, ref)

    # Check for the datasets in which NAs exist.
    trace <- readRDS(file.path("data", "trace_example.Rds"))

    ref <- predped::unpack_trace(trace, cpp = FALSE)
    tst <- predped::unpack_trace(trace, cpp = TRUE)

    testthat::expect_equal(tst, ref)
})
