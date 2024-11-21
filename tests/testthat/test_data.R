testthat::test_that("Transforming to time series works", {
    trace <- readRDS(file.path(".", "data", "example_trace.Rds"))
    ref <- data.table::fread(file.path(".", "data", "example_time_series.csv"), 
                             data.table = FALSE)

    tst <- predped::time_series(trace)

    testthat::expect_equal(ref, tst)
})