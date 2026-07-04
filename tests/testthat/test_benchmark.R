testthat::test_that("Check whether all variables contain the necessary info for the benchmark", {
    # Load all benchmark variables
    tests <- load_benchmark()
    args <- load_benchmark_arguments()

    specs <- load_benchmark_specifications()
    hier <- specs$hierarchy
    cap <- specs$caption

    # Check whether all functions can be found in the tests, arguments, hierarchy,
    # and captions
    testthat::expect_equal(
        names(tests), 
        names(args)
    )
    testthat::expect_equal(
        names(tests),
        hier |>
            unlist() |>
            as.vector()
    )
    testthat::expect_equal(
        names(tests),
        names(cap)
    )   
})
