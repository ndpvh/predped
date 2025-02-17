testthat::test_that("Utility R and Rcpp converge", {
    # Read in some test data. Of these, select only the first 100, as these 
    # already contain all necessary ingredients (social simulation)
    data <- readRDS(file.path("data", "data_utility.Rds"))
    data <- data[1:100, ]

    # Retrieve the parameters of the SocialBaselineEuropean
    params <- predped::params_from_csv[["params_archetypes"]]
    params <- params[params$name == "SocialBaselineEuropean", ]

    # Create test and reference and compare both across all datapoints
    result <- logical(100)
    for(i in 1:100) {
        ref <- predped::utility(data[i, ], params, cpp = FALSE)
        tst <- predped::utility(data[i, ], params, cpp = TRUE)

        result[i] <- all(ref == tst)
    }

    testthat::expect_true(all(result))
})