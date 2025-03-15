testthat::test_that("Computing the MLL works", {
    # Get the data to be used
    data <- readRDS(file.path("data", "data_mll.Rds"))
    data <- data[!is.na(data$ps_speed), ]

    # Extract the simulating parameters
    params <- predped::params_from_csv[["params_archetypes"]][1, -(1:2)]
    bounds <- predped::params_from_csv[["params_bounds"]]

    # Use the bounds to make some other parameters
    params_min <- as.data.frame(t(bounds[,1]))
    params_max <- as.data.frame(t(bounds[,2]))
    params_25 <- as.data.frame(t(bounds[,1] + 0.25 * (bounds[,2] - bounds[,1])))
    params_75 <- as.data.frame(t(bounds[,1] + 0.75 * (bounds[,2] - bounds[,1])))

    params_min$b_current_direction <- 1
    params_25$b_current_direction <- 1
    params_75$b_current_direction <- 1
    params_max$b_current_direction <- 1

    params_min$a_current_direction <- 2
    params_25$a_current_direction <- 2
    params_75$a_current_direction <- 2
    params_max$a_current_direction <- 2

    # Compute the min-log-likelihood for each of the parameters
    best <- predped::mll(data, params, transform = FALSE, summed = TRUE)

    min <- predped::mll(data, params_min, transform = FALSE, summed = TRUE)
    q25 <- predped::mll(data, params_25, transform = FALSE, summed = TRUE)
    q75 <- predped::mll(data, params_75, transform = FALSE, summed = TRUE)
    max <- predped::mll(data, params_max, transform = FALSE, summed = TRUE)
    
    # Ideally, the mll of the generating parameters should be lower than the 
    # mll of other parameters. Test this assumption here.
    testthat::expect_true(all(best <= min))
    testthat::expect_true(all(best <= q25))
    testthat::expect_true(all(best <= q75))
    testthat::expect_true(all(best <= max))
})

testthat::test_that("Computing the MLL with conversion works", {
    # Get the data to be used
    data <- readRDS(file.path("data", "data_mll.Rds"))
    data <- data[!is.na(data$ps_speed), ]

    # Extract the simulating parameters
    params <- predped::params_from_csv[["params_archetypes"]][1, -(1:2)]
    bounds <- predped::params_from_csv[["params_bounds"]]

    # Use the bounds to make some other parameters
    params_min <- as.data.frame(t(bounds[,1]))
    params_max <- as.data.frame(t(bounds[,2]))
    params_25 <- as.data.frame(t(bounds[,1] + 0.25 * (bounds[,2] - bounds[,1])))
    params_75 <- as.data.frame(t(bounds[,1] + 0.75 * (bounds[,2] - bounds[,1])))

    params_min$b_current_direction <- 1
    params_25$b_current_direction <- 1
    params_75$b_current_direction <- 1
    params_max$b_current_direction <- 1

    params_min$a_current_direction <- 2
    params_25$a_current_direction <- 2
    params_75$a_current_direction <- 2
    params_max$a_current_direction <- 2

    # Convert all these parameters to their real alternatives
    params <- predped::to_unbounded(params, bounds)
    params_min <- predped::to_unbounded(params_min, bounds)
    params_25 <- predped::to_unbounded(params_25, bounds)
    params_75 <- predped::to_unbounded(params_75, bounds)
    params_max <- predped::to_unbounded(params_max, bounds)

    # Compute the min-log-likelihood for each of the parameters
    best <- predped::mll(data, params, transform = TRUE, summed = TRUE)

    min <- predped::mll(data, params_min, transform = TRUE, summed = TRUE)
    q25 <- predped::mll(data, params_25, transform = TRUE, summed = TRUE)
    q75 <- predped::mll(data, params_75, transform = TRUE, summed = TRUE)
    max <- predped::mll(data, params_max, transform = TRUE, summed = TRUE)
    
    # Ideally, the mll of the generating parameters should be lower than the 
    # mll of other parameters. Test this assumption here.
    testthat::expect_true(all(best <= min))
    testthat::expect_true(all(best <= q25))
    testthat::expect_true(all(best <= q75))
    testthat::expect_true(all(best <= max))
})

testthat::test_that("Likelihood R and Rcpp converge", {
    ############################################################################
    # SUMMED

    # Read in some test data. Of these, select only the first 100, as these 
    # already contain all necessary ingredients (social simulation)
    data <- readRDS(file.path("data", "data_mll.Rds"))
    data <- data[!is.na(data$ps_speed), ]
    data <- data[1:100, ]

    # Retrieve the parameters of the SocialBaselineEuropean
    params <- predped::params_from_csv[["params_archetypes"]]
    params <- params[params$name == "SocialBaselineEuropean", ]

    # Create test and reference and compare both across all datapoints. Here, 
    # we don't transform the parameters
    ref <- predped::mll(data, params, transform = FALSE, cpp = FALSE, summed = TRUE)
    tst <- predped::mll(data, params, transform = FALSE, cpp = TRUE, summed = TRUE)

    testthat::expect_equal(ref, tst)

    # And now do one where you do transform the parameters
    params <- predped::to_unbounded(params, predped::params_from_csv[["params_bounds"]])

    ref <- predped::mll(data, params, transform = TRUE, cpp = FALSE, summed = TRUE)
    tst <- predped::mll(data, params, transform = TRUE, cpp = TRUE, summed = TRUE)

    testthat::expect_equal(ref, tst)



    ############################################################################
    # RAW

    # Read in some test data. Of these, select only the first 100, as these 
    # already contain all necessary ingredients (social simulation)
    data <- readRDS(file.path("data", "data_mll.Rds"))
    data <- data[1:100, ]

    # Retrieve the parameters of the SocialBaselineEuropean
    params <- predped::params_from_csv[["params_archetypes"]]
    params <- params[params$name == "SocialBaselineEuropean", ]

    # Create test and reference and compare both across all datapoints. Here, 
    # we don't transform the parameters
    ref <- predped::mll(data, params, transform = FALSE, cpp = FALSE, summed = FALSE)
    tst <- predped::mll(data, params, transform = FALSE, cpp = TRUE, summed = FALSE)

    testthat::expect_equal(ref, tst)

    # And now do one where you do transform the parameters
    params <- predped::to_unbounded(params, predped::params_from_csv[["params_bounds"]])

    ref <- predped::mll(data, params, transform = TRUE, cpp = FALSE, summed = FALSE)
    tst <- predped::mll(data, params, transform = TRUE, cpp = TRUE, summed = FALSE)

    testthat::expect_equal(ref, tst)
})