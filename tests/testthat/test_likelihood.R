testthat::test_that("Computing the MLL works", {
    # Get the data to be used
    data <- readRDS(file.path("data", "data_mll.Rds"))

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
    best <- predped::mll(data, params)

    min <- predped::mll(data, params_min)
    q25 <- predped::mll(data, params_25)
    q75 <- predped::mll(data, params_75)
    max <- predped::mll(data, params_max)
    
    # Ideally, the mll of the generating parameters should be lower than the 
    # mll of other parameters. Test this assumption here.
    testthat::expect_true(all(best <= min))
    testthat::expect_true(all(best <= q25))
    testthat::expect_true(all(best <= q75))
    testthat::expect_true(all(best <= max))
})
