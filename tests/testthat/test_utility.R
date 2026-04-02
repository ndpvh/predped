testthat::test_that("Utility data R and Rcpp converge", {
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

        # There is a difference in precision between R and Rcpp. Hence the 
        # leniency in the tolerance. Note that we first check for infinities, 
        # only then we check for finite values
        check_1 <- all(which(ref == -Inf) == which(tst == -Inf))

        diffs <- abs(ref[!is.infinite(ref)] - tst[!is.infinite(tst)])
        check_2 <- all(diffs <= 10^(-10))

        result[i] <- check_1 & check_2
    }

    testthat::expect_true(all(result))
})

testthat::test_that("Utility agent R and Rcpp converge", {
    trace <- readRDS(file.path("data", "trace_utility.Rds"))

    # Loop over 10 of the traces
    result <- logical(10)
    for(i in 1:10) {
        # Retrieve state and agent
        state_i <- trace[[10 + i]]
        agent_i <- state_i@agents[[2]]

        # Create agent specifications in the same way as done in simulate
        agent_specs <- predped::create_agent_specifications(state_i@agents)

        # Create centers and check. For the latter, we need to delete the current
        # agent from the list
        centers <- m4ma::c_vd_rcpp(
            1:33, 
            predped::position(agent_i),
            predped::speed(agent_i),
            predped::orientation(agent_i),
            matrix(rep(c(1.5, 1, 0.5), each = 11), ncol = 3),
            matrix(rep(c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 327.5, 310, 287.5), times = 3), ncol = 3)
        )

        tmp_state <- state_i
        agents(tmp_state) <- agents(tmp_state)[-2]
        check <- predped::moving_options(
            agent_i,
            tmp_state,
            state_i@setting, 
            centers
        )

        # Do the two necessary computations
        data_r <- predped::utility(
            agent_i,
            state_i,
            state_i@setting,
            agent_specs,
            centers,
            check,
            cpp = FALSE
        )

        data_rcpp <- predped::utility(
            agent_i,
            state_i,
            state_i@setting,
            agent_specs,
            centers,
            check,
            cpp = TRUE
        )

        # Do the comparison
        result[i] <- identical(data_r, data_rcpp)
    }

    testthat::expect_true(all(result))
})

testthat::test_that("Compute utility variables agent R and Rcpp converge", {
    trace <- readRDS(file.path("data", "trace_utility.Rds"))

    # Loop over 10 of the traces
    result <- logical(10)
    for(i in 1:10) {
        # Retrieve state and agent
        state_i <- trace[[10 + i]]
        agent_i <- state_i@agents[[2]]

        # Create agent specifications in the same way as done in simulate
        agent_specs <- predped::create_agent_specifications(state_i@agents)

        # Create centers and check. For the latter, we need to delete the current
        # agent from the list
        centers <- m4ma::c_vd_rcpp(
            1:33, 
            predped::position(agent_i),
            predped::speed(agent_i),
            predped::orientation(agent_i),
            matrix(rep(c(1.5, 1, 0.5), each = 11), ncol = 3),
            matrix(rep(c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 327.5, 310, 287.5), times = 3), ncol = 3)
        )

        tmp_state <- state_i
        agents(tmp_state) <- agents(tmp_state)[-2]
        check <- predped::moving_options(
            agent_i,
            tmp_state,
            state_i@setting, 
            centers
        )

        # Do the two necessary computations
        data_r <- predped::compute_utility_variables(
            agent_i,
            state_i,
            state_i@setting,
            agent_specs,
            centers,
            check,
            cpp = FALSE
        )

        data_rcpp <- predped::compute_utility_variables(
            agent_i,
            state_i,
            state_i@setting,
            agent_specs,
            centers,
            check,
            cpp = TRUE
        )

        # Do the comparison
        result[i] <- identical(data_r, data_rcpp)
    }

    testthat::expect_true(all(result))
})
