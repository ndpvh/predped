testthat::test_that("Predict movement works", {
    pedestrians <- list(predped::agent(center = c(0, 0), 
                                       speed = 1, 
                                       orientation = 90,
                                       radius = 0.2),
                        predped::agent(center = c(-1, 0),
                                       speed = 2,
                                       orientation = 0,
                                       radius = 0.2),
                        predped::agent(center = c(1, 0),
                                       speed = 0.5, 
                                       orientation = 180,
                                       radius = 0.2),
                        predped::agent(center = c(0, 0), 
                                       speed = 1, 
                                       orientation = 90, 
                                       radius = 0.2, 
                                       status = "stop"))

    # With time_step = 1, stay_stopped = TRUE
    ref <- list(predped::coordinate(c(0, 1)), 
                predped::coordinate(c(1, 0)), 
                predped::coordinate(c(0.5, 0)),
                predped::coordinate(c(0, 0)))

    tst <- lapply(pedestrians, 
                  \(x) predict_movement(x, time_step = 1))

    testthat::expect_equal(tst, ref)

    # With time_step is the default, stay_stopped = TRUE
    ref <- list(predped::coordinate(c(0, 0.5)), 
                predped::coordinate(c(0, 0)), 
                predped::coordinate(c(0.75, 0)),
                predped::coordinate(c(0, 0)))

    tst <- lapply(pedestrians, 
                  \(x) predict_movement(x))

    testthat::expect_equal(tst, ref)

    # With time_step = 1, stay_stopped = FALSE
    ref <- list(predped::coordinate(c(0, 1)), 
                predped::coordinate(c(1, 0)), 
                predped::coordinate(c(0.5, 0)),
                predped::coordinate(c(0, 1)))

    tst <- lapply(pedestrians, 
                  \(x) predict_movement(x, stay_stopped = FALSE, time_step = 1))

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Predict movement same across R and Rcpp", {
    pedestrians <- list(predped::agent(center = c(0, 0), 
                                       speed = 1, 
                                       orientation = 90,
                                       radius = 0.2),
                        predped::agent(center = c(-1, 0),
                                       speed = 2,
                                       orientation = 0,
                                       radius = 0.2),
                        predped::agent(center = c(1, 0),
                                       speed = 0.5, 
                                       orientation = 180,
                                       radius = 0.2),
                        predped::agent(center = c(0, 0), 
                                       speed = 1, 
                                       orientation = 90, 
                                       radius = 0.2, 
                                       status = "stop"))

    # With time_step = 1, stay_stopped = TRUE
    ref <- lapply(pedestrians,
                  \(x) predict_movement(x, time_step = 1, cpp = FALSE))

    tst <- lapply(pedestrians, 
                  \(x) predict_movement(x, time_step = 1, cpp = TRUE))

    testthat::expect_equal(tst, ref)

    # With time_step is the default, stay_stopped = TRUE
    ref <- lapply(pedestrians,
                  \(x) predict_movement(x, cpp = FALSE))

    tst <- lapply(pedestrians, 
                  \(x) predict_movement(x, cpp = TRUE))

    testthat::expect_equal(tst, ref)

    # With time_step = 1, stay_stopped = FALSE
    ref <- lapply(pedestrians,
                  \(x) predict_movement(x, stay_stopped = FALSE, cpp = FALSE))

    tst <- lapply(pedestrians, 
                  \(x) predict_movement(x, stay_stopped = FALSE, cpp = TRUE))

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Creating agent specifications is the same across R and Rcpp", {
    pedestrians <- list(predped::agent(center = c(0, 0), 
                                       speed = 1, 
                                       orientation = 90,
                                       radius = 0.2),
                        predped::agent(center = c(-1, 0),
                                       speed = 2,
                                       orientation = 0,
                                       radius = 0.2),
                        predped::agent(center = c(1, 0),
                                       speed = 0.5, 
                                       orientation = 180,
                                       radius = 0.2),
                        predped::agent(center = c(0, 0), 
                                       speed = 1, 
                                       orientation = 90, 
                                       radius = 0.2, 
                                       status = "stop"))

    ref <- create_agent_specifications(pedestrians, cpp = FALSE)
    tst <- create_agent_specifications(pedestrians, cpp = TRUE)

    testthat::expect_equal(tst, ref)
})
