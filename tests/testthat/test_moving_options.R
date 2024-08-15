testthat::test_that("Number of pedestrians blocking view from goal works", {
    mock_setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                                   size = c(10, 2)),
                                        objects = list(), 
                                        entrance = predped::coordinate(c(-5, 0)))

    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         current_goal = predped::goal(position = c(4, 0)))

    # Create some blocking and non-blocking pedestrians
    block_1 <- predped::agent(center = c(-2, 0), radius = 0.2)
    block_2 <- predped::agent(center = c(2, 0), radius = 0.2)
    noblock_1 <- predped::agent(center = c(-2, 0.75) , radius = 0.2)
    noblock_2 <- predped::agent(center = c(-2, -0.75) , radius = 0.2)

    # Try several combinations and see whether the correct number of blocking
    # agents are derived. `me` is not included in this list anymore, as the 
    # `state$agents` list that gets passed down to this function only contains
    # the other agents
    state_0a <- list(setting = mock_setting, 
                     agents = list(noblock_1, noblock_2))
    state_0b <- list(setting = mock_setting, 
                     agents = list(noblock_1))
    state_0c <- list(setting = mock_setting, 
                     agents = list(noblock_2))

    state_1a <- list(setting = mock_setting, 
                     agents = list(block_1))
    state_1b <- list(setting = mock_setting, 
                     agents = list(noblock_1, block_1))
    state_1c <- list(setting = mock_setting, 
                     agents = list(noblock_2, block_2))

    state_2a <- list(setting = mock_setting, 
                     agents = list(block_1, block_2))
    state_2b <- list(setting = mock_setting, 
                     agents = list(block_1, noblock_1, block_2))
    state_2c <- list(setting = mock_setting, 
                     agents = list(block_2, noblock_2, block_2))

    # Put them all in a list
    states <- list(state_0a, state_0b, state_0c,
                   state_1a, state_1b, state_1c,
                   state_2a, state_2b, state_2c)

    # Test the function
    tst <- lapply(states, \(x) agents_between_goal(me, x))
    ref <- list(0, 0, 0, 1, 1, 1, 2, 2, 2)

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Identifying moving options works", {
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(10, 2)),
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(5, 1))), 
                                   entrance = predped::coordinate(c(-5, 0)))

    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         speed = 0.1,
                         current_goal = goal(position = c(-2.51, 0)))

    centers <- m4ma::c_vd_rcpp(1:33, 
                               predped::position(me),
                               predped::speed(me),
                               predped::orientation(me),
                               vels = rep(c(1.5, 1, 0.5), each = 11) |>
                                   matrix(ncol = 3),
                               angles = rep(c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 327.5, 310, 287.5), times = 3) |>
                                   matrix(ncol = 3))
    centers[1:9,] <- rbind(# Overlap with objects
                           c(0, 0), 
                           c(-2.5, 0), 
                           c(2.5, 0), 
                           # Unseen
                           c(0, 0.75), 
                           c(0, -0.75), 
                           c(3, 0),
                           # Okay
                           c(-4, 0),
                           c(-4, -0.75),
                           c(-4, 0.75))

    tst <- predped::moving_options(me, 
                                   list(setting = setting, 
                                        agents = list()),
                                   setting,
                                   centers)[1:9]

    ref <- c(rep(FALSE, times = 6), rep(TRUE, times = 3))

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Identifying moving options with other agents around works", {
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(10, 2)),
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(5, 1))), 
                                   entrance = predped::coordinate(c(-5, 0)))

    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         speed = 0.1,
                         current_goal = goal(position = c(-2.51, 0)))

    others <- list(predped::agent(center = c(-3, 0.5), 
                                  radius = 0.2), 
                   predped::agent(center = c(-3.5, 0),
                                  radius = 0.2))

    centers <- m4ma::c_vd_rcpp(1:33, 
                               predped::position(me),
                               predped::speed(me),
                               predped::orientation(me),
                               vels = rep(c(1.5, 1, 0.5), each = 11) |>
                                   matrix(ncol = 3),
                               angles = rep(c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 327.5, 310, 287.5), times = 3) |>
                                   matrix(ncol = 3))
    centers[1:9,] <- rbind(# Overlap with other agents
                           c(-3.5, 0), 
                           c(-3, 0.5), 
                           c(-3.15, 0.5), 
                           # Unseen
                           c(0, 0.75), 
                           c(0, -0.75), 
                           c(3, 0),
                           # Okay
                           c(-4, 0),
                           c(-4, -0.75),
                           c(-4, 0.75))

    tst <- predped::moving_options(me, 
                                   list(setting = setting, 
                                        agents = others),
                                   setting,
                                   centers)[1:9]

    ref <- c(rep(FALSE, times = 6), rep(TRUE, times = 3))

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Overlap with objects works", {
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(10, 2)),
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(5, 1))), 
                                   entrance = predped::coordinate(c(-5, 0)))

    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         speed = 0.1,
                         current_goal = goal(position = c(-2.51, 0)))

    centers <- m4ma::c_vd_rcpp(1:33, 
                               predped::position(me),
                               predped::speed(me),
                               predped::orientation(me),
                               vels = rep(c(1.5, 1, 0.5), each = 11) |>
                                   matrix(ncol = 3),
                               angles = rep(c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 327.5, 310, 287.5), times = 3) |>
                                   matrix(ncol = 3))
    centers[1:6,] <- rbind(# Overlap with objects
                           c(0, 0),     # Interesting case: Agent is completely contained within object, and is not checked as such
                           c(-2.5, 0), 
                           c(2.5, 0),
                           # Okay
                           c(-4, 0),
                           c(-4, -0.75),
                           c(-4, 0.75))

    tst <- predped::overlap_with_objects(me,
                                         setting,
                                         centers, 
                                         matrix(TRUE, nrow = 11, ncol = 3))[1:6]

    ref <- c(c(TRUE, FALSE, FALSE), rep(TRUE, times = 3))

    testthat::expect_equal(tst, ref)
})
