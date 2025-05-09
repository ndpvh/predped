testthat::test_that("Number of pedestrians blocking view from goal works", {
    # Create a setting in which the agent can wander
    mock_setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                                   size = c(10, 2)),
                                        objects = list(), 
                                        entrance = predped::coordinate(c(-5, 0)))

    # Create an agent with a particular goal. 'moving_options' relies on the 
    # path of the agent to the goal being defined, so add a path "as if" the
    # agent can walk in a straight line
    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         current_goal = predped::goal(position = c(4, 0)))
    current_goal(me)@path <- matrix(current_goal(me)@position, nrow = 1)

    # Create some blocking and non-blocking pedestrians
    block_1 <- predped::agent(center = c(-2, 0), radius = 0.2)
    block_2 <- predped::agent(center = c(2, 0), radius = 0.2)
    noblock_1 <- predped::agent(center = c(-2, 0.75) , radius = 0.2)
    noblock_2 <- predped::agent(center = c(-2, -0.75) , radius = 0.2)

    # Try several combinations and see whether the correct number of blocking
    # agents are derived. `me` is not included in this list anymore, as the 
    # `state$agents` list that gets passed down to this function only contains
    # the other agents
    state_0a <- predped::state(iteration = 1, 
                               setting = mock_setting, 
                               agents = list(noblock_1, noblock_2))
    state_0b <- predped::state(iteration = 1, 
                               setting = mock_setting, 
                               agents = list(noblock_1))
    state_0c <- predped::state(iteration = 1, 
                               setting = mock_setting, 
                               agents = list(noblock_2))

    state_1a <- predped::state(iteration = 1, 
                               setting = mock_setting, 
                               agents = list(block_1))
    state_1b <- predped::state(iteration = 1, 
                               setting = mock_setting, 
                               agents = list(noblock_1, block_1))
    state_1c <- predped::state(iteration = 1, 
                               setting = mock_setting, 
                               agents = list(noblock_2, block_2))

    state_2a <- predped::state(iteration = 1, 
                               setting = mock_setting, 
                               agents = list(block_1, block_2))
    state_2b <- predped::state(iteration = 1, 
                               setting = mock_setting, 
                               agents = list(block_1, noblock_1, block_2))
    state_2c <- predped::state(iteration = 1, 
                               setting = mock_setting, 
                               agents = list(block_2, noblock_2, block_2))

    # Put them all in a list
    states <- list(state_0a, state_0b, state_0c,
                   state_1a, state_1b, state_1c,
                   state_2a, state_2b, state_2c)

    # Test the function
    tst <- lapply(states, \(x) length(agents_between_goal(me, x)))
    ref <- list(0, 0, 0, 1, 1, 1, 2, 2, 2)

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Identifying moving options works", {
    # Create a setting in which to wander
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(10, 2)),
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(5, 1))), 
                                   entrance = predped::coordinate(c(-5, 0)))

    # Create an agent with a particular goal. 'moving_options' relies on the 
    # path of the agent to the goal being defined, so add a path "as if" the
    # agent can walk in a straight line
    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         speed = 0.1,
                         current_goal = goal(position = c(-2.51, 0)))
    current_goal(me)@path <- matrix(current_goal(me)@position, nrow = 1)

    # Create and modify centers so you closely control which centers can be 
    # accessed and which cannot
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
                           c(-4.5, 0),
                           c(-4, -0.75),
                           c(-4, 0.75))

    # Create test and reference
    tst <- predped::moving_options(me, 
                                   predped::state(iteration = 1, 
                                                  setting = setting, 
                                                  agents = list()),
                                   setting,
                                   centers)[1:9]

    ref <- c(rep(FALSE, times = 6), rep(TRUE, times = 3))

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Identifying moving options with other agents around works", {
    # Create a setting in which to wander
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(10, 2)),
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(5, 1))), 
                                   entrance = predped::coordinate(c(-5, 0)))

    # Create an agent with a particular goal. 'moving_options' relies on the 
    # path of the agent to the goal being defined, so add a path "as if" the
    # agent can walk in a straight line
    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         speed = 0.1,
                         current_goal = goal(position = c(-2.51, 0)),
                         color = "cornflowerblue")
    current_goal(me)@path <- matrix(current_goal(me)@position, nrow = 1)

    # Define the other agents in the room
    others <- list(predped::agent(center = c(-3, 0.5), 
                                  radius = 0.2), 
                   predped::agent(center = c(-3.5, 0),
                                  radius = 0.2))

    # Create and modify centers so you closely control which centers can be 
    # accessed and which cannot
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
                           # Okay (no overlap + goal visible)
                           c(-3.5, -0.75),
                           c(-4, -0.75),
                           c(-4, 0.75))

    # Create test and reference
    tst <- predped::moving_options(me, 
                                   predped::state(iteration = 1, 
                                                  setting = setting, 
                                                  agents = others),
                                   setting,
                                   centers)[1:9]

    ref <- c(rep(FALSE, times = 6), rep(TRUE, times = 3))

    testthat::expect_equal(tst, ref)

    # To visualize the test
    # my_state <- predped::state(iteration = 0, 
    #                            setting = setting, 
    #                            agents = append(others, me))
    # plot(my_state) +
    #     ggplot2::geom_point(ggplot2::aes(x = centers[1:9,1], y = centers[1:9,2]), 
    #                         color = "cornflowerblue")
})

testthat::test_that("Identifying moving options is the same across R and Rcpp", {
    # Create a setting in which to wander
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(10, 2)),
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(5, 1))), 
                                   entrance = predped::coordinate(c(-5, 0)))

    # Create an agent with a particular goal. 'moving_options' relies on the 
    # path of the agent to the goal being defined, so add a path "as if" the
    # agent can walk in a straight line
    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         speed = 0.1,
                         current_goal = goal(position = c(-2.51, 0)))
    current_goal(me)@path <- matrix(current_goal(me)@position, nrow = 1)

    # Define the other agents in the room
    others <- list(predped::agent(center = c(-3, 0.5), 
                                  radius = 0.2), 
                   predped::agent(center = c(-3.5, 0),
                                  radius = 0.2))

    # Create and modify centers so you closely control which centers can be 
    # accessed and which cannot
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
                           c(-4.5, 0),
                           c(-4, -0.75),
                           c(-4, 0.75))

    # Create test and reference for moving options without any others
    ref <- predped::moving_options(me, 
                                   predped::state(iteration = 1, 
                                                  setting = setting, 
                                                  agents = list()),
                                   setting,
                                   centers,
                                   cpp = FALSE)

    tst <- predped::moving_options(me, 
                                   predped::state(iteration = 1, 
                                                  setting = setting, 
                                                  agents = list()),
                                   setting,
                                   centers,
                                   cpp = TRUE)

    testthat::expect_equal(tst, ref)

    # Create test and reference for moving options with others
    ref <- predped::moving_options(me, 
                                   predped::state(iteration = 1, 
                                                  setting = setting, 
                                                  agents = others),
                                   setting,
                                   centers,
                                   cpp = FALSE)

    tst <- predped::moving_options(me, 
                                   predped::state(iteration = 1, 
                                                  setting = setting, 
                                                  agents = others),
                                   setting,
                                   centers,
                                   cpp = TRUE)

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Overlap with objects works", {
    # Create a setting in which to wander
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(10, 2)),
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(5, 1))), 
                                   entrance = predped::coordinate(c(-5, 0)))

    # Create an agent with a particular goal. 'moving_options' relies on the 
    # path of the agent to the goal being defined, so add a path "as if" the
    # agent can walk in a straight line
    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         speed = 0.1,
                         current_goal = goal(position = c(-2.51, 0)))
    current_goal(me)@path <- matrix(current_goal(me)@position, nrow = 1)

    # Create and modify centers so you closely control which centers can be 
    # accessed and which cannot
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

    # Create test and reference
    tst <- predped::overlap_with_objects(me,
                                         setting,
                                         centers, 
                                         matrix(TRUE, nrow = 11, ncol = 3))[1:6]

    ref <- c(c(TRUE, FALSE, FALSE), rep(TRUE, times = 3))

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Overlap with objects of R and Rcpp converges", {
    # Create a setting in which to wander
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(10, 2)),
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(5, 1))), 
                                   entrance = predped::coordinate(c(-5, 0)))

    # Create an agent with a particular goal. 'moving_options' relies on the 
    # path of the agent to the goal being defined, so add a path "as if" the
    # agent can walk in a straight line
    me <- predped::agent(center = c(-4, 0), 
                         radius = 0.2, 
                         orientation = 0,
                         speed = 0.1,
                         current_goal = goal(position = c(-2.51, 0)))
    current_goal(me)@path <- matrix(current_goal(me)@position, nrow = 1)

    # Create and modify centers so you closely control which centers can be 
    # accessed and which cannot
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

    # Create test and reference
    ref <- predped::overlap_with_objects(me,
                                         setting,
                                         centers, 
                                         matrix(TRUE, nrow = 11, ncol = 3),
                                         cpp = FALSE)

    tst <- predped::overlap_with_objects(me,
                                         setting,
                                         centers, 
                                         matrix(TRUE, nrow = 11, ncol = 3),
                                         cpp = TRUE)

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Testing edge case; agent cannot walk inside of an object", {
    # Create agent, background, and state
    my_agent <- predped::agent(center = c(0.5, 0), 
                               radius = 0.25, 
                               orientation = 0,
                               current_goal = predped::goal(position = c(-0.1, 0), 
                                                            path = matrix(c(-0.1, 0), nrow = 1)))

    my_background <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                                    size = c(20, 5)),
                                         objects = list(predped::rectangle(center = c(5, 0), 
                                                                           size = c(10, 3))))

    my_state <- predped::state(iteration = 0, 
                               setting = my_background, 
                               agents = list(my_agent))

    # Three sets of centers, ones that fall inside of the object, ones that fall 
    # outside of the object, and ones that combine the two
    inside <- cbind(rep(seq(1, 9, length.out = 11), times = 3),
                     rep(-1:1, each = 11))
    outside <- cbind(rep(- seq(1, 9, length.out = 11), times = 3),
                    rep(-1:1, each = 11))
    combo <- rbind(inside[1:16, ], 
                   outside[1:17, ])

    # Do the test for each of these possibilities, as well as R and Rcpp
    testthat::expect_false(any(predped::moving_options(my_agent, 
                                                       my_state, 
                                                       my_background,
                                                       outside, 
                                                       cpp = FALSE)))
    testthat::expect_false(any(predped::moving_options(my_agent, 
                                                       my_state, 
                                                       my_background,
                                                       outside, 
                                                       cpp = TRUE)))

    testthat::expect_false(any(predped::moving_options(my_agent, 
                                                       my_state, 
                                                       my_background,
                                                       inside, 
                                                       cpp = FALSE)))
    testthat::expect_false(any(predped::moving_options(my_agent, 
                                                       my_state, 
                                                       my_background,
                                                       inside, 
                                                       cpp = TRUE)))

    testthat::expect_false(any(predped::moving_options(my_agent, 
                                                       my_state, 
                                                       my_background,
                                                       combo, 
                                                       cpp = FALSE)))
    testthat::expect_false(any(predped::moving_options(my_agent, 
                                                       my_state, 
                                                       my_background,
                                                       combo, 
                                                       cpp = TRUE)))

    # Visualize them all 
    # predped::plot(my_background) +
    #     predped::plot(my_agent) +
    #     ggplot2::annotate("point", 
    #                       x = outside[, 1],
    #                       y = outside[, 2],
    #                       color = "red") +
    #     ggplot2::annotate("point", 
    #                       x = inside[, 1],
    #                       y = inside[, 2],
    #                       color = "blue") +
    #     ggplot2::annotate("point", 
    #                       x = combo[, 1],
    #                       y = combo[, 2],
    #                       color = "green")
})

testthat::test_that("Testing edge case; agent cannot cross border of an object", {
    # Create agent, background, and state
    my_agent <- predped::agent(center = c(-0.5, 0), 
                               radius = 0.25, 
                               orientation = 0,
                               current_goal = predped::goal(position = c(-0.1, 0), 
                                                            path = matrix(c(-0.1, 0), nrow = 1)))

    my_background <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                                    size = c(20, 5)),
                                         objects = list(predped::rectangle(center = c(5, 0), 
                                                                           size = c(10, 3))))

    my_state <- predped::state(iteration = 0, 
                               setting = my_background, 
                               agents = list(my_agent))

    # Three sets of centers, ones that fall inside of the object, ones that fall 
    # outside of the object, and ones that combine the two
    outside <- cbind(rep(seq(1, 9, length.out = 11), times = 3),
                     rep(-1:1, each = 11))
    inside <- cbind(rep(- seq(1, 9, length.out = 11), times = 3),
                    rep(-1:1, each = 11))
    combo <- rbind(outside[1:16, ], 
                   inside[1:17, ])

    # Do the test for each of these possibilities, as well as R and Rcpp
    testthat::expect_false(any(predped::moving_options(my_agent, 
                                                       my_state, 
                                                       my_background,
                                                       outside, 
                                                       cpp = FALSE)))
    testthat::expect_false(any(predped::moving_options(my_agent, 
                                                       my_state, 
                                                       my_background,
                                                       outside, 
                                                       cpp = TRUE)))

    testthat::expect_true(all(predped::moving_options(my_agent, 
                                                      my_state, 
                                                      my_background,
                                                      inside, 
                                                      cpp = FALSE)))
    testthat::expect_true(all(predped::moving_options(my_agent, 
                                                      my_state, 
                                                      my_background,
                                                      inside, 
                                                      cpp = TRUE)))

    testthat::expect_equal(predped::moving_options(my_agent, 
                                                   my_state, 
                                                   my_background,
                                                   combo, 
                                                   cpp = FALSE), 
                           matrix(c(rep(FALSE, 16), rep(TRUE, 17)), 
                                  nrow = 11, 
                                  ncol = 3))
    testthat::expect_equal(predped::moving_options(my_agent, 
                                                   my_state, 
                                                   my_background,
                                                   combo, 
                                                   cpp = TRUE), 
                           matrix(c(rep(FALSE, 16), rep(TRUE, 17)), 
                                  nrow = 11, 
                                  ncol = 3))

    # Visualize them all 
    # predped::plot(my_background) +
    #     predped::plot(my_agent) +
    #     ggplot2::annotate("point", 
    #                       x = outside[, 1],
    #                       y = outside[, 2],
    #                       color = "red") +
    #     ggplot2::annotate("point", 
    #                       x = inside[, 1],
    #                       y = inside[, 2],
    #                       color = "blue") +
    #     ggplot2::annotate("point", 
    #                       x = combo[, 1],
    #                       y = combo[, 2],
    #                       color = "green")
})

# Add attempted test: Problem is that when all seesGoalOK are FALSE, it will move
# back to the initial check, which in this case is all TRUE. 
#
# TO DO: Check whether we can test this feature of the model anyway.
# testthat::test_that(
#     "Testing edge case: Can move everywhere, but won't see goal, R and Rcpp",
#     {
#         # Create a setting with only a single object
#         my_setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
#                                                                      size = c(10, 10)),
#                                           objects = list(predped::rectangle(center = c(0, 0), 
#                                                                             size = c(2, 2))))

#         # Create a state (importantly, without the actual agent in there)
#         my_state <- predped::state(iteration = 1, 
#                                    setting = my_setting, 
#                                    agents = list())
        
#         # Create a set of goals that may or may not be visible to the agent
#         goals <- list(predped::goal(position = c(0, 1.05)),
#                       predped::goal(position = c(1.05, 0)),
#                       predped::goal(position = c(0, -1.05)),
#                       predped::goal(position = c(-1.05, 0)))
#         goals <- lapply(goals, 
#                         function(x) {
#                             x@path <- matrix(x@position, nrow = 1)
#                             return(x)
#                         })

#         # Create an agent in the left-corner of the space. Will allow them to 
#         # see the negative position goals, but not the others. Importantly, 
#         # will be able to move anywhere with regards to not overlapping with 
#         # objects: Any restrictions in moving_options comes from not seeing a 
#         # goal
#         my_agent <- predped::agent(center = c(-4, -4), 
#                                    radius = 0.25,
#                                    orientation = 45, 
#                                    speed = 1.5)

#         centers <- m4ma::c_vd(1:33, 
#                               predped::position(my_agent),
#                               predped::speed(my_agent),
#                               predped::orientation(my_agent))

#         # Create the reference
#         ref <- c(FALSE, FALSE, TRUE, TRUE)

#         # Loop over the different goals and do the required tests
#         for(i in seq_along(goals)) {
#             # Change the current goal of the agent
#             my_agent@current_goal <- goals[[i]] 

#             # Compute the moving options for the agent
#             check <- predped::moving_options(my_agent, 
#                                              my_state, 
#                                              my_setting, 
#                                              centers,
#                                              cpp = FALSE)

#         }

#         # Visualize the situation
#         # predped::plot(my_setting) +
#         #     predped::plot(my_agent) + 
#         #     ggplot2::annotate("point", 
#         #                       x = sapply(goals, \(x) x@position[1]),
#         #                       y = sapply(goals, \(x) x@position[2]),
#         #                       size = 3, 
#         #                       color = "cornflowerblue") +
#         #     ggplot2::annotate("point", 
#         #                       x = centers[, 1],
#         #                       y = centers[, 2],
#         #                       size = 1.5, 
#         #                       color = "red")
#     }
# )
