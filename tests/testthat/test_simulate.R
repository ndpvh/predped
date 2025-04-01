

testthat::test_that("Adding agent works", {
    # Create several settings differing in the entrance
    settings <- list(predped::background(shape = rectangle(center = c(0, 0), 
                                                     size = c(2, 2)),
                                         objects = list(circle(center = c(0, 0), 
                                                               radius = 0.2,
                                                               interactable = TRUE)),
                                         entrance = c(-1, 0)),
                     predped::background(shape = rectangle(center = c(0, 0), 
                                                     size = c(2, 2)),
                                         objects = list(circle(center = c(0, 0), 
                                                               radius = 0.2,
                                                               interactable = TRUE)),
                                         entrance = c(0, 1)),
                     predped::background(shape = rectangle(center = c(0, 0), 
                                                     size = c(2, 2)),
                                         objects = list(circle(center = c(0, 0), 
                                                               radius = 0.2,
                                                               interactable = TRUE)),
                                         entrance = c(1, 0)),
                     predped::background(shape = rectangle(center = c(0, 0), 
                                                     size = c(2, 2)),
                                         objects = list(circle(center = c(0, 0), 
                                                               radius = 0.2,
                                                               interactable = TRUE)),
                                         entrance = c(0, -1)))

    # Create the predped models necessary for `add_agent` to work
    models <- lapply(1:length(settings), 
                     \(x) predped::predped(settings[[x]], 
                                           id = paste0("model", x), 
                                           archetypes = "BaselineEuropean"))

    # Create an agent for each setting
    agents <- lapply(models, 
                     \(x) predped::add_agent(x, 
                                             goal_number = 5, 
                                             goal_duration = 5))

    # Extrace all necessary information for the check to work
    tst_goals <- lapply(agents, 
                        \(x) length(predped::goals(x)))
    tst_duration <- lapply(agents, 
                           \(x) predped::current_goal(x)@counter)
    tst_orient <- lapply(agents, 
                         \(x) as.numeric(predped::orientation(x)))
    tst_pos <- lapply(agents, 
                      \(x) predped::position(x))

    ref_goals <- list(4, 4, 4, 4)
    ref_duration <- list(5, 5, 5, 5)
    ref_orient <- list(0, 270, 180, 90)
    ref_pos <- list(predped::coordinate(c(-0.7, 0)), 
                    predped::coordinate(c(0, 0.7)), 
                    predped::coordinate(c(0.7, 0)), 
                    predped::coordinate(c(0, -0.7)))

    # And the tests
    testthat::expect_equal(tst_goals, ref_goals)
    testthat::expect_equal(tst_duration, ref_duration)
    testthat::expect_equal(tst_orient, ref_orient)
    testthat::expect_equal(tst_pos, ref_pos, tolerance = 1e-1)
})

testthat::test_that("Creating initial condition works", {
    # Create the model within which the agents should be simulated
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(2, 2)),
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(0.5, 0.5),
                                                                     interactable = TRUE)),
                                   entrance = c(-1, 0))

    model <- predped::predped(setting, archetypes = "BaselineEuropean")

    # Create an initial condition with 3 agents within this environment
    set.seed(1)
    agents_few <- predped::create_initial_condition(3, model, goal_number = 5, individual_differences = FALSE)

    # Also create one with an impossible number of agents
    #
    # Check this test, stops prematurely for some reason
    set.seed(1)
    agents_many <- predped::create_initial_condition(50, model, goal_number = 5, individual_differences = FALSE)

    testthat::expect_equal(length(agents_few), 3)
    testthat::expect_equal(length(agents_many), 5)
    testthat::expect_message(predped::create_initial_condition(50, model, goal_number = 5))

    # If you would ever want to visualize it during debugging
    state <- predped::state(iteration = 0, agents = agents_many, setting = setting)
    plot(state)
})

testthat::test_that("Creating initial condition with groups works", {
    # Create the model within which the agents should be simulated
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(10, 10)),
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(1, 1),
                                                                     interactable = TRUE)),
                                   entrance = c(-5, 0))

    model <- predped::predped(setting, archetypes = "BaselineEuropean")

    # Create an initial condition with 10 agents that each belong to a different 
    # group (default)
    set.seed(1)
    agents_alone <- predped::create_initial_condition(10, model, goal_number = 5)

    # Create an initial condition with 10 agents that each go together in pairs.
    # In one, we will test the function's ability to correct the weights
    set.seed(1)
    agents_pairs_1 <- predped::create_initial_condition(10, model, goal_number = 5, group_size = matrix(c(2, 1), nrow = 1))
    agents_pairs_2 <- predped::create_initial_condition(10, model, goal_number = 5, group_size = matrix(c(2, 2), nrow = 1))

    # Create an initial condition with 10 agents that might differ in their 
    # group sizes. In one, we will test the function's ability to correct the weights
    set.seed(1)
    agents_more_1 <- predped::create_initial_condition(20, model, goal_number = 5, group_size = matrix(c(1, 2, 3, 4, 0.25, 0.25, 0.25, 0.25), nrow = 4))
    set.seed(1)
    agents_more_2 <- predped::create_initial_condition(20, model, goal_number = 5, group_size = matrix(c(1, 2, 3, 4, 1, 1, 1, 1), nrow = 4))

    # For each of the initial conditions, find out which group the agents belong 
    # to
    tst_alone <- sapply(agents_alone, predped::group)
    tst_pairs_1 <- sapply(agents_pairs_1, predped::group)
    tst_pairs_2 <- sapply(agents_pairs_2, predped::group)
    tst_more_1 <- sapply(agents_more_1, predped::group)
    tst_more_2 <- sapply(agents_more_2, predped::group)

    testthat::expect_equal(as.numeric(tst_alone), 1:10)
    testthat::expect_equal(as.numeric(tst_pairs_1), rep(1:5, each = 2))
    testthat::expect_equal(as.numeric(tst_pairs_2), rep(1:5, each = 2))
    testthat::expect_equal(as.numeric(tst_more_1), c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 5, 5, 6, 7, 8, 8, 8, 8, 9))
    testthat::expect_equal(as.numeric(tst_more_2), as.numeric(tst_more_1))

    # Create an initial condition that will lead to an error
    testthat::expect_error(predped::create_initial_condition(10, model, goal_number = 5, group_size = matrix(c(1, Inf), nrow = 1)))
    testthat::expect_error(predped::create_initial_condition(10, model, goal_number = 5, group_size = matrix(c(1, NA), nrow = 1)))

    # If you would ever want to visualize it during debugging
    # state <- list(agents = agents_many, setting = setting)
    # predped::plot(list(state), trace = TRUE)
})
