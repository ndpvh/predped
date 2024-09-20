testthat::test_that("State initialization works", {
    # Create a valid background
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0),
                                                              size = c(2, 2)),
                                   objects = list())

    # Create valid and invalid agent lists
    valid_agents_1 <- list()
    valid_agents_2 <- list(predped::agent(center = c(0, 0), radius = 0.25))
    invalid_agents_1 <- list("test")
    invalid_agents_2 <- list(predped::agent(center = c(0, 0), radius = 0.25), 
                             "test")

    # Valid initializations
    testthat::expect_no_error(predped::state(iteration = 1, 
                                             setting = setting))
    testthat::expect_no_error(predped::state(iteration = 1.5, 
                                             setting = setting))
    testthat::expect_no_error(predped::state(iteration = 1, 
                                             setting = setting, 
                                             agents = valid_agents_1))
    testthat::expect_no_error(predped::state(iteration = 1, 
                                             setting = setting, 
                                             agents = valid_agents_2))

    # Insufficient information
    testthat::expect_error(predped::state(agents = valid_agents_1))
    testthat::expect_error(predped::state(setting = setting, 
                                          agents = valid_agents_1))
    testthat::expect_error(predped::state(iteration = 1, 
                                          agents = valid_agents_1))

    # Invalid iteration
    testthat::expect_error(predped::state(iteration = "test", 
                                          setting = setting, 
                                          agents = valid_agents_1))
    testthat::expect_error(predped::state(iteration = "1", 
                                          setting = setting, 
                                          agents = valid_agents_1))

    # Invalid setting
    testthat::expect_error(predped::state(iteration = 1, 
                                          setting = "test", 
                                          agents = valid_agents_1))
    testthat::expect_error(predped::state(iteration = 1, 
                                          setting = valid_agents_1, 
                                          agents = valid_agents_1))

    # Invalid agents
    testthat::expect_error(predped::state(iteration = 1, 
                                          setting = setting, 
                                          agents = invalid_agents_1))
    testthat::expect_error(predped::state(iteration = 1, 
                                          setting = setting, 
                                          agents = invalid_agents_2))
})

testthat::test_that("State getters work", {
    # Create a state
    my_setting <- predped::background(shape = predped::rectangle(center = c(0, 0),
                                                                 size = c(2, 2)),
                                      objects = list())
    my_agents <- list(predped::agent(center = c(0, 0), radius = 0.25))

    my_state <- predped::state(iteration = 1.5, 
                               setting = my_setting, 
                               agents = my_agents)

    # Test getters
    testthat::expect_equal(predped::iteration(my_state), 1)
    testthat::expect_equal(predped::setting(my_state), my_setting)
    testthat::expect_equal(predped::agents(my_state), my_agents)
})

testthat::test_that("State setters work", {
    # Create a state
    my_setting <- predped::background(shape = predped::rectangle(center = c(0, 0),
                                                                 size = c(2, 2)),
                                      objects = list())
    my_agents <- list(predped::agent(center = c(0, 0), radius = 0.25))

    my_state <- predped::state(iteration = 1, 
                               setting = my_setting, 
                               agents = my_agents)

    # Change the values of the state through the setters
    new_setting <- predped::background(shape = predped::circle(center = c(0, 0),
                                                                              radius = 1),
                                       objects = list())
    new_agents <- list()

    predped::iteration(my_state) <- 2.5
    predped::setting(my_state) <- new_setting
    predped::agents(my_state) <- new_agents

    testthat::expect_equal(predped::iteration(my_state), 2)
    testthat::expect_equal(predped::setting(my_state), new_setting)
    testthat::expect_equal(predped::agents(my_state), new_agents)

    # Also check whether invalid values are flagged for state
    testthat::expect_error(predped::setting(my_state) <- "test")
    testthat::expect_error(predped::agents(my_state) <- "test")
})
