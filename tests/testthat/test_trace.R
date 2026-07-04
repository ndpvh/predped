testthat::test_that("Trace initialization works", {
    # Create a mock environment to be used 
    setting <- background(shape = rectangle(center = c(0, 0), size = c(5, 5)),
                          objects = list(circle(center = c(0, 0), radius = 1)))

    # Check when to expect errors and when not to 
    testthat::expect_no_error(trace(setting,
                                    id = "my trace",
                                    time_step = 0.5, 
                                    states = list(), 
                                    variables = list()))
    testthat::expect_no_error(trace(setting = setting))

    testthat::expect_error(trace(id = "my trace",
                                 time_step = 0.5,
                                 states = list(), 
                                 variables = list()))
})

testthat::test_that("Trace getters work", {
    # Create a trace to test the getters on. Starts with defining an environment
    # an agent, and variables that were accounted for
    my_background <- background(shape = rectangle(center = c(0, 0), size = c(5, 5)),
                                objects = list(circle(center = c(0, 0), radius = 1)))
    my_agent <- agent(center = c(-2, 0), radius = 0.25)
    
    my_trace <- trace(id = "my trace", 
                      time_step = 1, 
                      setting = my_background, 
                      states = list(list(),
                                    list(my_agent)),
                      variables = list(list("evacuation" = FALSE), 
                                       list("evacuation" = TRUE)))

    # Use the getters to retrieve all pieces of information
    testthat::expect_equal(id(my_trace), "my trace")
    testthat::expect_equal(time_step(my_trace), 1)
    testthat::expect_equal(setting(my_trace), my_background)
    testthat::expect_equal(states(my_trace), 
                           list(list(), list(my_agent)))
    testthat::expect_equal(variables(my_trace), 
                           list(list("evacuation" = FALSE), list("evacuation" = TRUE)))
})

testthat::test_that("Trace setters works", {
    # Create a trace to test the getters on. Starts with defining an environment
    # an agent, and variables that were accounted for
    my_background <- background(shape = rectangle(center = c(0, 0), size = c(5, 5)),
                                objects = list(circle(center = c(0, 0), radius = 1)))
    my_agent <- agent(center = c(-2, 0), radius = 0.25)
    
    my_trace <- trace(id = "my trace", 
                      time_step = 1, 
                      setting = my_background, 
                      states = list(list(),
                                    list(my_agent)),
                      variables = list(list("evacuation" = FALSE), 
                                       list("evacuation" = TRUE)))

    # New setting
    new_background <- background(shape = circle(center = c(0, 0), radius = 2.5),
                                 objects = list(rectangle(center = c(0, 0), size = c(1, 1))))

    # Change all of the values for the trace
    id(my_trace) <- "test"
    suppressWarnings(time_step(my_trace) <- 2)
    suppressWarnings(setting(my_trace) <- new_background)
    states(my_trace) <- list(list(my_agent), list())
    variables(my_trace) <- list(list("vars" = 1), list("vars" = 2))

    # Check these values
    testthat::expect_equal(id(my_trace), "test")
    testthat::expect_equal(time_step(my_trace), 2)
    testthat::expect_equal(setting(my_trace), new_background)
    testthat::expect_equal(states(my_trace), 
                           list(list(my_agent), list()))
    testthat::expect_equal(variables(my_trace), 
                           list(list("vars" = 1), list("vars" = 2)))

    # Check whether a warning is thrown when changing the time step
    testthat::expect_warning(time_step(my_trace) <- 3)

    # Check whether a warning is thrown when changing the setting
    testthat::expect_warning(setting(my_trace) <- my_background)

    # Check whether an error is thrown when there is a discrepancy between the 
    # states and variables when changing these slots
    testthat::expect_error(states(my_trace) <- list(list()))
    testthat::expect_error(states(my_trace) <- list(list(), list(my_agent), list(my_agent)))

    testthat::expect_error(variables(my_trace) <- list(list("vars" = 1)))
    testthat::expect_error(variables(my_trace) <- list(list("vars" = 1), list("vars" = 1), list("vars" = 1)))
})

testthat::test_that("Appending to a trace works", {
    # Create a setting, an initial list of agents and variables, and an initial 
    # trace.
    my_background <- background(shape = rectangle(center = c(0, 0), size = c(5, 5)),
                                objects = list(circle(center = c(0, 0), radius = 1)))
    my_agent <- agent(center = c(-2, 0), radius = 0.25)
    my_vars <- list("vars" = 1)

    my_trace <- trace(id = "my trace", 
                      setting = my_background, 
                      time_step = 0.5,
                      states = list(list(my_agent)),
                      variables = list(my_vars))

    # Create a new state to append to the trace
    new_agent <- agent(center = c(2, 0), radius = 0.25)
    new_vars <- list("vars" = 2)

    my_state <- state(iteration = 1, 
                      setting = my_background,
                      agents = list(my_agent, new_agent), 
                      variables = new_vars)

    # Create a reference trace of what this should look like after being
    # appended to
    ref <- trace(id = "my trace", 
                 time_step = 0.5, 
                 setting = my_background, 
                 states = list(list(my_agent), list(my_agent, new_agent)),
                 variables = list(my_vars, new_vars))

    # Append the state to the trace and perform some tests
    tst <- append_trace(my_trace, my_state)
    testthat::expect_equal(ref, tst)

    # Append the agents and variables to the trace separately and perform some 
    # tests
    tst <- append_trace(my_trace, 
                        agents = list(my_agent, new_agent), 
                        variables = new_vars)
    testthat::expect_equal(ref, tst)
})
