testthat::test_that("Perpendicular orientation making works", {
    settings <- list(background(shape = rectangle(center = c(0, 0),
                                                  size = c(10, 10)), 
                                entrance = c(-5, 0)), 
                     background(shape = rectangle(center = c(0, 0),
                                                  size = c(10, 10), 
                                                  orientation = pi/4), 
                                entrance = c(-2.5, 2.5)),
                     background(shape = rectangle(center = c(0, 0),
                                                  size = c(10, 10),
                                                  clock_wise = FALSE), 
                                entrance = c(-5, 0)))

    ref <- list(0, 315, 0)

    tst <- lapply(settings, 
                  \(x) perpendicular_orientation(shape(x), entrance(x)))

    testthat::expect_equal(tst, ref)
})

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
                     \(x) predped::predped(id = paste0("model", x), 
                                           setting = settings[[x]],
                                           archetypes = "BaselineEuropean"))

    # Create an agent for each setting
    agents <- lapply(models, 
                     \(x) predped::add_agent(x, 5, \(y) 5))

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

    model <- predped::predped(id = "toy model", 
                              setting = setting, 
                              archetypes = "BaselineEuropean")

    # Create an initial condition with 3 agents within this environment
    set.seed(1)
    agents_few <- predped::create_initial_condition(3, model, 5)

    # Also create one with an impossible number of agents
    set.seed(1)
    agents_many <- predped::create_initial_condition(50, model, 5)

    testthat::expect_equal(length(agents_few), 3)
    testthat::expect_equal(length(agents_many), 8)
    testthat::expect_message(predped::create_initial_condition(50, model, 5))

    # If you would ever want to visualize it during debugging
    # state <- list(agents = agents_many, setting = setting)
    # predped::plot(list(state), trace = TRUE)
})
