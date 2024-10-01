testthat::test_that("Agent initialization works", {
    testthat::expect_no_error(predped::agent(center = c(1, 1), 
                                             orientation = 0,
                                             radius = 0.2))

    testthat::expect_error(predped::agent(center = c(1, 1, 1)))     # Invalid coordinate
    testthat::expect_error(predped::agent(position = c(1, 1)))      # Invalid 

    # In response to bug in which the given id was not assigned to the objects,
    # but it was to agents and goals
    me <- predped::agent(id = "test", center = c(0, 0), radius = 0.2)
    testthat::expect_equal(me@id, "test")
})

testthat::test_that("Agent getters work", {
    params <- load_parameters()
    tst <- predped::agent(id = "test",
                          center = c(0, 0), 
                          radius = 0.2,
                          orientation = 0, 
                          speed = 0.1, 
                          group = 1, 
                          cell = 0, 
                          status = "wait",
                          color = "blue",
                          waiting_counter = 5,
                          current_goal = goal(id = "goal test", 
                                              position = c(0, 0)),
                          goals = list(goal(id = "new goal",
                                            position = c(1, 0))),
                          parameters = params[["params_archetypes"]][1,])

    testthat::expect_equal(as.character(predped::id(tst)), "test")
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(0, 0)))
    testthat::expect_equal(predped::position(tst), predped::coordinate(c(0, 0)))
    testthat::expect_equal(predped::radius(tst), 0.2)
    testthat::expect_equal(as.numeric(predped::size(tst)), 0.2)
    testthat::expect_equal(as.numeric(predped::orientation(tst)), 0)
    testthat::expect_equal(as.numeric(predped::speed(tst)), 0.1)
    testthat::expect_equal(as.numeric(predped::group(tst)), 1)
    testthat::expect_equal(as.numeric(predped::cell(tst)), 0)
    testthat::expect_equal(as.character(predped::status(tst)), "wait")
    testthat::expect_equal(as.character(predped::color(tst)), "blue")
    testthat::expect_equal(as.numeric(predped::waiting_counter(tst)), 5)
    testthat::expect_equal(predped::current_goal(tst), predped::goal(id = "goal test", 
                                                                     position = c(0, 0)))
    testthat::expect_equal(predped::goals(tst), list(predped::goal(id = "new goal", 
                                                                   position = c(1, 0))))
    testthat::expect_equal(predped::parameters(tst), params[["params_archetypes"]][1,])
})

testthat::test_that("Agent setters work", {
    params <- load_parameters()
    tst <- predped::agent(id = "test",
                          center = c(0, 0), 
                          radius = 0.2)

    # Agent-specific setters
    predped::id(tst) <- "other name"
    predped::orientation(tst) <- 180
    predped::speed(tst) <- 0.2
    predped::group(tst) <- 0
    predped::cell(tst) <- 1
    predped::status(tst) <- "plan"
    predped::color(tst) <- "green"
    predped::waiting_counter(tst) <- 2
    predped::current_goal(tst) <- predped::goal(id = "other goal", 
                                                position = c(0, 0))
    predped::goals(tst) <- list(predped::goal(id = "other new goal", 
                                              position = c(1, 0)))
    predped::parameters(tst) <- params[["params_archetypes"]][2,]

    testthat::expect_equal(as.character(predped::id(tst)), "other name")
    testthat::expect_equal(as.numeric(predped::orientation(tst)), 180)
    testthat::expect_equal(as.numeric(predped::speed(tst)), 0.2)
    testthat::expect_equal(as.numeric(predped::group(tst)), 0)
    testthat::expect_equal(as.numeric(predped::cell(tst)), 1)
    testthat::expect_equal(as.character(predped::status(tst)), "plan")
    testthat::expect_equal(as.character(predped::color(tst)), "green")
    testthat::expect_equal(as.numeric(predped::waiting_counter(tst)), 2)
    testthat::expect_equal(predped::current_goal(tst), predped::goal(id = "other goal",
                                                                     position = c(0, 0)))
    testthat::expect_equal(predped::goals(tst), list(predped::goal(id = "other new goal", 
                                                                   position = c(1, 0))))
    testthat::expect_equal(predped::parameters(tst), params[["params_archetypes"]][2,])

    # Shared setters for agents and circles
    predped::center(tst) <- c(1, 0)
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(1, 0)))
    testthat::expect_equal(predped::position(tst), predped::coordinate(c(1, 0)))

    predped::position(tst) <- c(0, 1)
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(0, 1)))
    testthat::expect_equal(predped::position(tst), predped::coordinate(c(0, 1)))

    predped::radius(tst) <- 0.3
    testthat::expect_equal(predped::radius(tst), 0.3)
    testthat::expect_equal(as.numeric(predped::size(tst)), 0.3)

    predped::size(tst) <- 0.4
    testthat::expect_equal(predped::radius(tst), 0.4)
    testthat::expect_equal(as.numeric(predped::size(tst)), 0.4)
})
