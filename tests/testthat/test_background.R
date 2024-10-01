testthat::test_that("Background initialization works", {
    # Create valid and invalid objects
    valid_objects <- list(predped::rectangle(center = c(0, 0), 
                                             size = c(1, 1)),
                          predped::circle(center = c(0, 0),
                                          radius = 1))
    invalid_objects <- list(predped::rectangle(center = c(0, 0), 
                                               size = c(1, 1)),
                            "test")

    # Initialization tests
    shp <- predped::circle(center = c(0, 0), radius = 2)
    testthat::expect_no_error(predped::background(shape = shp, objects = valid_objects))
    testthat::expect_no_error(predped::background(shape = shp, objects = valid_objects, entrance = c(0, 1)))

    testthat::expect_error(predped::background(shape = shp, objects = invalid_objects)) 

    # Tests of single entrance and exit
    setting <- predped::background(shape = shp, objects = valid_objects, entrance = c(0, 1))
    testthat::expect_equal(setting@exit, matrix(c(0, 1), ncol = 2))

    setting <- predped::background(shape = shp, objects = valid_objects, exit = c(0, 1), same_exit = FALSE)
    testthat::expect_equal(setting@exit, matrix(c(0, 1), ncol = 2))

    # Tests of multiple entrances and exits
    setting <- predped::background(shape = shp, objects = valid_objects, entrance = c(0, 1, 1, 1))
    testthat::expect_equal(setting@exit, matrix(c(0, 1, 1, 1), ncol = 2))

    setting <- predped::background(shape = shp, objects = valid_objects, exit = c(0, 1, 1, 1), same_exit = FALSE)
    testthat::expect_equal(setting@exit, matrix(c(0, 1, 1, 1), ncol = 2))
})

testthat::test_that("Background getters work", {
    shp <- predped::circle(center = c(0, 0), radius = 5)
    obj <- list(predped::rectangle(center = c(0, 0), size = c(1, 1)))

    setting <- predped::background(shape = shp, 
                                   objects = obj, 
                                   entrance = c(1, 1),
                                   exit = c(2, 1),
                                   same_exit = FALSE)

    testthat::expect_equal(predped::shape(setting), shp)
    testthat::expect_equal(predped::objects(setting), obj)
    testthat::expect_equal(predped::entrance(setting), matrix(c(1, 1), ncol = 2))
    testthat::expect_equal(predped::exit(setting), matrix(c(2, 1), ncol = 2))
})

testthat::test_that("Background setters work", {
    shp <- predped::circle(center = c(0, 0), radius = 5)
    obj <- list(predped::rectangle(center = c(0, 0), size = c(1, 1)))

    setting <- predped::background(shape = shp, 
                                   objects = obj, 
                                   entrance = c(1, 1),
                                   exit = c(2, 1),
                                   same_exit = FALSE)

    shp_2 <- predped::rectangle(center = c(0, 0), size = c(10, 10))
    obj_2 <- list(predped::circle(center = c(0, 0), radius = 1))

    predped::shape(setting) <- shp_2
    predped::objects(setting) <- obj_2
    predped::entrance(setting) <- c(2, 2)
    predped::exit(setting) <- c(1, 2)

    testthat::expect_equal(predped::shape(setting), shp_2)
    testthat::expect_equal(predped::objects(setting), obj_2)
    testthat::expect_equal(predped::entrance(setting), matrix(c(2, 2), ncol = 2))
    testthat::expect_equal(predped::exit(setting), matrix(c(1, 2), ncol = 2))
})

testthat::test_that("Limit access for an agent works", {
    # Create settings and agents
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(2, 2)),
                                   objects = list(),
                                   limited_access = list(segment(from = c(-1, 0), 
                                                                 to = c(1, 0))), 
                                   entrance = predped::coordinate(c(-1, 0)))

    all_agents <- list(# Above the segment, should be blocked
                       predped::agent(center = c(-0.5, 0.5), 
                                      radius = 0.2),
                       predped::agent(center = c(0.5, 0.5), 
                                      radius = 0.2),
                       # Below the segment, should not be blocked
                       predped::agent(center = c(-0.5, -0.5), 
                                      radius = 0.2),
                       predped::agent(center = c(0.5, -0.5), 
                                      radius = 0.2),
                       # On the segment, should not be blocked
                       predped::agent(center = c(-0.5, 0), 
                                      radius = 0.2),
                       predped::agent(center = c(0.5, 0), 
                                      radius = 0.2))

    # Apply the test. In this test, we first limit the access of the agents and 
    # then check whether the corresponding polygon can be found in the list of 
    # objects for those agents for whom it is relevant.
    tst <- sapply(all_agents, 
                  \(x) predped::limit_access(setting, x))
    ref <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)

    testthat::expect_equal(tst, ref)

    # Let's reverse the line in the setting, which should make it so that the 
    # agents below the segment are blocked and those above are not
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(2, 2)),
                                   objects = list(),
                                   limited_access = list(segment(from = c(1, 0), 
                                                                 to = c(-1, 0))), 
                                   entrance = predped::coordinate(c(-1, 0)))

    # Apply the test. In this test, we first limit the access of the agents and 
    # then check whether the corresponding polygon can be found in the list of 
    # objects for those agents for whom it is relevant.
    tst <- sapply(all_agents, 
                  \(x) predped::limit_access(setting, x))
    ref <- c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)

    testthat::expect_equal(tst, ref)
})

testthat::test_that("Limit access for a coordinate works", {
    # Create settings and agents. Here, we can immediately add two segments to 
    # the  `limited_access` and test all coordinates against it simultaneously. 
    # The results will be the same as in the previous test, but now the two 
    # references will be two columns in one matrix
    setting <- predped::background(shape = predped::rectangle(center = c(0,0), 
                                                              size = c(2, 2)),
                                   objects = list(),
                                   limited_access = list(segment(from = c(-1, 0), 
                                                                 to = c(1, 0)), 
                                                         segment(from = c(1, 0), 
                                                                 to = c(-1, 0))), 
                                   entrance = predped::coordinate(c(-1, 0)))

    pts <- rbind(# Above the segment, should be blocked
                c(-0.5, 0.5), 
                c(0.5, 0.5), 
                # Below the segment, should not be blocked
                c(-0.5, -0.5), 
                c(0.5, -0.5), 
                # On the segment, should not be blocked
                c(-0.5, 0), 
                c(0.5, 0))

    # Apply the test. In this test, we first limit the access of the agents and 
    # then check whether the corresponding polygon can be found in the list of 
    # objects for those agents for whom it is relevant.
    tst <- predped::limit_access(setting, pts)
    ref <- cbind(c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE), 
                 c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))

    testthat::expect_equal(tst, ref)
})
