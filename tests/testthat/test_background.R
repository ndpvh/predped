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
