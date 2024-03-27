
testthat::test_that("Plotting a circle works", {
    # Plot a circle with a radius of 1
    testthat::expect_no_error(predped::plot(predped::circle(center = c(0,0), radius = 1)))

    # Plot a circle with an incorrect radius argument
    testthat::expect_error(predped::plot(predped::circle(center = c(0,0), radius = c(1,0))))
})

testthat::test_that("Plotting a rectangle works", {
    # Plot a rectangle with center c(0,0) and size c(1,2)
    testthat::expect_no_error(predped::plot(predped::rectangle(center = c(0,0), size = c(1,2))))
    testthat::expect_error(predped::plot(predped::rectangle(center = 1), size = c(1,2)))
})

testthat::test_that("Plotting a polygon works", {
    # Plot a polygon with x amount of points
    testthat::expect_no_error(predped::plot(predped::polygon(points = matrix(c(-7.5, 7.5, 
                                                                        -2.5, 7.5, 
                                                                        -2.5, 5, 
                                                                        -5, 5,
                                                                        -5, 2.5,
                                                                        -7.5, 2.5),
                                                                        ncol = 2, 
                                                                        byrow = T)))) 
})
