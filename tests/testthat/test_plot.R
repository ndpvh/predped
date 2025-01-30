
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

testthat::test_that("Plotting forbidden edges works", {
    my_background <- predped::background(
        shape = predped::rectangle(
            center = c(0, 0),
            size = c(5, 5)
        )
    )

    rect <- predped::rectangle(center = c(0, 0), size = c(2, 2))
    poly <- predped::polygon(points = rbind(c(-1, -1), c(-1, 1), c(1, 1), c(1, -1)))
    circ <- predped::circle(center = c(0, 0), radius = 1)

    # Rectangles
    cases <- list(1, 
                  1:2,
                  1:4,
                  c(1, 3),
                  c(1, 3:4),
                  c(1:2, 4),
                  c(4, 1))

    lapply(cases,
           function(x) {
              rect@forbidden <- x
              my_background@objects <- list(rect)
              testthat::expect_no_error(predped::plot(my_background, plot_forbidden = TRUE)) 
           })

    # Polygons
    cases <- list(1, 
                  1:2,
                  1:4,
                  c(1, 3),
                  c(1, 3:4),
                  c(1:2, 4),
                  c(4, 1))

    lapply(cases,
           function(x) {
              poly@forbidden <- x
              my_background@objects <- list(poly)
              testthat::expect_no_error(predped::plot(my_background, plot_forbidden = TRUE)) 
           })

    # Circles
    cases <- list(matrix(c(0, 1, 0.5, 2), nrow = 2),
                  matrix(c(0.5, 0, 2, 1), nrow = 2),
                  matrix(c(0, 1), nrow = 1))
    lapply(cases,
           function(x) {
              circ@forbidden <- x
              my_background@objects <- list(circ)
              testthat::expect_no_error(predped::plot(my_background, plot_forbidden = TRUE)) 
           })

})
