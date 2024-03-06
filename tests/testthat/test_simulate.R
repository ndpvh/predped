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
                  \(x) perpendicular_orientation(x))

    testthat::expect_equal(tst, ref)
})