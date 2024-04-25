
testthat::test_that("Finding a class in a list works", {
    state <- list(predped::rectangle(center = c(0, 0), size = c(2, 2), orientation = 0), 
                  predped::rectangle(center = c(1, 1), size = c(1, 1), orientation = 45), 
                  predped::circle(center = c(2, 2), radius = 1))

    rect <- predped::find_class("rectangle", state)
    circ <- predped::find_class("circle", state)
    empty <- predped::find_class("triangle", state)

    testthat::expect_true(all(sapply(rect, function(x) class(x) == "rectangle")))
    testthat::expect_true(all(sapply(circ, function(x) class(x) == "circle")))

    testthat::expect_false(any(sapply(rect, function(x) class(x) == "circle")))
    testthat::expect_false(any(sapply(circ, function(x) class(x) == "rectangle")))

    testthat::expect_equal(length(rect), 2)
    testthat::expect_equal(length(circ), 1)
    testthat::expect_equal(length(empty), 0)
})

testthat::test_that("Perpendicular orientation works", {
    # For the polygon
    points <- rbind(c(1, 2), 
                    c(2, 1), 
                    c(2, -1), 
                    c(1, -2), 
                    c(-1, -2), 
                    c(-2, -1), 
                    c(-2, 1), 
                    c(-1, 2))

    # Create the general shapes to be used
    circ <- circle(center = c(0, 0), radius = 5)
    rect <- rectangle(center = c(0, 0), size = c(10, 10))
    rect_45 <- rectangle(center = c(0, 0), 
                         size = c(4, 4),
                         orientation = pi / 4)
    poly <- polygon(points = points)

    # Create the different settings all with different entrances
    settings <- list(# Circles
                     background(shape = circ,
                                entrance = 5 * c(cos(pi), sin(pi))),
                     background(shape = circ, 
                                entrance = 5 * c(cos(pi + pi/4), sin(pi + pi/4))),
                     background(shape = circ, 
                                entrance = 5 * c(cos(pi + pi/2), sin(pi + pi/2))),
                     background(shape = circ, 
                                entrance = 5 * c(cos(pi + 3*pi/4), sin(pi + 3*pi/4))),
                     background(shape = circ, 
                                entrance = 5 * c(cos(2 * pi), sin(2 * pi))),
                     background(shape = circ, 
                                entrance = 5 * c(cos(pi/4), sin(pi/4))),
                     background(shape = circ, 
                                entrance = 5 * c(cos(pi/2), sin(pi/2))),
                     background(shape = circ, 
                                entrance = 5 * c(cos(3 * pi/4), sin(3 * pi/4))),
                     # Rectangles
                     background(shape = rect,
                                entrance = c(-5, 0)),
                     background(shape = rect_45, 
                                entrance = c(-sqrt(2), -sqrt(2))),
                     background(shape = rect,
                                entrance = c(0, -5)),
                     background(shape = rect_45, 
                                entrance = c(sqrt(2), -sqrt(2))),
                     background(shape = rect,
                                entrance = c(5, 0)),
                     background(shape = rect_45, 
                                entrance = c(sqrt(2), sqrt(2))),
                     background(shape = rect,
                                entrance = c(0, 5)),
                     background(shape = rect_45, 
                                entrance = c(-sqrt(2), sqrt(2))),
                     # Polygons
                     background(shape = poly, 
                                entrance = c(-2, 0)),
                     background(shape = poly, 
                                entrance = c(-1.5, -1.5)),
                     background(shape = poly, 
                                entrance = c(0, -2)),
                     background(shape = poly, 
                                entrance = c(1.5, -1.5)),
                     background(shape = poly, 
                                entrance = c(2, 0)),
                     background(shape = poly, 
                                entrance = c(1.5, 1.5)),
                     background(shape = poly, 
                                entrance = c(0, 2)),
                     background(shape = poly, 
                                entrance = c(-1.5, 1.5)))

    tst <- lapply(settings, 
                  \(x) predped::perpendicular_orientation(shape(x), entrance(x)))
    ref <- list(0, 45, 90, 135, 180, 225, 270, 315,
                0, 45, 90, 135, 180, 225, 270, 315,
                0, 45, 90, 135, 180, 225, 270, 315)

    testthat::expect_equal(tst, ref) # To change once figured out how to deal with this
})

testthat::test_that("Vectorized line-line intersection works", {
    tst1 <- rbind(c(1, 1, 4, 4), 
                  c(3, -2, 6, -2),
                  c(-3, -1, -3, -5))
    tst2 <- rbind(c(1, 2, 5, 2),
                  c(8, 3, 8, 6),
                  c(3, -2, 3, -5))
    tst3 <- rbind(c(-3, 2, 1, 5),
                  c(-4, -1, -4, -4),
                  c(7, -1, 10, -1))
    tst4 <- rbind(c(-3, -5, -3, -8),
                  c(6, -2, 9, -2),
                  c(4, 4, 7, 7))

    testthat::expect_true(predped::line_line_intersection(tst1, tst2))
    testthat::expect_false(predped::line_line_intersection(tst1, tst3))
    testthat::expect_false(predped::line_line_intersection(tst1, tst3)) # To change once figured out how to deal with this
})
