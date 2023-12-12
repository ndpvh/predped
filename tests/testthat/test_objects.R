
testthat::test_that("Coordinate initialization works", {
    ref <- c(1, 1)
    p <- predped::coordinate(ref)
    testthat::expect_true(all(p == ref))

    testthat::expect_error(predped::coordinate(c(1, 1, 1)))
    testthat::expect_error(predped::coordinate(""))
})

testthat::test_that("Point rotation works", {
    # Center is origin
    p <- predped::rotate(c(0, 1), radians = pi/2, center = c(0, 0))

    testthat::expect_equal(p, predped::coordinate(c(-1, 0)))

    # Center is not origin
    p <- predped::rotate(c(2, 4), radians = pi/2, center = c(2, 2))

    testthat::expect_equal(p, predped::coordinate(c(0, 2)))
})

testthat::test_that("Object is abstract base class", {
    testthat::expect_error(new("object"))
})

testthat::test_that("Rectangle initialization works", {
    testthat::expect_no_error(predped::rectangle(center = c(0, 0), size = c(1, 1)))
    testthat::expect_error(predped::rectangle(center = c(0, 0), size = 1))
    testthat::expect_error(predped::rectangle(center = c(0, 0, 0), size = c(1, 1)))
})

testthat::test_that("Rectangle moving works", {
    r <- predped::move(predped::rectangle(
        center = c(0, 0),
        size = c(1, 1),
        moveable = TRUE
    ), c(1, 1))
    testthat::expect_equal(r@center, predped::coordinate(c(1, 1)))
    r <- predped::move(predped::rectangle(
        center = c(0, 0),
        size = c(1, 1),
        moveable = FALSE
    ), c(1, 1))
    testthat::expect_equal(r@center, predped::coordinate(c(0, 0)))
})

testthat::test_that("Rectangle rotation works", {
    ref <- rbind(
        predped::coordinate(c(1.5, 0.5)),
        predped::coordinate(c(-0.5, 0.5)),
        predped::coordinate(c(-0.5, 1.5)),
        predped::coordinate(c(1.5, 1.5))
    )
    r <- predped::rotate(predped::rectangle(
        center = c(0.5, 1),
        size = c(1, 2)
    ), radians = pi/2)
    testthat::expect_equal(r@points, ref)
    r <- predped::rotate(predped::rectangle(
        center = c(0.5, 1),
        size = c(1, 2)
    ), degrees = 90)
    testthat::expect_equal(r@points, ref)
    r <- predped::rotate(predped::rectangle(
        center = c(0.5, 1),
        size = c(1, 2)
    ), radians = pi/2)
    testthat::expect_equal(r@points, ref)
})

testthat::test_that("Circle initialization works", {
    testthat::expect_no_error(predped::circle(center = c(0, 0), radius = 0.5))
    testthat::expect_error(predped::rectangle(center = c(0, 0), radius = c(0.5, 0.5)))
})

testthat::test_that("Circle moving works", {
    r <- predped::move(predped::circle(
        center = c(0, 0),
        radius = 0.5,
        moveable = TRUE
    ), c(1, 1))
    testthat::expect_equal(r@center, predped::coordinate(c(1, 1)))
    r <- predped::move(predped::circle(
        center = c(0, 0),
        radius = 0.5,
        moveable = FALSE
    ), c(1, 1))
    testthat::expect_equal(r@center, predped::coordinate(c(0, 0)))
})
