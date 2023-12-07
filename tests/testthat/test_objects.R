
testthat::test_that("Coordinate initialization works", {
  ref <- c(1, 1)
  p <- predped::coordinate(ref)
  testthat::expect_true(all(p == ref))
  
  testthat::expect_error(predped::coordinate(c(1, 1, 1)))
  testthat::expect_error(predped::coordinate(""))
})

testthat::test_that("Point rotation works", {
  ref <- c(0, 1)
  p <- predped::rotate(ref, radians = 90*pi/180, center = c(0, 0))
  
  testthat::expect_equal(p, predped::coordinate(c(1, 0)))
})

testthat::test_that("Object is abstract base class", {
  testthat::expect_error(new("object"))
})

testthat::test_that("Rectangle initialization works", {
  testthat::expect_no_error(predped::rectangle(lower = c(0, 0), upper = c(1, 1)))
  testthat::expect_no_error(predped::rectangle(center = c(0, 0), size = c(1, 1)))
  testthat::expect_error(predped::rectangle(center = c(0, 0), lower = c(1, 1)))
  testthat::expect_error(predped::rectangle(size = c(0, 0), lower = c(1, 1)))
  testthat::expect_error(predped::rectangle(lower = c(0, 0), upper = c(0, 1)))
})

testthat::test_that("Rectangle moving works", {
  r <- predped::move(predped::rectangle(
    lower = c(0, 0),
    upper = c(1, 1),
    moveable = TRUE
  ), c(1, 1))
  testthat::expect_equal(r@center, predped::coordinate(c(1, 1)))
  r <- predped::move(predped::rectangle(
    lower = c(0, 0),
    upper = c(1, 1),
    moveable = FALSE
  ), c(1, 1))
  testthat::expect_equal(r@center, predped::coordinate(c(0.5, 0.5)))
})

testthat::test_that("Rectangle rotation works", {
  ref <- list(
    predped::coordinate(c(-0.5, 1.5)), 
    predped::coordinate(c(1.5, 1.5)),
    predped::coordinate(c(1.5, 0.5)),
    predped::coordinate(c(-0.5, 0.5))
  )
  r <- predped::rotate(predped::rectangle(
    lower = c(0, 0),
    upper = c(1, 2),
    moveable = TRUE
  ), radians = 90*pi/180, center = c(0, 0))
  testthat::expect_equal(unname(predped::corners(r)), ref)
  r <- predped::rotate(predped::rectangle(
    lower = c(0, 0),
    upper = c(1, 2),
    moveable = TRUE
  ), degrees = 90, center = c(0, 0))
  testthat::expect_equal(unname(predped::corners(r)), ref)
})

testthat::test_that("Rectangle corners method works", {
  r <- predped::rectangle(
    lower = c(0, 0),
    upper = c(1, 2)
  )
  testthat::expect_equal(predped::corners(r), list(
    lowerleft = predped::coordinate(c(0, 0)), 
    upperleft = predped::coordinate(c(0, 2)),
    upperright = predped::coordinate(c(1, 2)),
    lowerright = predped::coordinate(c(1, 0))
  ))
})

testthat::test_that("Rectangle inObject works", {
  r <- predped::rectangle(
    lower = c(0, 0),
    upper = c(2, 2)
  )
  testthat::expect_true(predped::inObject(r, c(1, 1), outside = FALSE))
  testthat::expect_false(predped::inObject(r, c(1, 1), outside = TRUE))
  testthat::expect_false(predped::inObject(r, c(3, 3), outside = FALSE))
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
