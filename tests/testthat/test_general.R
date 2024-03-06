
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
