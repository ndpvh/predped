
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

    testthat::expect_true(predped::line_intersection(tst1, tst2))
    testthat::expect_false(predped::line_intersection(tst1, tst3))
})
