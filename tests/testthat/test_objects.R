################################################################################
# OBJECT

testthat::test_that("Object is abstract base class", {
    testthat::expect_error(new("object"))
})

################################################################################
# POINTS

testthat::test_that("Coordinate initialization works", {
    ref <- c(1, 1)
    p <- predped::coordinate(ref)
    testthat::expect_true(all(p == ref))

    testthat::expect_error(predped::coordinate(c(1, 1, 1)))
    testthat::expect_error(predped::coordinate(""))
})

testthat::test_that("Single point rotation works", {
    # Center is origin
    p <- predped::rotate(c(0, 1), radians = pi/2, center = c(0, 0))

    testthat::expect_equal(p, predped::coordinate(c(-1, 0)))

    # Center is not origin
    p <- predped::rotate(c(2, 4), radians = pi/2, center = c(2, 2))

    testthat::expect_equal(p, predped::coordinate(c(0, 2)))
})

testthat::test_that("Multiple points rotation works", {
    # Center is origin
    X <- rbind(c(0, 1), c(1, 0))
    p <- predped::rotate(X, radians = pi/2, center = c(0, 0))

    testthat::expect_equal(p, rbind(c(-1, 0), c(0, 1)))

    # Center is not origin
    X <- rbind(c(2, 4), c(4, 2))
    p <- predped::rotate(X, radians = pi/2, center = c(2, 2))

    testthat::expect_equal(p, rbind(c(0, 2), c(2, 4)))
})

################################################################################
# POLYGONS

# TO DO:
#   - Make initialization test
#   - Make `move` test

testthat::test_that("Polygon contains single point works", {
    # Shapes and points are chosen so that points are always contained within 
    # all shapes if they are in the object, or that they are to the left, right,
    # above, or below the shapes if not
    shapes <- list(predped::polygon(points = rbind(c(0,0), c(0,1), c(1,1), c(1,0)),
                                    clock_wise = TRUE),
                   predped::polygon(points = rbind(c(0,0), c(-0.5, 1), c(1.5,1.5), c(1,-0.5)),
                                    clock_wise = TRUE))
    
    points <- rbind(c(0.5, 0.5), c(-1, 0.5), c(2, 0.5), c(0.5, 2), c(0.5, -1))

    # Inside
    for(i in shapes) {
        testthat::expect_true(predped::in_object(i, points[1,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[2,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[3,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[4,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[5,], outside = FALSE))
    }

    # Outside
    for(i in shapes) {
        testthat::expect_false(predped::in_object(i, points[1,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[2,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[3,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[4,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[5,], outside = TRUE))
    }
})

testthat::test_that("Polygon contains multiple point works", {
    # Shapes and points are chosen so that points are always contained within 
    # all shapes if they are in the object, or that they are to the left, right,
    # above, or below the shapes if not
    shapes <- list(predped::polygon(points = rbind(c(0,0), c(0,1), c(1,1), c(1,0)),
                                    clock_wise = TRUE),
                   predped::polygon(points = rbind(c(0,0), c(-0.5, 1), c(1.5,1.5), c(1,-0.5)),
                                    clock_wise = TRUE))
    
    points <- rbind(c(0.5, 0.5), c(-1, 0.5), c(2, 0.5), c(0.5, 2), c(0.5, -1))

    # Inside
    ref <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
    for(i in shapes) {
        testthat::expect_equal(predped::in_object(i, points, outside = FALSE), ref)
    }

    # Outside
    ref <- !ref
    for(i in shapes) {
        testthat::expect_equal(predped::in_object(i, points, outside = TRUE), ref)
    }
})

testthat::test_that("Polygon intersection works", {
    rect <- predped::polygon(points = rbind(c(-1, -1), 
                                            c(-1, 1),
                                            c(1, 1),
                                            c(1, -1)))

    obj <- list(# Circles
                predped::circle(center = c(0, 0),
                                radius = 0.5),
                predped::circle(center = c(0, 0),
                                radius = 1.1),
                predped::circle(center = c(10, 0),
                                radius = 1),
                # Rectangles
                predped::rectangle(center = c(0, 0),
                                   size = c(1, 2),
                                   orientation = 45), 
                predped::rectangle(center = c(0, 10),
                                   size = c(2, 2)),
                predped::rectangle(center = c(0, 0),
                                   size = c(2, 2),
                                   orientation = 45), 
                predped::rectangle(center = c(0, 10),
                                   size = c(2, 2),
                                   orientation = 45),
                # Polygons
                predped::polygon(points = rbind(c(0, 0), 
                                                c(0.5, 4),
                                                c(1, -1))),
                predped::polygon(points = rbind(c(0, 4),
                                                c(0.5, 8),
                                                c(-1, 3))))

    tst <- lapply(obj,
                  \(x) predped::intersects(rect, x))
    ref <- list(FALSE, TRUE, FALSE,
                TRUE, FALSE, TRUE, FALSE, 
                TRUE, FALSE)

    # Actual tests
    testthat::expect_equal(tst, ref)
})


################################################################################
# RECTANGLES

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

testthat::test_that("Rectangle contains single point works", {
    # Shapes and points are chosen so that points are always contained within 
    # all shapes if they are in the object, or that they are to the left, right,
    # above, or below the shapes if not
    shapes <- list(predped::rectangle(center = c(0,0), size = c(1,1), orientation = 0),
                   predped::rectangle(center = c(0,0), size = c(1,1), orientation = pi/2))
    
    points <- rbind(c(0, 0), c(-1, 0), c(1, 0), c(0, 1), c(0, -1))

    # Inside
    for(i in shapes) {
        testthat::expect_true(predped::in_object(i, points[1,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[2,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[3,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[4,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[5,], outside = FALSE))
    }

    # Outside
    for(i in shapes) {
        testthat::expect_false(predped::in_object(i, points[1,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[2,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[3,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[4,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[5,], outside = TRUE))
    }
})

testthat::test_that("Rectangle contains multiple points works", {
    # Shapes and points are chosen so that points are always contained within 
    # all shapes if they are in the object, or that they are to the left, right,
    # above, or below the shapes if not
    shapes <- list(predped::rectangle(center = c(0,0), size = c(1,1), orientation = 0),
                   predped::rectangle(center = c(0,0), size = c(1,1), orientation = pi/2))
    
    points <- rbind(c(0, 0), c(-1, 0), c(1, 0), c(0, 1), c(0, -1))

    # Inside
    ref <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
    for(i in shapes) {
        testthat::expect_equal(predped::in_object(i, points, outside = FALSE), ref)
    }

    # Outside
    ref <- !ref
    for(i in shapes) {
        testthat::expect_equal(predped::in_object(i, points, outside = TRUE), ref)
    }
})

testthat::test_that("Rectangle random generation of point works", {
    r <- list(predped::rectangle(center = c(0, 0),
                                 size = c(2, 2)),
              predped::rectangle(center = c(0, 0), 
                                 size = c(2, 2), 
                                 orientation = pi / 4))

    # Middle point of edge
    ref1 <- list(c(-1, 0), 
                 c(0.7071, -0.7071))

    set.seed(1)
    tst1 <- lapply(r, \(x) round(rng_point(x), 4))

    # Random point on edge
    ref2 <- list(c(-1, -0.2558), 
                 c(1.2844, 0.1298))

    set.seed(1)
    tst2 <- lapply(r, \(x) round(rng_point(x, middle_edge = FALSE), 4))

    # Forbid all except one edge
    ref3 <- list(c(0, -1),
                 c(0.7071, -0.7071))

    set.seed(1)
    tst3 <- lapply(r, \(x) round(rng_point(x, forbidden = c(1, 2, 3)), 4))

    # Actual test
    testthat::expect_equal(tst1, ref1)
    testthat::expect_equal(tst2, ref2)
    testthat::expect_equal(tst3, ref3)
})

testthat::test_that("Rectangle intersection works", {
    rect <- predped::rectangle(center = c(0, 0), 
                               size = c(2, 2))

    obj <- list(# Circles
                predped::circle(center = c(0, 1), 
                                radius = 2),
                predped::circle(center = c(0, 10), 
                                radius = 2),
                # Rectangles
                # Commented out for now, but to solve later: Seems to be a special
                # case: Parallel lines not detected as intersecting, even when 
                # contained within themselves.
                # predped::rectangle(center = c(0, 1),
                #                    size = c(2, 2)), 
                predped::rectangle(center = c(0, 10),
                                   size = c(2, 2)),
                predped::rectangle(center = c(0, 1),
                                   size = c(2, 2),
                                   orientation = 45), 
                predped::rectangle(center = c(0, 10),
                                   size = c(2, 2),
                                   orientation = 45),
                # Polygons
                predped::polygon(points = rbind(c(0, 0), 
                                                c(0.5, 4),
                                                c(1, -1))),
                predped::polygon(points = rbind(c(0, 4),
                                                c(0.5, 8),
                                                c(-1, 3))))

    tst <- lapply(obj,
                  \(x) predped::intersects(rect, x))
    ref <- list(TRUE, FALSE, #TRUE, 
                FALSE, TRUE, FALSE, TRUE, FALSE)

    # Actual tests
    testthat::expect_equal(tst, ref)
})

################################################################################
# CIRCLES

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

testthat::test_that("Circle contains single point works", {
    # Shapes and points are chosen so that points are always contained within 
    # all shapes if they are in the object, or that they are to the left, right,
    # above, or below the shapes if not
    shapes <- list(predped::circle(center = c(0,0), radius = 2),
                   predped::circle(center = c(1,0), radius = 2))
    
    points <- rbind(c(0.5, 0), c(-3, 0), c(3, 0), c(0, 4), c(0, -3))

    # Inside
    for(i in shapes) {
        testthat::expect_true(predped::in_object(i, points[1,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[2,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[3,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[4,], outside = FALSE))
        testthat::expect_false(predped::in_object(i, points[5,], outside = FALSE))
    }

    # Outside
    for(i in shapes) {
        testthat::expect_false(predped::in_object(i, points[1,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[2,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[3,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[4,], outside = TRUE))
        testthat::expect_true(predped::in_object(i, points[5,], outside = TRUE))
    }
})

testthat::test_that("Circle contains multiple points works", {
    # Shapes and points are chosen so that points are always contained within 
    # all shapes if they are in the object, or that they are to the left, right,
    # above, or below the shapes if not
    shapes <- list(predped::circle(center = c(0,0), radius = 2),
                   predped::circle(center = c(1,0), radius = 2))
    
    points <- rbind(c(0.5, 0), c(-3, 0), c(3, 0), c(0, 4), c(0, -3))

    # Inside
    ref <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
    for(i in shapes) {
        testthat::expect_equal(predped::in_object(i, points, outside = FALSE), ref)
    }

    # Outside
    ref <- !ref
    for(i in shapes) {
        testthat::expect_equal(predped::in_object(i, points, outside = TRUE), ref)
    }
})

testthat::test_that("Circle random generation of point works", {
    r <- predped::circle(center = c(0, 0), 
                         radius = 2)

    # Unconstrained, constrained for one specific interval, constrained on
    # several intervals
    ref <- list(c(-0.1946, 1.9905), 
                c(1.5664, -1.2435),
                c(0.6231, -1.9005))

    set.seed(1)
    tst <- list(round(rng_point(r), 4),
                round(rng_point(r, forbidden = c(pi / 2, 3 * pi / 2)), 4),
                round(rng_point(r, forbidden = c(pi / 2, pi, pi, 3 * pi / 2)), 4))    

    # Do the same tests, but now check whether in a repeated procedure the actual
    # angles are never in the forbidden intervals
    sim <- matrix(0, nrow = 1000, ncol = 2)
    bounds <- matrix(c(pi / 4, pi / 2, 
                       3 * pi / 4, pi, 
                       3 * pi / 2, 2 * pi), 
                     ncol = 2, 
                     byrow = TRUE)
    for(i in 1:1000) {
        sim[i,] <- rng_point(r, forbidden = bounds)
    }

    # For each of these angles, check whether they are inside of the bounds
    angles <- atan2(sim[,2] / r@radius, sim[,1] / r@radius)
    inside <- 
        (bounds[1,1] < angles & angles < bounds[1,2]) |
        (bounds[2,1] < angles & angles < bounds[2,2]) |
        (bounds[3,1] < angles & angles < bounds[3,2])

    # Actual tests
    testthat::expect_equal(tst, ref)
    testthat::expect_false(any(inside)) # Also nice to see plotted: plot(sim)
})

testthat::test_that("Circle intersection works", {
    circ <- predped::circle(center = c(0, 0), 
                            radius = 2)

    obj <- list(# Circles
                predped::circle(center = c(0, 1), 
                                radius = 2),
                predped::circle(center = c(0, 10), 
                                radius = 2),
                # Rectangles
                predped::rectangle(center = c(0, 1),
                                   size = c(2, 2)), 
                predped::rectangle(center = c(0, 10),
                                   size = c(2, 2)),
                predped::rectangle(center = c(0, 1),
                                   size = c(2, 2),
                                   orientation = 45), 
                predped::rectangle(center = c(0, 10),
                                   size = c(2, 2),
                                   orientation = 45),
                # Polygons
                predped::polygon(points = rbind(c(0, 0), 
                                                c(0.5, 4),
                                                c(1, -1))),
                predped::polygon(points = rbind(c(0, 4),
                                                c(0.5, 8),
                                                c(-1, 3))))

    tst <- lapply(obj,
                  \(x) predped::intersects(circ, x))
    ref <- list(TRUE, FALSE, 
                TRUE, FALSE, TRUE, FALSE,
                TRUE, FALSE)

    # Actual tests
    testthat::expect_equal(tst, ref)
})
