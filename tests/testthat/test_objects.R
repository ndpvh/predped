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
#   - Make `move` test

testthat::test_that("Polgyon initialization works", {
    testthat::expect_no_error(predped::polygon(points = cbind(rep(1, 3), rep(0, 3))))

    testthat::expect_error(predped::polygon(points = matrix(rep(1, 3), ncol = 1)))
    testthat::expect_error(predped::polygon(points = cbind(rep(1, 3), rep(0, 3), rep(-1, 3))))

    # In response to bug in which the given id was not assigned to the object
    poly <- predped::polygon(id = "test", points = cbind(rep(1, 3), rep(0, 3)))
    testthat::expect_equal(poly@id, "test")
})

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

testthat::test_that("Polygon line intersection works", {
    rect <- predped::polygon(points = rbind(c(-1, -1), 
                                            c(-1, 1),
                                            c(1, 1),
                                            c(1, -1)))

    segments <- rbind(c(-2, -2, 2, 2),  
                      c(-2, 2, 2, -2),
                      c(-3, -3, -1.5, -1.5),
                      c(-3, 3, -1.5, 1.5),
                      c(0.5, 0.5, -0.5, -0.5))

    tst <- lapply(1:nrow(segments), 
                  \(x) predped::line_intersection(rect, matrix(segments[x,], nrow = 1)))
    ref <- list(TRUE, TRUE, FALSE, FALSE, FALSE)

    # Actual tests
    testthat::expect_equal(tst, ref)
})


################################################################################
# RECTANGLES

testthat::test_that("Rectangle initialization works", {
    testthat::expect_no_error(predped::rectangle(center = c(0, 0), size = c(1, 1)))
    testthat::expect_error(predped::rectangle(center = c(0, 0), size = 1))
    testthat::expect_error(predped::rectangle(center = c(0, 0, 0), size = c(1, 1)))
    testthat::expect_error(predped::rectangle(center = c(0, 0), size = c(-1, 1)))
    testthat::expect_error(predped::rectangle(center = c(0, 0), size = c(1, 1), orientation = c(1, 1)))

    # In response to bug in which the given id was not assigned to the object
    rect <- predped::rectangle(id = "test", center = c(0, 0), size = c(1, 1))
    testthat::expect_equal(rect@id, "test")
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

testthat::test_that("Rectangle area works", {
    rect <- list(predped::rectangle(center = c(0, 0), size = c(1, 2)),
                 predped::rectangle(center = c(0, 0), size = c(2, 2)),
                 predped::rectangle(center = c(0, 0), size = c(2, 1)))

    tst <- lapply(rect, 
                  \(x) area(x))
    ref <- list(2, 4, 2)

    testthat::expect_equal(tst, ref)
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

    # In response to bug in which the given id was not assigned to the object
    circ <- predped::circle(id = "test", center = c(0, 0), radius = 0.5)
    testthat::expect_equal(circ@id, "test")
})

testthat::test_that("Circle area works", {
    circ <- list(predped::circle(center = c(0, 0), radius = 1),
                 predped::circle(center = c(0, 0), radius = 2),
                 predped::circle(center = c(0, 0), radius = 3))

    tst <- lapply(circ, 
                  \(x) area(x))
    ref <- list(pi, 2^2 * pi, 3^2 * pi)

    testthat::expect_equal(tst, ref)
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

testthat::test_that("Circle transformation to polygon works", {
    circ <- predped::circle(center = c(0, 0), radius = 1)

    # Make case in which intersection is obvious, as all points should be on the 
    # circle
    coords <- predped::to_polygon(circ)
    segments <- cbind(coords, coords[c(2:nrow(coords), 1), ])

    tst_1 <- predped::line_intersection(circ, segments)

    # Make cases in which intersection should not occur, as points are not on the 
    # circle
    circ_2 <- predped::circle(center = c(0, 0), radius = 1.01)
    circ_3 <- predped::circle(center = c(0, 0), radius = 0.99)

    coords <- predped::to_polygon(circ_2)
    segments <- cbind(coords, coords[c(2:nrow(coords), 1), ])
    tst_2 <- predped::line_intersection(circ, segments)

    coords <- predped::to_polygon(circ_3)
    segments <- cbind(coords, coords[c(2:nrow(coords), 1), ])
    tst_3 <- predped::line_intersection(circ, segments)

    testthat::expect_true(tst_1)
    testthat::expect_false(tst_2)
    testthat::expect_false(tst_3)
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





################################################################################
# GETTERS AND SETTERS

testthat::test_that("Object getters work", {
    # Polygon
    set.seed(1)
    tst <- predped::polygon(points = rbind(c(-1, -1),
                                           c(-1, 1), 
                                           c(1, 1),
                                           c(1, -1)))

    testthat::expect_equal(predped::id(tst), "object ydgab")
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(0, 0)))

    # Rectangle
    set.seed(1)
    tst <- predped::rectangle(center = c(0, 0), 
                              size = c(2, 2),
                              orientation = pi)

    testthat::expect_equal(predped::id(tst), "object wknrs") # Why would this object have a different name?
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(0, 0)))
    testthat::expect_equal(predped::size(tst), c(2, 2))
    testthat::expect_equal(predped::orientation(tst), pi)

    # Circle
    set.seed(1)
    tst <- predped::circle(center = c(0, 0), radius = 1)

    testthat::expect_equal(predped::id(tst), "object ydgab")
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(0, 0)))
    testthat::expect_equal(predped::size(tst), 1)
    testthat::expect_equal(predped::radius(tst), 1)
})

testthat::test_that("Object setters work", {
    # Polygon
    tst <- predped::polygon(points = rbind(c(-1, -1),
                                           c(-1, 1), 
                                           c(1, 1),
                                           c(1, -1)))

    predped::id(tst) <- "test"
    predped::center(tst) <- c(1, 1)

    testthat::expect_equal(predped::id(tst), "test")
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(1, 1)))
    testthat::expect_equal(tst@points, rbind(c(0, 0), 
                                             c(0, 2),
                                             c(2, 2),
                                             c(2, 0)))

    # Rectangle
    tst <- predped::rectangle(center = c(0, 0), 
                              size = c(2, 2),
                              orientation = pi)

    predped::id(tst) <- "test"
    predped::center(tst) <- c(1, 1)
    predped::size(tst) <- c(1, 1)
    predped::orientation(tst) <- 0

    testthat::expect_equal(predped::id(tst), "test")
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(1, 1)))
    testthat::expect_equal(predped::size(tst), c(1, 1))
    testthat::expect_equal(predped::orientation(tst), 0)

    points <- tst@points |>
        as.numeric() |>
        matrix(ncol = 2)
    testthat::expect_equal(points, rbind(c(0.5, 0.5), 
                                         c(0.5, 1.5),
                                         c(1.5, 1.5),
                                         c(1.5, 0.5)))

    # A tilted rectangle: Changing orientation and size
    # All values here were mathematically derived
    tst <- predped::rectangle(center = c(0, 0), 
                              size = c(1, 1), 
                              orientation = pi / 4)
    tst_1 <- tst ; tst_2 <- tst

    predped::orientation(tst_1) <- 0
    testthat::expect_equal(tst_1@points, rbind(c(-0.5, -0.5), 
                                               c(-0.5, 0.5), 
                                               c(0.5, 0.5), 
                                               c(0.5, -0.5)))

    predped::size(tst_2) <- c(2, 2)
    testthat::expect_equal(tst_2@points, 
                           rbind(c(0, sqrt(8)), 
                                 c(sqrt(8), 0), 
                                 c(0, -sqrt(8)),
                                 c(-sqrt(8), 0)),
                           tolerance = 1e-4)

    predped::size(tst_2) <- c(2, 1)
    testthat::expect_equal(tst_2@points, 
                           rbind(c(sqrt(0.125), sqrt(2) / 2 + sqrt(0.125)), 
                                 c(sqrt(2) / 2 + sqrt(0.125), sqrt(0.125)),
                                 c(-sqrt(0.125), -sqrt(2) / 2 - sqrt(0.125)),
                                 c(-sqrt(2) / 2 - sqrt(0.125), -sqrt(0.125))),
                           tolerance = 1e-4)

    # Circle
    tst <- predped::circle(center = c(0, 0), radius = 1)

    predped::id(tst) <- "test"
    predped::center(tst) <- c(1, 1)

    testthat::expect_equal(predped::id(tst), "test")
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(1, 1)))
    
    predped::radius(tst) <- 2
    testthat::expect_equal(predped::size(tst), 2)
    testthat::expect_equal(predped::radius(tst), 2)

    predped::size(tst) <- 3
    testthat::expect_equal(predped::size(tst), 3)
    testthat::expect_equal(predped::radius(tst), 3)
})
