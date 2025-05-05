################################################################################
# INITIALIZE

testthat::test_that("Object is abstract base class", {
    testthat::expect_error(new("object"))
})

testthat::test_that("Coordinate initialization works", {
    ref <- c(1, 1)
    p <- predped::coordinate(ref)
    testthat::expect_true(all(p == ref))

    testthat::expect_error(predped::coordinate(c(1, 1, 1)))
    testthat::expect_error(predped::coordinate(""))
})

testthat::test_that("Polgyon initialization works", {
    testthat::expect_no_error(predped::polygon(points = cbind(rep(1, 3), rep(0, 3))))

    testthat::expect_error(predped::polygon(points = matrix(rep(1, 3), ncol = 1)))
    testthat::expect_error(predped::polygon(points = cbind(rep(1, 3), rep(0, 3), rep(-1, 3))))

    # In response to bug in which the given id was not assigned to the object
    poly <- predped::polygon(id = "test", points = cbind(rep(1, 3), rep(0, 3)))
    testthat::expect_equal(poly@id, "test")
})

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

testthat::test_that("Circle initialization works", {
    testthat::expect_no_error(predped::circle(center = c(0, 0), radius = 0.5))
    testthat::expect_error(predped::rectangle(center = c(0, 0), radius = c(0.5, 0.5)))

    # In response to bug in which the given id was not assigned to the object
    circ <- predped::circle(id = "test", center = c(0, 0), radius = 0.5)
    testthat::expect_equal(circ@id, "test")
})

testthat::test_that("Segment initialization works", {
    testthat::expect_no_error(predped::segment(from = c(0, 0), to = c(0, 0)))
    testthat::expect_error(predped::segment(from = c(0, 0)))
    testthat::expect_error(predped::segment(to = c(0, 0)))
})





################################################################################
# GETTERS AND SETTERS

testthat::test_that("Polygon getters work", {
    # Polygon
    set.seed(1)
    tst <- predped::polygon(points = rbind(c(-1, -1),
                                           c(-1, 1), 
                                           c(1, 1),
                                           c(1, -1)))

    testthat::expect_equal(predped::id(tst), "object ydgab")
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(0, 0)))
    testthat::expect_equal(predped::points(tst), rbind(c(-1, -1),
                                                       c(-1, 1), 
                                                       c(1, 1),
                                                       c(1, -1)))
})

testthat::test_that("Polygon setters work", {
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
})

testthat::test_that("Rectangle getters work", {
    # Rectangle
    set.seed(1)
    tst <- predped::rectangle(center = c(0, 0), 
                              size = c(2, 2),
                              orientation = pi)

    testthat::expect_equal(predped::id(tst), "object ydgab")
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(0, 0)))
    testthat::expect_equal(predped::size(tst), c(2, 2))
    testthat::expect_equal(predped::orientation(tst), pi)
    testthat::expect_equal(predped::points(tst), rbind(c(1, 1),
                                                       c(1, -1), 
                                                       c(-1, -1),
                                                       c(-1, 1)))
})

testthat::test_that("Rectangle setters work", {
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
    points <- tst_1@points |>
        as.numeric() |>
        matrix(ncol = 2)
    testthat::expect_equal(points, rbind(c(-0.5, -0.5), 
                                         c(-0.5, 0.5), 
                                         c(0.5, 0.5), 
                                         c(0.5, -0.5)))

    predped::size(tst_2) <- c(2, 2)
    points <- tst_2@points |>
        as.numeric() |>
        matrix(ncol = 2)
    testthat::expect_equal(points, 
                           rbind(c(0, -sqrt(8) / 2), 
                                 c(-sqrt(8) / 2, 0), 
                                 c(0, sqrt(8) / 2),
                                 c(sqrt(8) / 2, 0)),
                           tolerance = 1e-4)

    predped::size(tst_2) <- c(2, 1)
    points <- tst_2@points |>
        as.numeric() |>
        matrix(ncol = 2)
    testthat::expect_equal(points, 
                           rbind(c(-sqrt(0.125), -(sqrt(2) / 2 + sqrt(0.125))), 
                                 c(-(sqrt(2) / 2 + sqrt(0.125)), -sqrt(0.125)),
                                 c(sqrt(0.125), sqrt(2) / 2 + sqrt(0.125)),
                                 c(sqrt(2) / 2 + sqrt(0.125), sqrt(0.125))),
                           tolerance = 1e-4)
})

testthat::test_that("Circle getters work", {
    # Circle
    set.seed(1)
    tst <- predped::circle(center = c(0, 0), radius = 1)
    angles <- seq(0, 2 * pi, length.out = 101)[1:100]

    testthat::expect_equal(predped::id(tst), "object ydgab")
    testthat::expect_equal(predped::center(tst), predped::coordinate(c(0, 0)))
    testthat::expect_equal(predped::size(tst), 1)
    testthat::expect_equal(predped::radius(tst), 1)
    testthat::expect_equal(as.matrix(predped::points(tst)), 
                           matrix(c(cos(angles), sin(angles)), ncol = 2))
})

testthat::test_that("Circle setters work", {
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

testthat::test_that("Circle transformation to polygon works", {
    circ <- predped::circle(center = c(0, 0), radius = 1)

    # Make case in which intersection is obvious, as all points should be on the 
    # circle
    coords <- predped::points(circ)
    segments <- cbind(coords, coords[c(2:nrow(coords), 1), ])

    tst_1 <- predped::line_intersection(circ, segments)

    # Make cases in which intersection should not occur, as points are not on the 
    # circle
    circ_2 <- predped::circle(center = c(0, 0), radius = 1.01)
    circ_3 <- predped::circle(center = c(0, 0), radius = 0.99)

    coords <- predped::points(circ_2)
    segments <- cbind(coords, coords[c(2:nrow(coords), 1), ])
    tst_2 <- predped::line_intersection(circ, segments)

    coords <- predped::points(circ_3)
    segments <- cbind(coords, coords[c(2:nrow(coords), 1), ])
    tst_3 <- predped::line_intersection(circ, segments)

    testthat::expect_true(tst_1)
    testthat::expect_false(tst_2)
    testthat::expect_false(tst_3)
})

testthat::test_that("Segment getters work", {
    # Segment
    set.seed(1)
    tst <- predped::segment(from = c(-1, -1), to = c(1, 1))

    testthat::expect_equal(predped::id(tst), "object ydgab")
    testthat::expect_equal(predped::center(tst), c(0, 0))
    testthat::expect_equal(predped::size(tst), 2 * sqrt(2))
    testthat::expect_equal(predped::from(tst), c(-1, -1))
    testthat::expect_equal(predped::to(tst), c(1, 1))
    testthat::expect_equal(predped::orientation(tst), pi / 4)
})

testthat::test_that("Segment setters work", {
    # Segment
    tst <- predped::segment(from = c(-1, -1), to = c(1, 1))

    predped::id(tst) <- "test"
    predped::center(tst) <- c(1, 1)

    testthat::expect_equal(predped::id(tst), "test")
    testthat::expect_equal(predped::center(tst), c(1, 1))
    testthat::expect_equal(predped::from(tst), c(0, 0))
    testthat::expect_equal(predped::to(tst), c(2, 2))
    
    predped::from(tst) <- c(-2, -2)
    predped::to(tst) <- c(3, 3)
    testthat::expect_equal(predped::from(tst), c(-2, -2))
    testthat::expect_equal(predped::to(tst), c(3, 3))
    testthat::expect_equal(predped::size(tst), 5 * sqrt(2))
    testthat::expect_equal(predped::center(tst), c(0.5, 0.5))

    predped::size(tst) <- sqrt(2)
    testthat::expect_equal(predped::size(tst), sqrt(2))
    testthat::expect_equal(predped::from(tst), c(-2, -2))
    testthat::expect_equal(predped::to(tst), c(-1, -1))
    testthat::expect_equal(predped::center(tst), c(-1.5, -1.5))

    predped::orientation(tst) <- pi / 2 + pi / 4
    testthat::expect_equal(predped::from(tst), c(-1, -2))
    testthat::expect_equal(predped::to(tst), c(-2, -1))
})





################################################################################
# ROTATE

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

testthat::test_that("Polygon rotation around own center works", {
    # Create a reference for rotation around the polgyon's center
    ref <- rbind(c(1.5, 0.5),
                 c(-0.5, 0.5),
                 c(-0.5, 1.5),
                 c(1.5, 1.5))

    # Create a polygon to be rotated
    p <- predped::polygon(points = rbind(c(0, 0), 
                                         c(0, 2), 
                                         c(1, 2), 
                                         c(1, 0)))

    # Rotation based on radians
    rotated <- predped::rotate(p, radians = pi / 2)
    testthat::expect_equal(predped::points(rotated), ref)

    # Rotation based on degrees
    rotated <- predped::rotate(p, degrees = 90)
    testthat::expect_equal(predped::points(rotated), ref)

    # Rotation based on degrees, but both provided
    rotated <- predped::rotate(p, radians = pi / 2, degrees = 90)
    testthat::expect_equal(predped::points(rotated), ref)
})

testthat::test_that("Polygon rotation around other center works", {
    # Create a reference for rotation around the origin
    ref <- rbind(c(0, 0), 
                 c(-2, 0), 
                 c(-2, 1),
                 c(0, 1))

    # Create a polygon to be rotated
    p <- predped::polygon(points = rbind(c(0, 0), 
                                         c(0, 2), 
                                         c(1, 2), 
                                         c(1, 0)))

    # Rotation based on radians
    rotated <- predped::rotate(p, radians = pi / 2, center = c(0, 0))
    testthat::expect_equal(predped::points(rotated), ref)

    # Rotation based on degrees
    rotated <- predped::rotate(p, degrees = 90, center = c(0, 0))
    testthat::expect_equal(predped::points(rotated), ref)

    # Rotation based on degrees, but both provided
    rotated <- predped::rotate(p, radians = pi / 2, degrees = 90, center = c(0, 0))
    testthat::expect_equal(predped::points(rotated), ref)
})

testthat::test_that("Rectangle rotation around own center works", {
    # Create a reference for rotation around the rectangles center
    ref <- rbind(c(1.5, 0.5),
                 c(-0.5, 0.5),
                 c(-0.5, 1.5),
                 c(1.5, 1.5))

    # Create a rectangle to be rotated
    r <- predped::rectangle(center = c(0.5, 1), size = c(1, 2))

    # Rotation based on radians
    rotated <- predped::rotate(r, radians = pi / 2)
    testthat::expect_equal(predped::points(rotated), ref)

    # Rotation based on degrees
    rotated <- predped::rotate(r, degrees = 90)
    testthat::expect_equal(predped::points(rotated), ref)

    # Rotation based on degrees, but both provided
    rotated <- predped::rotate(r, radians = pi / 2, degrees = 90)
    testthat::expect_equal(predped::points(rotated), ref)
})

testthat::test_that("Rectangle rotation around other center works", {
    # Create a reference for rotation around the origin
    ref <- rbind(c(0, 0), 
                 c(-2, 0), 
                 c(-2, 1),
                 c(0, 1))

    # Create a rectangle to be rotated
    r <- predped::rectangle(center = c(0.5, 1), size = c(1, 2))

    # Rotation based on radians
    rotated <- predped::rotate(r, radians = pi / 2, center = c(0, 0))
    testthat::expect_equal(predped::points(rotated), ref)

    # Rotation based on degrees
    rotated <- predped::rotate(r, degrees = 90, center = c(0, 0))
    testthat::expect_equal(predped::points(rotated), ref)

    # Rotation based on degrees, but both provided
    rotated <- predped::rotate(r, radians = pi / 2, degrees = 90, center = c(0, 0))
    testthat::expect_equal(predped::points(rotated), ref)
})

testthat::test_that("Segment rotation around own center works", {
    # Create a reference for rotation around the segment's center
    ref_from <- c(1, 0)
    ref_to <- c(0, 1)

    # Create a segment to be rotated
    s <- predped::segment(from = c(0, 0), to = c(1, 1))

    # Rotation based on radians
    rotated <- predped::rotate(s, radians = pi / 2)
    testthat::expect_equal(predped::from(rotated), ref_from)
    testthat::expect_equal(predped::to(rotated), ref_to)

    # Rotation based on degrees
    rotated <- predped::rotate(s, degrees = 90)
    testthat::expect_equal(predped::from(rotated), ref_from)
    testthat::expect_equal(predped::to(rotated), ref_to)

    # Rotation based on degrees, but both provided
    rotated <- predped::rotate(s, radians = pi / 2, degrees = 90)
    testthat::expect_equal(predped::from(rotated), ref_from)
    testthat::expect_equal(predped::to(rotated), ref_to)
})

testthat::test_that("Segment rotation around other center works", {
    # Create a reference for rotation around the segment's center
    ref_from <- c(0, 0)
    ref_to <- c(-1, 1)

    # Create a segment to be rotated
    s <- predped::segment(from = c(0, 0), to = c(1, 1))

    # Rotation based on radians
    rotated <- predped::rotate(s, radians = pi / 2, center = c(0, 0))
    testthat::expect_equal(predped::from(rotated), ref_from)
    testthat::expect_equal(predped::to(rotated), ref_to)

    # Rotation based on degrees
    rotated <- predped::rotate(s, degrees = 90, center = c(0, 0))
    testthat::expect_equal(predped::from(rotated), ref_from)
    testthat::expect_equal(predped::to(rotated), ref_to)

    # Rotation based on degrees, but both provided
    rotated <- predped::rotate(s, radians = pi / 2, degrees = 90, center = c(0, 0))
    testthat::expect_equal(predped::from(rotated), ref_from)
    testthat::expect_equal(predped::to(rotated), ref_to)
})





################################################################################
# MOVE

testthat::test_that("Polygon moving works", {
    # Create a rectangle to move
    p <- predped::polygon(points = rbind(c(0.5, 0.5),
                                         c(0.5, -0.5), 
                                         c(-0.5, -0.5), 
                                         c(-0.5, 0.5)),
                          moveable = TRUE)

    moved <- predped::move(p, c(1, 1))

    testthat::expect_equal(predped::center(moved), c(1, 1))
    testthat::expect_equal(predped::points(moved), predped::points(p) + c(1, 1))

    # Create an immobile rectangle
    p <- predped::polygon(points = rbind(c(0.5, 0.5),
                                         c(0.5, -0.5), 
                                         c(-0.5, -0.5), 
                                         c(-0.5, 0.5)),
                          moveable = FALSE) 

    moved <- predped::move(p, c(1, 1)) |>
        suppressWarnings()

    testthat::expect_equal(predped::center(moved), predped::coordinate(c(0, 0)))
    testthat::expect_equal(predped::points(moved), predped::points(p))

    # When immobile, we should get a warning
    testthat::expect_warning(predped::move(p, c(1, 1)))
})

testthat::test_that("Rectangle moving works", {
    # Create a rectangle to move
    r <- predped::rectangle(center = c(0, 0), 
                            size = c(1, 1), 
                            moveable = TRUE)
    moved <- predped::move(r, c(1, 1))

    testthat::expect_equal(predped::center(moved), c(1, 1))
    testthat::expect_equal(predped::points(moved), predped::points(r) + c(1, 1))

    # Create an immobile rectangle
    r <- predped::rectangle(center = c(0, 0), 
                            size = c(1, 1), 
                            moveable = FALSE)
    moved <- suppressWarnings(predped::move(r, c(1, 1)))

    testthat::expect_equal(predped::center(moved), predped::coordinate(c(0, 0)))
    testthat::expect_equal(predped::points(moved), predped::points(r))

    # When immobile, we should get a warning
    testthat::expect_warning(predped::move(r, c(1, 1)))
})

testthat::test_that("Circle moving works", {
    # Create a circle to move
    c <- predped::circle(center = c(0, 0), 
                         radius = 0.5, 
                         moveable = TRUE)

    moved <- predped::move(c, c(1, 1))
    
    testthat::expect_equal(center(moved), c(1, 1))

    # Create an immobile circle
    c <- predped::circle(center = c(0, 0), 
                         radius = 0.5, 
                         moveable = FALSE)
    moved <- predped::move(c, c(1, 1)) |>
        suppressWarnings()

    testthat::expect_equal(center(moved), predped::coordinate(c(0, 0)))

    # When immobile, we should get a warning
    testthat::expect_warning(predped::move(c, c(1, 1)))
})

testthat::test_that("Segment moving works", {
    # Create a circle to move
    s <- predped::segment(from = c(-2, -2), 
                          to = c(2, 2), 
                          moveable = TRUE)

    moved <- predped::move(s, c(1, 1))
    
    testthat::expect_equal(center(moved), c(1, 1))
    testthat::expect_equal(from(moved), c(-1, -1))
    testthat::expect_equal(to(moved), c(3, 3))

    # Create an immobile circle
    s <- predped::segment(from = c(-2, -2), 
                          to = c(2, 2), 
                          moveable = FALSE)
    moved <- predped::move(s, c(1, 1)) |>
        suppressWarnings()

    testthat::expect_equal(center(moved), c(0, 0))
    testthat::expect_equal(from(moved), c(-2, -2))
    testthat::expect_equal(to(moved), c(2, 2))

    # When immobile, we should get a warning
    testthat::expect_warning(predped::move(s, c(1, 1)))
})





################################################################################
# AREA

testthat::test_that("Rectangle area works", {
    rect <- list(predped::rectangle(center = c(0, 0), size = c(1, 2)),
                 predped::rectangle(center = c(0, 0), size = c(2, 2)),
                 predped::rectangle(center = c(0, 0), size = c(2, 1)))

    tst <- lapply(rect, 
                  \(x) area(x))
    ref <- list(2, 4, 2)

    testthat::expect_equal(tst, ref)
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





################################################################################
# IN_OBJECT/OUT_OBJECT

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
        testthat::expect_true(predped::in_object(i, points[1,]))
        testthat::expect_false(predped::in_object(i, points[2,]))
        testthat::expect_false(predped::in_object(i, points[3,]))
        testthat::expect_false(predped::in_object(i, points[4,]))
        testthat::expect_false(predped::in_object(i, points[5,]))
    }

    # Outside
    for(i in shapes) {
        testthat::expect_false(predped::out_object(i, points[1,]))
        testthat::expect_true(predped::out_object(i, points[2,]))
        testthat::expect_true(predped::out_object(i, points[3,]))
        testthat::expect_true(predped::out_object(i, points[4,]))
        testthat::expect_true(predped::out_object(i, points[5,]))
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
        testthat::expect_equal(predped::in_object(i, points), ref)
    }

    # Outside
    ref <- !ref
    for(i in shapes) {
        testthat::expect_equal(predped::out_object(i, points), ref)
    }
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
        testthat::expect_true(predped::in_object(i, points[1,]))
        testthat::expect_false(predped::in_object(i, points[2,]))
        testthat::expect_false(predped::in_object(i, points[3,]))
        testthat::expect_false(predped::in_object(i, points[4,]))
        testthat::expect_false(predped::in_object(i, points[5,]))
    }

    # Outside
    for(i in shapes) {
        testthat::expect_false(predped::out_object(i, points[1,]))
        testthat::expect_true(predped::out_object(i, points[2,]))
        testthat::expect_true(predped::out_object(i, points[3,]))
        testthat::expect_true(predped::out_object(i, points[4,]))
        testthat::expect_true(predped::out_object(i, points[5,]))
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
        testthat::expect_equal(predped::in_object(i, points), ref)
    }

    # Outside
    ref <- !ref
    for(i in shapes) {
        testthat::expect_equal(predped::out_object(i, points), ref)
    }
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
        testthat::expect_true(predped::in_object(i, points[1,]))
        testthat::expect_false(predped::in_object(i, points[2,]))
        testthat::expect_false(predped::in_object(i, points[3,]))
        testthat::expect_false(predped::in_object(i, points[4,]))
        testthat::expect_false(predped::in_object(i, points[5,]))
    }

    # Outside
    for(i in shapes) {
        testthat::expect_false(predped::out_object(i, points[1,]))
        testthat::expect_true(predped::out_object(i, points[2,]))
        testthat::expect_true(predped::out_object(i, points[3,]))
        testthat::expect_true(predped::out_object(i, points[4,]))
        testthat::expect_true(predped::out_object(i, points[5,]))
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
        testthat::expect_equal(predped::in_object(i, points), ref)
    }

    # Outside
    ref <- !ref
    for(i in shapes) {
        testthat::expect_equal(predped::out_object(i, points), ref)
    }
})

testthat::test_that("Segment contains single point works", {
    # Create the shapes and the points to be evaluated
    shapes <- list(predped::segment(from = c(0, 0), to = c(1, 1)),
                   predped::segment(from = c(0, 0), to = c(1, 0)),
                   predped::segment(from = c(0, 0), to = c(0, 1)))
    
    points <- rbind(c(0.5, 0.5), c(0.5, 0), c(0, 0.5), c(2, 2), c(2, 0), c(0, 2))

    # Inside
    ref <- list(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 
                c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE), 
                c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
    tst <- lapply(shapes, 
                  \(x) sapply(seq_len(nrow(points)), 
                              \(i) predped::in_object(x, points[i,])))
                              
    testthat::expect_equal(tst, ref)

    # Outside
    ref <- lapply(ref, \(x) !x)
    tst <- lapply(shapes, 
                  \(x) sapply(seq_len(nrow(points)), 
                              \(i) predped::out_object(x, points[i,])))
                              
    testthat::expect_equal(tst, ref)
})

testthat::test_that("Segment contains multiple points works", {
    # Create the shapes and the points to be evaluated
    shapes <- list(predped::segment(from = c(0, 0), to = c(1, 1)),
                   predped::segment(from = c(0, 0), to = c(1, 0)),
                   predped::segment(from = c(0, 0), to = c(0, 1)))
    
    points <- rbind(c(0.5, 0.5), c(0.5, 0), c(0, 0.5), c(2, 2), c(2, 0), c(0, 2))

    # Inside
    ref <- list(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 
                c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE), 
                c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
    tst <- lapply(shapes, 
                  \(x) predped::in_object(x, points))
                              
    testthat::expect_equal(tst, ref)

    # Outside
    ref <- lapply(ref, \(x) !x)
    tst <- lapply(shapes, 
                  \(x) predped::out_object(x, points))
                              
    testthat::expect_equal(tst, ref)
})

testthat::test_that("Results in_object converge between R and Rcpp", {
    # Create the different shapes that should be tested.
    shapes <- list(predped::polygon(points = cbind(c(-5, -5, 5, 5, 0, 0),
                                                   c(-5, 5, 5, 0, 0, -5))),
                   predped::polygon(points = cbind(c(-5, -5, 5, 5, 3, 3, -3, -3),
                                                   c(-5, 5, 5, -5, -5, 3, 3, -5))),
                   predped::rectangle(center = c(-2.5, -2.5), size = c(5, 5)),
                   predped::rectangle(center = c(2.5, 2.5), size = c(5, 5)),
                   predped::rectangle(center = c(-2.5, -2.5), size = c(5, 5), orientation = pi/4),
                   predped::rectangle(center = c(2.5, 2.5), size = c(5, 5), orientation = pi/4),
                   predped::circle(center = c(-2.5, -2.5), radius = 2.5),
                   predped::circle(center = c(2.5, 2.5), radius = 2.5))
    
    points <- cbind(rep(seq(-5, 5, 0.5), each = 21),
                    rep(seq(-5, 5, 0.5), times = 21))

    # Create the results for the R and Rcpp versions and compare (in_object)
    ref <- lapply(shapes, \(x) predped::in_object(x, points, cpp = FALSE))
    tst <- lapply(shapes, \(x) predped::in_object(x, points, cpp = TRUE))

    testthat::expect_equal(tst, ref)

    # Create the results for the R and Rcpp versions and compare (out_object)
    ref <- lapply(shapes, \(x) predped::out_object(x, points, cpp = FALSE))
    tst <- lapply(shapes, \(x) predped::out_object(x, points, cpp = TRUE))

    testthat::expect_equal(tst, ref)
})





################################################################################
# ENLARGE

testthat::test_that("Enlarging object works", {
    # Create a list of objects
    objects <- list(predped::circle(center = c(0, 0), 
                                    radius = 1), 
                    predped::rectangle(center = c(2, 2), 
                                       size = c(1, 1)), 
                    predped::polygon(points = rbind(c(-2.5, -1.5), 
                                                    c(-1.5, -1.5),
                                                    c(-1.5, -2.5), 
                                                    c(-2.5, -2.5))))

    # Do the test
    larger_objects <- lapply(objects, 
                             \(x) predped:::enlarge(x, 1))

    testthat::expect_equal(radius(larger_objects[[1]]), 2, tolerance = 1e-2)
    testthat::expect_equal(size(larger_objects[[2]]), rep(1 + 2 * 0.707, 2), tolerance = 1e-2)
    testthat::expect_equal(diff(range(larger_objects[[3]]@points[,1])), 1 + 2 * 0.707, tolerance = 1e-2)
    testthat::expect_equal(diff(range(larger_objects[[3]]@points[,2])), 1 + 2 * 0.707, tolerance = 1e-2)
})




################################################################################
# RNG_POINT

testthat::test_that("Polygon random generation of point works", {
    p <- predped::polygon(points = rbind(c(1, 1), 
                                         c(1, -1), 
                                         c(-1, -1), 
                                         c(-1, 1)))

    # Middle point of edge
    ref1 <- c(1, 0)

    set.seed(1)
    tst1 <- round(rng_point(p), 4)

    # Random point on edge
    ref2 <- c(1, 0.2558)

    set.seed(1)
    tst2 <- round(rng_point(p, middle_edge = FALSE), 4)

    # Forbid all except one edge
    ref3 <- c(0, 1)

    set.seed(1)
    forbidden(p) <- c(1, 2, 3)
    tst3 <- round(rng_point(p), 4)

    # Actual test
    testthat::expect_equal(tst1, ref1)
    testthat::expect_equal(tst2, ref2)
    testthat::expect_equal(tst3, ref3)
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
    r <- lapply(r, \(x) {forbidden(x) <- c(1, 2, 3) ; return(x)})
    tst3 <- lapply(r, \(x) round(rng_point(x), 4))

    # Actual test
    testthat::expect_equal(tst1, ref1)
    testthat::expect_equal(tst2, ref2)
    testthat::expect_equal(tst3, ref3)
})

testthat::test_that("Circle random generation of point works", {
    r <- predped::circle(center = c(0, 0), 
                         radius = 2)

    r2 <- r3 <- r 
    forbidden(r2) <- matrix(c(pi / 2, 3 * pi / 2), nrow = 1)
    forbidden(r3) <- matrix(c(pi / 2, pi, pi, 3 * pi / 2), nrow = 1)

    # Unconstrained, constrained for one specific interval, constrained on
    # several intervals
    ref <- list(c(-0.1946, 1.9905), 
                c(1.5664, -1.2435),
                c(0.6231, -1.9005))

    set.seed(1)
    tst <- list(round(rng_point(r), 4),
                round(rng_point(r2), 4),
                round(rng_point(r3), 4))    

    # Do the same tests, but now check whether in a repeated procedure the actual
    # angles are never in the forbidden intervals
    sim <- matrix(0, nrow = 1000, ncol = 2)
    bounds <- matrix(c(pi / 4, pi / 2, 
                       3 * pi / 4, pi, 
                       3 * pi / 2, 2 * pi), 
                     ncol = 2, 
                     byrow = TRUE)

    forbidden(r) <- bounds
    for(i in 1:1000) {
        sim[i,] <- rng_point(r)
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





################################################################################
# ADD_NODES





################################################################################
# NODES_ON_CIRCUMFERENCE





################################################################################
# INTERSECTS

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
# LINE_INTERSECTION

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
# R vs RCPP

testthat::test_that("Nodes on circumference returns same result for R and Rcpp", {
    # Polygon without tilted segments and one with tilted segments
    poly <- list(predped::polygon(points = rbind(c(-1, -1),
                                                 c(-1, 1),
                                                 c(1, 1),
                                                 c(1, -1))),
                 predped::polygon(points = rbind(c(-1, 0),
                                                 c(0, 0.25),
                                                 c(1, -3),
                                                 c(0, -1),
                                                 c(0.5, -0.9))))

    ref <- lapply(poly,
                  \(x) predped::nodes_on_circumference(x, cpp = FALSE))
    tst <- lapply(poly, 
                  \(x) predped::nodes_on_circumference(x, cpp = TRUE))

    testthat::expect_equal(tst, ref)

    # Rectangle with and without orientation
    rect <- list(predped::rectangle(center = c(1, 1),
                                    size = c(2, 2)),
                 predped::rectangle(center = c(1, 1),
                                    size = c(2, 2),
                                    orientation = pi / 4))

    ref <- lapply(rect,
                  \(x) predped::nodes_on_circumference(x, cpp = FALSE))
    tst <- lapply(rect, 
                  \(x) predped::nodes_on_circumference(x, cpp = TRUE))

    testthat::expect_equal(tst, ref)

    # Circle
    circ <- predped::circle(center = c(1, 1),
                            radius = 1)

    ref <- predped::nodes_on_circumference(circ, cpp = FALSE)
    tst <- predped::nodes_on_circumference(circ, cpp = TRUE)

    testthat::expect_equal(tst, ref)
})
