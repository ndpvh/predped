testthat::test_that("Pruning edges works", {
    # Create a list of objects
    objects <- list(predped::circle(center = c(0, 0), 
                                    radius = 1), 
                    predped::rectangle(center = c(2, 2), 
                                       size = c(1, 1)), 
                    predped::polygon(points = rbind(c(-2.5, -1.5), 
                                                    c(-1.5, -1.5),
                                                    c(-1.5, -2.5), 
                                                    c(-2.5, -2.5))))

    # Create the segments to evaluate
    segments <- rbind(# No intersections
                      c(2, 0, 0, 2),
                      c(-2, 0, 0, -2),
                      c(0, -2, 2, 0),
                      c(-2, 0, 0, 2),
                      # Leading up to object, but no intersection
                      c(0, -2, -1.4, -2),
                      c(0, 2, 1.4, 2),
                      c(0, 2, 0, 1.1),
                      c(0, -2, 0, -1.1),
                      # Intersection with one object
                      c(0, -2, 0, 2),   # Circle, partially
                      c(0, 0, 0, -2),   # Circle, fully
                      c(-2, 0, -2, -2), # Polygon, partially 
                      c(-2, 0, -2, -4), # Polygon, fully
                      c(2, 0, 2, 2),    # Rectangle, partially 
                      c(2, 0, 2, 4),    # Rectangle, fully
                      # Intersection with two objects
                      c(0, 0, 2, 2),    # Circle and rectangle, partially
                      c(-1, -1, 3, 3),  # Circle and rectangle, fully
                      c(0, 0, -2, -2),  # Circle and polygon, partially
                      c(1, 1, -3, -3),  # Circle and polygon, fully
                      # Intersection with all objects
                      c(-2, -2, 2, 2),  # Partially
                      c(-3, -3, 3, 3))  # Fully

    # Do the test
    tst <- predped:::prune_edges(objects, segments)

    testthat::expect_equal(tst, c(rep(TRUE, 8), rep(FALSE, 12)))
})

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

    larger_objects <- lapply(objects, 
                             \(x) predped:::enlarge_object(x, space_between = 1))

    testthat::expect_equal(radius(larger_objects[[1]]), 2, tolerance = 1e-2)
    testthat::expect_equal(size(larger_objects[[2]]), rep(1 + 2 * 0.707, 2), tolerance = 1e-2)
    testthat::expect_equal(diff(range(larger_objects[[3]]@points[,1])), 1 + 2 * 0.707, tolerance = 1e-2)
    testthat::expect_equal(diff(range(larger_objects[[3]]@points[,2])), 1 + 2 * 0.707, tolerance = 1e-2)
})
