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

    # Do the test
    larger_objects <- lapply(objects, 
                             \(x) predped:::enlarge_object(x, space_between = 1))

    testthat::expect_equal(radius(larger_objects[[1]]), 2, tolerance = 1e-2)
    testthat::expect_equal(size(larger_objects[[2]]), rep(1 + 2 * 0.707, 2), tolerance = 1e-2)
    testthat::expect_equal(diff(range(larger_objects[[3]]@points[,1])), 1 + 2 * 0.707, tolerance = 1e-2)
    testthat::expect_equal(diff(range(larger_objects[[3]]@points[,2])), 1 + 2 * 0.707, tolerance = 1e-2)
})

testthat::test_that("Creating nodes works (few)", {
    # Create a background
    objects <- list(predped::circle(center = c(0, 0), 
                                    radius = 1), 
                    predped::rectangle(center = c(2, 2), 
                                       size = c(1, 1)), 
                    predped::polygon(points = rbind(c(-2.5, -1.5), 
                                                    c(-1.5, -1.5),
                                                    c(-1.5, -2.5), 
                                                    c(-2.5, -2.5))))

    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(6, 6)), 
                                   objects = objects,
                                   entrance = c(0, -3))

    # Actually do the test
    tst <- predped::create_nodes(c(-1.5, 1.5), 
                                 c(1.5, -1.5),
                                 setting, 
                                 many_options = FALSE)

    # Create the reference (from a previous run that looked right)
    ref <- readRDS(file.path(".", "data", "ref_nodes_few.Rds"))

    testthat::expect_equal(tst, ref)

    # If you want to visualize it
    # plot(setting) +
    #     ggplot2::geom_point(data = tst, ggplot2::aes(x = X, y = Y), color = "black") +
    #     ggplot2::geom_point(data = ref, ggplot2::aes(x = X, y = Y), color = "red")
    
})

testthat::test_that("Creating nodes works (many)", {
    # Create a background
    objects <- list(predped::circle(center = c(0, 0), 
                                    radius = 1), 
                    predped::rectangle(center = c(2, 2), 
                                       size = c(1, 1)), 
                    predped::polygon(points = rbind(c(-2.5, -1.5), 
                                                    c(-1.5, -1.5),
                                                    c(-1.5, -2.5), 
                                                    c(-2.5, -2.5))))

    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(6, 6)), 
                                   objects = objects,
                                   entrance = c(0, -3))

    # Actually do the test
    tst <- predped::create_nodes(c(-1.5, 1.5), 
                                 c(1.5, -1.5),
                                 setting, 
                                 many_options = TRUE)

    # Create the reference (from a previous run that looked right)
    ref <- readRDS(file.path(".", "data", "ref_nodes_many.Rds"))

    testthat::expect_equal(tst, ref)

    # If you want to visualize it
    # plot(setting) +
    #     ggplot2::geom_point(data = tst, ggplot2::aes(x = X, y = Y), color = "black") +
    #     ggplot2::geom_point(data = ref, ggplot2::aes(x = X, y = Y), color = "red")
})

testthat::test_that("Evaluating which edges should be deleted works", {
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
    ids <- cbind(paste("node", 1:nrow(segments)), 
                 paste("agent", 1:nrow(segments)))

    # Create a reference
    cost <- (segments[,1] - segments[,3])^2 + (segments[,2] - segments[,4])^2
    ref <- list(edges = data.frame(from = paste("node", 1:8), 
                                   to = paste("agent", 1:8), 
                                   cost = as.character(cost[1:8])), 
                edges_with_coords = data.frame(from = paste("node", 1:8), 
                                               to = paste("agent", 1:8), 
                                               from_x = as.character(segments[1:8, 1]),
                                               from_y = as.character(segments[1:8, 2]),
                                               to_x = as.character(segments[1:8, 3]),
                                               to_y = as.character(segments[1:8, 4]),
                                               cost = as.character(cost[1:8])))

    # Do the test
    tst <- predped:::evaluate_edges(ids, segments, objects)

    testthat::expect_equal(tst$edges, ref$edges)
    testthat::expect_equal(tst$edges_with_coords, ref$edges_with_coords)
})

testthat::test_that("Adjusting edges works", {
    # Create precomputed_edges manually, knowing exactly what they are
    ids <- paste("node", 1:4)
    coords <- rbind(c(2, 0), 
                    c(0, -2), 
                    c(-2, 0), 
                    c(0, 2))

    segments <- cbind(ids, ids[c(2:4, 1)], coords, coords[c(2:4, 1),]) |>
        as.data.frame()
    segments[,3] <- as.numeric(segments[,3])
    segments[,4] <- as.numeric(segments[,4])
    segments[,5] <- as.numeric(segments[,5])
    segments[,6] <- as.numeric(segments[,6])

    cost <- (segments[,3] - segments[,5])^2 + (segments[,4] - segments[,6])^2

    edges <- list(edges = data.frame(from = segments[,1], 
                                     to = segments[,2], 
                                     cost = cost), 
                  edges_with_coords = setNames(cbind(segments, cost), 
                                               c("from", "to", "from_x", "from_y", "to_x", "to_y", "cost")),
                  nodes = data.frame(node_ID = ids, 
                                     X = coords[,1],
                                     Y = coords[,2]))

    # Create a new object that should delete the all potential segments that 
    # agent and goal can interact with except for one clear path.
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), size = c(1, 1)),
                                   objects = list(predped::rectangle(center = c(0, 2), size = c(3, 3)), 
                                                  predped::circle(center = c(1.5, 0), radius = 1.75)))
    agent <- c(-2, 2)
    goal <- c(2, -2)

    # Create the references
    ref_1 <- list(edges = rbind(edges$edges, 
                                data.frame(from = c("node 3", "node 2"), 
                                           to = c("agent", "goal"), 
                                           cost = c(4, 4))),
                  edges_with_coords = rbind(edges$edges_with_coords, 
                                            data.frame(from = c("node 3", "node 2"), 
                                                       to = c("agent", "goal"), 
                                                       from_x = c(-2, 0), 
                                                       from_y = c(0, -2), 
                                                       to_x = c(-2, 2), 
                                                       to_y = c(2, -2), 
                                                       cost = c(4, 4))),
                  nodes = rbind(edges$nodes, 
                                data.frame(node_ID = c("agent", "goal"), 
                                           X = c(-2, 2), 
                                           Y = c(2, -2))))

    ref_2 <- list(edges = data.frame(from = c("node 3", "node 2", "node 2"), 
                                     to = c("agent", "goal", "node 3"), 
                                     cost = as.character(c(4, 4, 8))),
                  edges_with_coords = data.frame(from = c("node 3", "node 2", "node 2"), 
                                                 to = c("agent", "goal", "node 3"), 
                                                 from_x = c(-2, 0, 0), 
                                                 from_y = c(0, -2, -2),
                                                 to_x = c(-2, 2, -2),
                                                 to_y = c(2, -2, 0),
                                                 cost = c(4, 4, 8)),
                  nodes = rbind(edges$nodes, 
                                data.frame(node_ID = c("agent", "goal"), 
                                           X = c(-2, 2), 
                                           Y = c(2, -2))))

    # Do the tests: With reevaluation and without
    tst_1 <- predped::adjust_edges(agent, goal, setting, precomputed_edges = edges, reevaluate = FALSE)
    tst_2 <- predped::adjust_edges(agent, goal, setting, precomputed_edges = edges, reevaluate = TRUE)

    # For the evaluation, we are going to put them in matrix format with no 
    # dimension names. Makes things easier for us
    ref_1 <- lapply(ref_1, \(x) { x <- as.matrix(x) ; dimnames(x) <- NULL ; x }) 
    ref_2 <- lapply(ref_2, \(x) { x <- as.matrix(x) ; dimnames(x) <- NULL ; x }) 
    tst_1 <- lapply(tst_1, \(x) { x <- as.matrix(x) ; dimnames(x) <- NULL ; x }) 
    tst_2 <- lapply(tst_2, \(x) { x <- as.matrix(x) ; dimnames(x) <- NULL ; x }) 

    testthat::expect_equal(tst_1$nodes, ref_1$nodes)
    testthat::expect_equal(tst_1$edges, ref_1$edges)
    testthat::expect_equal(tst_1$edges_with_coords, ref_1$edges_with_coords)

    testthat::expect_equal(tst_2$nodes, ref_2$nodes)
    testthat::expect_equal(tst_2$edges, ref_2$edges)
    testthat::expect_equal(tst_2$edges_with_coords, ref_2$edges_with_coords)
})
