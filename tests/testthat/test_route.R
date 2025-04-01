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
                                 many_nodes = FALSE)

    # Create the reference (from a previous run that looked right)
    ref <- readRDS(file.path(".", "data", "ref_nodes_few.Rds"))

    testthat::expect_equal(tst, ref)

    # If you want to visualize it
    # plot(setting) +
    #     ggplot2::annotate("point", x = tst$X, y = tst$Y, color = "black") +
    #     ggplot2::annotate("point", x = ref$X, y = ref$Y, color = "red")
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
                                 many_nodes = TRUE)

    # Create the reference (from a previous run that looked right)
    ref <- readRDS(file.path(".", "data", "ref_nodes_many.Rds"))

    testthat::expect_equal(tst, ref)

    # If you want to visualize it
    # plot(setting) +
    #     ggplot2::annotate("point", x = tst$X, y = tst$Y, color = "black") +
    #     ggplot2::annotate("point", x = ref$X, y = ref$Y, color = "red")
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
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(10, 10)), 
                                   objects = objects)

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
    segments <- cbind.data.frame(ids[,1], segments[,1:2], ids[,2], segments[,3:4]) |>
        setNames(c("from", "from_x", "from_y", "to", "to_x", "to_y"))

    # Create a reference
    cost <- (segments$from_x - segments$to_x)^2 + (segments$from_y - segments$to_y)^2
    ref <- list(edges = data.frame(from = paste("node", 1:8), 
                                   to = paste("agent", 1:8), 
                                   cost = cost[1:8]), 
                edges_with_coords = data.frame(from = paste("node", 1:8), 
                                               from_x = segments$from_x[1:8],
                                               from_y = segments$from_y[1:8],
                                               to = paste("agent", 1:8), 
                                               to_x = segments$to_x[1:8],
                                               to_y = segments$to_y[1:8],
                                               cost = cost[1:8]))

    # Do the test
    tst <- predped:::evaluate_edges(segments, setting, space_between = 0)

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

    segments <- cbind.data.frame(ids, coords, ids[c(2:4, 1)], coords[c(2:4, 1),]) |>
        setNames(c("from", "from_x", "from_y", "to", "to_x", "to_y"))

    cost <- (segments$from_x - segments$to_x)^2 + (segments$from_y - segments$to_y)^2

    edges <- list(edges = data.frame(from = segments$from, 
                                     to = segments$to, 
                                     cost = cost), 
                  edges_with_coords = cbind(segments, cost = cost),
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
                                data.frame(from = c("node 2", "node 3", "goal", "agent"), 
                                           to = c("goal", "agent", "node 2", "node 3"), 
                                           cost = c(4, 4, 4, 4))),
                  edges_with_coords = rbind(edges$edges_with_coords, 
                                            data.frame(from = c("node 2", "node 3", "goal", "agent"), 
                                                       from_x = c(0, -2, 2, -2), 
                                                       from_y = c(-2, 0, -2, 2), 
                                                       to = c("goal", "agent", "node 2", "node 3"), 
                                                       to_x = c(2, -2, 0, -2), 
                                                       to_y = c(-2, 2, -2, 0), 
                                                       cost = c(4, 4, 4, 4))),
                  nodes = rbind(edges$nodes, 
                                data.frame(node_ID = c("agent", "goal"), 
                                           X = c(-2, 2), 
                                           Y = c(2, -2))))

    ref_2 <- list(edges = data.frame(from = c("node 2", "node 3", "goal", "agent", "node 2"), 
                                     to = c("goal", "agent", "node 2", "node 3", "node 3"), 
                                     cost = as.character(c(4, 4, 4, 4, 8))),
                  edges_with_coords = data.frame(from = c("node 2", "node 3", "goal", "agent", "node 2"), 
                                                 from_x = c(0, -2, 2, -2, 0), 
                                                 from_y = c(-2, 0, -2, 2, -2),
                                                 to = c("goal", "agent", "node 2", "node 3", "node 3"), 
                                                 to_x = c(2, -2, 0, -2, -2),
                                                 to_y = c(-2, 2, -2, 0, 0),
                                                 cost = c(4, 4, 4, 4, 8)),
                  nodes = rbind(edges$nodes, 
                                data.frame(node_ID = c("agent", "goal"), 
                                           X = c(-2, 2), 
                                           Y = c(2, -2))))

    # Do the tests: With reevaluation and without
    tst_1 <- predped::adjust_edges(agent, goal, setting, precomputed_edges = edges, reevaluate = FALSE)
    tst_2 <- predped::adjust_edges(agent, goal, setting, precomputed_edges = edges, reevaluate = TRUE)

    # Before evaluating, put all matrices in alphabetical order
    tst_1 <- list(edges = dplyr::arrange(tst_1$edges, from, to),
                  edges_with_coords = dplyr::arrange(tst_1$edges_with_coords, from, to),
                  nodes = dplyr::arrange(tst_1$nodes, node_ID))

    ref_1 <- list(edges = dplyr::arrange(ref_1$edges, from, to),
                  edges_with_coords = dplyr::arrange(ref_1$edges_with_coords, from, to),
                  nodes = dplyr::arrange(ref_1$nodes, node_ID))

    tst_2 <- list(edges = dplyr::arrange(tst_2$edges, from, to),
                  edges_with_coords = dplyr::arrange(tst_2$edges_with_coords, from, to),
                  nodes = dplyr::arrange(tst_2$nodes, node_ID))

    ref_2 <- list(edges = dplyr::arrange(ref_2$edges, from, to),
                  edges_with_coords = dplyr::arrange(ref_2$edges_with_coords, from, to),
                  nodes = dplyr::arrange(ref_2$nodes, node_ID))

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

testthat::test_that("Adjusting edges gives same output as creating edges", {
    # Create precomputed_edges for a simple environment
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), size = c(2, 2)), 
                                   objects = list(predped::circle(center = c(0, 0), radius = 0.5),
                                                  predped::rectangle(center = c(-0.5, -0.5), size = c(0.5, 0.5)),
                                                  predped::polygon(points = rbind(c(0.75, 0.75), c(0.75, 0.25), c(0.25, 0.25), c(0.25, 0.75)))),
                                   entrance = c(0, -1))
    spc <- 0.1

    precomputed_edges <- predped::create_edges(c(-0.75, 0.75), 
                                               c(0.75, -0.75), 
                                               setting, 
                                               space_between = spc, 
                                               many_nodes = FALSE)
    precomputed_edges$edges <- precomputed_edges$edges[!(precomputed_edges$edges$from %in% c("agent", "goal")),]
    precomputed_edges$edges <- precomputed_edges$edges[!(precomputed_edges$edges$to %in% c("agent", "goal")),]
    precomputed_edges$nodes <- precomputed_edges$nodes[!(precomputed_edges$nodes$node_ID %in% c("agent", "goal")),]
    precomputed_edges$edges_with_coords <- precomputed_edges$edges_with_coords[!(precomputed_edges$edges_with_coords$from %in% c("agent", "goal")),]
    precomputed_edges$edges_with_coords <- precomputed_edges$edges_with_coords[!(precomputed_edges$edges_with_coords$to %in% c("agent", "goal")),]

    # The `create_edges` function needs a new environment, so provide it to them
    additional_object <- list(predped::circle(center = c(0, 0.75), radius = 0.5))

    new_setting <- setting 
    objects(new_setting) <- append(objects(new_setting), additional_object)

    # Create new edges with the create_edges and adjust_edges functions
    ref <- predped::create_edges(c(-0.75, 0.75), 
                                 c(0.75, -0.75), 
                                 new_setting, 
                                 space_between = spc, 
                                 many_nodes = FALSE)

    tst <- predped::adjust_edges(c(-0.75, 0.75),
                                 c(0.75, -0.75),
                                 setting, 
                                 new_objects = additional_object,
                                 space_between = spc,
                                 precomputed_edges = precomputed_edges,
                                 reevaluate = TRUE)

    # Check whether both contain the same information, all of which is contained
    # within edges_with_coords. Use the visual inspection below when interpreting
    # the results, but use the costs defined in edges for the actual test
    testthat::expect_equal(sort(as.numeric(tst$edges$cost)), 
                           sort(as.numeric(ref$edges$cost)))

    # If you want to visualize everything
    # plot(new_setting) +
    #     ggplot2::geom_segment(ggplot2::aes(x = ref$edges_with_coords$from_x, 
    #                                        y = ref$edges_with_coords$from_y,
    #                                        xend = ref$edges_with_coords$to_x, 
    #                                        yend = ref$edges_with_coords$to_y),
    #                           linewidth = 2) +
    #     ggplot2::geom_segment(ggplot2::aes(x = tst$edges_with_coords$from_x, 
    #                                        y = tst$edges_with_coords$from_y,
    #                                        xend = tst$edges_with_coords$to_x, 
    #                                        yend = tst$edges_with_coords$to_y),
    #                           color = "red") 
})

testthat::test_that("Creating edges with one-directional flow works", {
    # Create an environment in which some access is limited
    setting <- predped::background(shape = predped::rectangle(center = c(0, 0), 
                                                              size = c(2, 2)), 
                                   objects = list(predped::rectangle(center = c(0, 0), 
                                                                     size = c(1, 1))),
                                   limited_access = list(predped::segment(from = c(-1, 0.5), 
                                                                          to = c(-0.5, 0.5)),
                                                         predped::segment(from = c(0.5, 1), 
                                                                          to = c(0.5, 0.5)),
                                                         predped::segment(from = c(1, -0.5), 
                                                                          to = c(0.5, -0.5)),
                                                         predped::segment(from = c(-0.5, -1), 
                                                                          to = c(-0.5, -0.5))),
                                   entrance = c(-1, 0))
    spc <- 0.1

    # Load in the reference (originally done on a piece of paper and then 
    # adjusted to fit the algorithm)
    ref <- readRDS(file.path(".", "data", "unidirectional_edges.Rds"))

    # Compute the edges
    tst <- predped::create_edges(c(-0.75, 0), 
                                 c(0.75, 0),
                                 setting,
                                 space_between = spc,
                                 many_nodes = FALSE)

    # Put in alphabetical order
    ref<- list(edges = dplyr::arrange(ref$edges, from, to),
               edges_with_coords = dplyr::arrange(ref$edges_with_coords, from, to),
               nodes = dplyr::arrange(ref$nodes, node_ID))
    tst <- list(edges = dplyr::arrange(tst$edges, from, to),
                edges_with_coords = dplyr::arrange(tst$edges_with_coords, from, to),
                nodes = dplyr::arrange(tst$nodes, node_ID))

    # For the evaluation, we are going to put them in matrix format with no 
    # dimension names. Makes things easier for us. For the tst, also round the 
    # values
    ref <- lapply(ref, \(x) { x <- as.matrix(x) ; dimnames(x) <- NULL ; x }) 
    tst <- lapply(tst, \(x) { x <- as.matrix(x) ; dimnames(x) <- NULL ; x })

    testthat::expect_equal(tst$nodes, ref$nodes, tolerance = 1e-3)
    testthat::expect_equal(tst$edges, ref$edges, tolerance = 1e-3)
    testthat::expect_equal(tst$edges_with_coords, ref$edges_with_coords, tolerance = 1e-3)
})
