#' Create edges of graph to walk on
#' 
#' This function uses the background, goal position, and position of the agent
#' to create all possible routes along which the agent can walk towards their 
#' goal. The output is then used to find the shortest path.
#' 
#' @param from Coordinate from which to start
#' @param to Coordinate at which to end
#' @param background The background/setting in which the agent is walking
#' @param space_between Numeric denoting the space left between each of the 
#' different nodes. Defaults to 0.5.
#' 
#' @return Matrix containing from, to, and the distance between these coordinates
#' 
#' @export 
create_edges <- function(from, 
                         to, 
                         background, 
                         space_between = 0.5,
                         many_nodes = FALSE) {

    # Create the nodes that will serve as potential path points
    nodes <- create_nodes(from, 
                          to, 
                          background, 
                          space_between = space_between,
                          many_nodes = many_nodes)

    # If there are no nodes, then we will have to return NULL
    if(is.null(nodes)) {
        return(NULL)
    }

    # Make the connection between all possible edges. Importantly, keep the id's
    # identifying the nodes and the segments that are created between each node 
    # separate. They will be bound together in `evaluate_edges`.
    segments <- combine_nodes(nodes)

    # Check whether these edges don't pass through the objects in the background 
    # and return the required list of edges and nodes
    return(append(evaluate_edges(segments, background), 
                  list(nodes = nodes)))
}

#' Adjust edges of graph to walk on
#' 
#' This function uses the background, goal position, and position of the agent
#' to adjust a previously computed graph of possible routes along which the agent 
#' can walk towards their goal. The output is then used to find the shortest path.
#' 
#' @param from Coordinate from which to start
#' @param to Coordinate at which to end
#' @param background The background/setting in which the agent is walking
#' @param precomputed_edges Output of `create_edges` in which agent and goal 
#' are removed
#' 
#' @return Matrix containing from, to, and the distance between these coordinates
#' 
#' @export 
adjust_edges <- function(from, 
                         to, 
                         background,
                         precomputed_edges,
                         new_objects = NULL,                          
                         space_between = 0.5,
                         reevaluate = FALSE) {

    nodes <- precomputed_edges$nodes
    edges_with_coords <- precomputed_edges$edges_with_coords

    # Make a placeholder for obj_nodes 
    obj_nodes <- matrix(nrow = 0, ncol = 2)

    # Add the new objects to the background
    if(is.null(new_objects)) {
        new_objects <- list()
    }
    obj <- append(objects(background), new_objects)

    # Create new nodes based on the objects that are new to the setting. If none
    # are present, then we can just bind the agent and goal nodes to the frame.
    # Steps taken here are very similar to those taken in `create_nodes`
    #
    # Importantly, if you want to reevaluate the original nodes, this only 
    # happens when new objects have been added to the environment: As long as 
    # nothing new is introduced in the environment, you should not reevaluate 
    # the nodes
    if(!is.null(new_objects) & length(new_objects) != 0) {
        # Create nodes for each of the new objects and bind them together
        obj_nodes <- lapply(new_objects, 
                            \(x) add_nodes(x, 
                                           space_between = space_between, 
                                           only_corners = TRUE))
        obj_nodes <- do.call("rbind", obj_nodes)

        # Delete these nodes if they are already the same as those in `nodes`
        # Commented out for performance, and assumed not to give any trouble if 
        # some edges are duplicated.
        #
        # to_delete <- sapply(seq_len(nrow(obj_nodes)), 
        #                     \(i) any((obj_nodes[i,1] == nodes$X) & (obj_nodes[i,2] == nodes$Y)))
        # obj_nodes <- obj_nodes[!to_delete,]

        # If you want to reevaluate the previous set of nodes, then immediately 
        # do so in bulk (will make our lives easier)
        if(reevaluate) {
            # Keep an index that will tell us when the original nodes begin
            node_idx <- nrow(obj_nodes) + 1
            obj_nodes <- rbind(obj_nodes, as.matrix(nodes[,2:3]))
        }

        # Delete nodes based on whether they are occluded by an object and on 
        # whether they are contained within the environment
        extension <- space_between - 1e-4
        to_delete <- lapply(obj, 
                            \(x) in_object(enlarge(x, extension),
                                           obj_nodes, 
                                           outside = FALSE))
        to_delete <- Reduce("|", to_delete) | in_object(shape(background), obj_nodes, outside = TRUE)

        # Before we delete this, we need to first handle the original nodes in 
        # case of reevaluation, otherwise we lose which nodes to delete
        if(reevaluate) {
            n <- length(to_delete)

            # Delete the nodes that should be deleted
            nodes <- nodes[!to_delete[node_idx:n],]
            to_delete[node_idx:n] <- TRUE

            # Also delete these nodes from `edges_with_coords`
            names_nodes <- nodes$node_ID

            idx <- (edges_with_coords$from %in% names_nodes) & (edges_with_coords$to %in% names_nodes)
            edges_with_coords <- edges_with_coords[idx,]
        }

        # Once done, we can also delete the unnecessary nodes from obj_nodes. 
        # Here, we only keep the new nodes, not the old, reevaluated ones
        obj_nodes <- obj_nodes[!to_delete,] |>
            matrix(ncol = 2)
    }

    if(is.null(obj_nodes) | any(is.na(obj_nodes))) {
        obj_nodes <- matrix(nrow = 0, ncol = 2)
    }

    # Now that we created new nodes, we should bind them together with the 
    # positions of the agent and goals
    from_to <- data.frame(node_ID = c("agent", 
                                      "goal", 
                                      paste0("adjusted_nodes_", 1:nrow(obj_nodes))[!logical(nrow(obj_nodes))]),
                          X = c(from[1], to[1], obj_nodes[,1]),
                          Y = c(from[2], to[2], obj_nodes[,2]))

    # Create new pathways that go from the agent and goal to all of the other 
    # edges. First, we do this within the `obj_nodes` that we just created. 
    # Afterwards, we do this for all the nodes in `from_to` to all of the old 
    # nodes.
    if(nrow(nodes) == 0) {
        segments <- combine_nodes(from_to)
    } else {
        segments <- rbind(combine_nodes(nodes, from_to), 
                          combine_nodes(from_to))
    }

    # Bind these segments and ids together with those that are already present 
    # in the precomputed edges. This step is necessary if we want to make sure 
    # that all edges are reevaluated on their adequacy
    if(reevaluate) {
        segments <- rbind(segments, subset(edges_with_coords, select = -cost))
    }

    # Check whether these edges don't pass through the objects in the background
    objects(background) <- obj
    edges <- evaluate_edges(segments, background)

    # If there hadn't been a reevaluation before, we need to bind these edges
    # to the already computed ones
    if(!reevaluate) {
        edges$edges <- rbind(edges$edges, precomputed_edges$edges)
        edges$edges_with_coords <- rbind(edges$edges_with_coords, edges_with_coords)
    }

    return(append(edges, 
                  list(nodes = rbind(nodes, from_to))))
}





#' Create nodes of graph to walk on
#' 
#' This function uses the background, goal position, and position of the agent
#' to create all potential nodes along which the agent can plan their route 
#' towards their goal. The output is then used to find the shortest path.
#' 
#' @param from Coordinate from which to start
#' @param to Coordinate at which to end
#' @param background The background/setting in which the agent is walking
#' @param space_between Numeric denoting the space left between each of the 
#' different nodes. Defaults to 0.5.
#' 
#' @return Matrix containing from, to, and the distance between these coordinates
#' 
#' @export 
create_nodes <- function(from, 
                         to, 
                         background, 
                         space_between = 0.5,
                         many_nodes = FALSE) {
                            
    # Create a matrix of coordinates that fill up the complete space. This will 
    # allow agents to take whatever route to their destination 
    shp <- shape(background)

    if(many_nodes) {
        if(inherits(shp, "circle")) {
            xlim <- center(shp)[1] + c(-1, 1) * radius(shp)
            ylim <- center(shp)[2] + c(-1, 1) * radius(shp)
        } else {
            xlim <- range(shp@points[,1])
            ylim <- range(shp@points[,2])
        }
        X <- seq(xlim[1] + space_between, 
                 xlim[2] - space_between, 
                 (xlim[2] - xlim[1] - 2 * space_between) / 20)
        Y <- seq(ylim[1] + space_between, 
                 ylim[2] - space_between,
                 (ylim[2] - ylim[1] - 2 * space_between) / 20)
        nodes <- cbind(rep(X, each = length(Y)),
                       rep(Y, times = length(X)))
    }

    # Add nodes along the edges of each of the objects
    obj <- objects(background)
    obj_nodes <- lapply(obj, 
                        \(x) add_nodes(x, 
                                       space_between = space_between,
                                       only_corners = TRUE))

    if(many_nodes) {
        nodes <- rbind(nodes,
                       do.call("rbind", obj_nodes))
    } else {
        nodes <- do.call("rbind", obj_nodes)
    }

    # Check which nodes are contained within the environment and only retain 
    # those. Furthermore delete all nodes that fall within an object. For this,
    # we first create slightly bigger objects so that nodes close to each object
    # are deleted. This ensures that the agents will leave some space between 
    # them and the object.
    extension <- space_between - 1e-4
    to_delete <- lapply(obj, 
                        \(x) in_object(enlarge(x, extension), 
                                       nodes, 
                                       outside = FALSE))
    to_delete <- Reduce("|", to_delete) | in_object(shp, nodes, outside = TRUE)

    nodes <- nodes[!to_delete,] |> 
        matrix(ncol = 2)

    # Do a check of whether there are any nodes at all. If not, then we will have
    # to return NULL and get on with it.
    if(length(nodes) == 0) {
        return(NULL)
    }

    # Create node id's, as expected by `makegraph`
    ids <- paste0("node ", 1:nrow(nodes))

    # Add the person and the goal to the list of nodes and give them unique 
    # identifiers. These identifiers are again called in the `find_path` function
    # of the goals.
    #
    # Important, the agent should be added as a first node, and the goal as a 
    # last node. This will allow us to use `directed = TRUE` in `makegraph`, as 
    # all edges start from the agent and end in the goal
    nodes <- rbind(from, nodes, to)
    rownames(nodes) <- NULL
    ids <- c("agent", ids, "goal")

    # Now, transform the nodes to a dataframe as required by `makegraph` from
    # the cppRouting package
    nodes <- cbind.data.frame(node_ID = ids, 
                              X = as.numeric(nodes[,1]), 
                              Y = as.numeric(nodes[,2]))

    # For robustness, delete all nodes that have NA values associated to them
    idx <- !is.na(nodes$node_ID) & !is.na(nodes$X) & !is.na(nodes$Y)
    nodes <- nodes[idx,]

    return(nodes)
}

#' Evaluate whether edges pass through objects
#' 
#' This function evaluates whether all connections between the nodes are actually 
#' specified, or specifically which one's are occluded by the objects in 
#' the background.
#'
#' Approach taken is to minimize the time it takes to do these computations:
#'   - Step 1: Compute the distance between the nodes.
#'   - Step 2: Find out which nodes are reachable from one another. In other 
#'             words, if I stand at node 1, can I see node 2?
#' 
#' @param segments Named matrix or data.frame containing the ids of the nodes 
#' under column names "from" and "to", and their coordinates under "from_x", 
#' "from_y", "to_x", and "to_y"
#' @param objects List of objects that are contained in the setting
#' 
#' @return List containing the nodes, edges, and edges together with their 
#' coordinates
#' 
#' @export
evaluate_edges <- function(segments, 
                           background) {

    obj <- objects(background)
    lim <- limited_access(background)

    # Step 1: Note, we use squared distances as the cost for efficiency purposes
    # (taking the square root is computationally more expensive). Should not matter 
    # to the results we get from the routing algorithm
    cost <- (segments$from_x - segments$to_x)^2 + (segments$from_y - segments$to_y)^2

    # Step 2: Check which nodes can be seen at each location
    if(length(obj) == 0) {
        idx <- rep(TRUE, nrow(segments))
    } else {
        idx <- prune_edges(obj, segments[, c("from_x", "from_y", "to_x", "to_y")])
    }

    # Step 3: If there is limited access, we need to account for this. Unfortunately, 
    # this can only be assessed for each node separately, which thus necessitates
    # a loop.
    if(length(lim) != 0) {
        # Do an intermediate update of the segments based on the current results
        segments <- segments[idx,]
        cost <- cost[idx]

        # Get the indices indicating which coordinates should account for which 
        # of the segments
        idy <- limit_access(background, 
                            segments[, c("from_x", "from_y")])

        # Loop over the limited access and look at the interactions between 
        # these and the edges 
        intersections <- sapply(background@precomputed_limited_access, 
                                \(x) line_intersection(x, segments[, c("from_x", "from_y", "to_x", "to_y")], return_all = TRUE))

        # First check whether the intersections matter, which amounts to having 
        # a TRUE in both idy and in intersections. Then, we can check whether 
        # there is an intersection with any of the objects by reducing over the 
        # second dimension in this matrix and checking whether any of the edges 
        # crosses one of the segments (or-statement)
        #
        # Importantly, we need to reverse this statement, as the AND statement 
        # says which edges we should delete
        idx <- !as.logical(rowSums(idy & intersections) != 0)
    }

    # Bind all information together and delete all edges that have NA values 
    # associated to them (`prune_edges` returns NA whenever a segment is actually
    # a point, but only if the object is a circle)
    edges_with_coords <- cbind(segments[idx,], 
                               cost = cost[idx]) 
    edges <- edges_with_coords[, c("from", "to", "cost")]

    idx <- !is.na(edges[,1]) & !is.na(edges[,2]) & !is.na(edges[,3])

    return(list(edges = edges[idx,], 
                edges_with_coords = edges_with_coords[idx,]))
}





# Make a vectorized alternative to m4ma::seesGoal that will loop over the objects
# but looks at intersections in a vectorized manner.
#
# NOTE: Tried a completely vectorized alternative, but this was not helpful. 
# This form seems to be the fastest this function can work.
prune_edges <- function(objects, segments) {
    # If there are no objects, then there can be no intersections
    if(length(objects) == 0) {
        return(rep(TRUE, nrow(segments)))
    }

    # Loop over the objects in the environment and check their intersections 
    # with the lines in `segments`
    all_intersections <- lapply(objects, 
                                \(x) line_intersection(x, segments, return_all = TRUE))

    # I want to only retain those that do not intersect, meaning that the complete
    # row should be FALSE. We therefore check whether any of the sides is TRUE and 
    # then reverse the operation, so that none of them can be
    return(!Reduce("|", all_intersections))
}

# Make a function that takes in two vectors of nodes and will combine them 
# into one. Importantly, nodes should be in the required format, meaning a 
# data.frame with 3 columns, namely node_ID, X, and Y
combine_nodes <- function(nodes_1, 
                          nodes_2 = NULL) {

    # If the second set of nodes is not NULL, we need to combine the nodes in 
    # one data.frame with the nodes of the other. Otherwise, we combine the nodes
    # within the one data.frame with each other.
    if(!is.null(nodes_2)) {
        # Get the size of each of the data.frames
        n <- nrow(nodes_1)
        k <- nrow(nodes_2)

        # Create indices to be repeated. These indices define which member of node_1
        # is connected to which member of node_2
        idx_1 <- rep(1:n, each = k)
        idx_2 <- rep(1:k, times = n)

        # Create a new nodes matrix that contains all nodes in order. We then use
        # the absolute indices that refer to those nodes of the first matrix and 
        # those of the second matrix to bind them together in one cbind. Is 
        # faster than simply doing two rbinds and one cbind, and therefore used 
        # here (despite decreased understandability)
        nodes <- rbind(nodes_1[idx_1,], nodes_2[idx_2,])
        idx <- c(length(idx_1) + seq_along(idx_2), seq_along(idx_1))

        result <- cbind(nodes, nodes[idx,]) |>
            setNames(c("from", "from_x", "from_y", "to", "to_x", "to_y"))

    } else {
        n <- nrow(nodes_1)
        idx <- matrix(1:n, nrow = n, ncol = n)

        idx_1 <- as.numeric(t(idx))
        idx_2 <- as.numeric(idx)

        result <- cbind(nodes_1[idx_1,], nodes_1[idx_2,]) |>
            setNames(c("from", "from_x", "from_y", "to", "to_x", "to_y"))
    }

    # Finally, delete all nodes that are connected to themselves
    return(result[result$from != result$to, ])


    # ORIGINAL IMPLEMENTATION: Only unique combinations, as the algorithm 
    # was bi-directional. Changed with the change to uni-directional 
    # algorithm, but kept in here for legacy purposes. 
        
    # # If the second set of nodes is not NULL, we want to combine each node in 
    # # the one data.frame with all the nodes in the other. Otherwise, we want to 
    # # combine each node within the same data.frame with each other
    # if(!is.null(nodes_2)) {
    #     # Get the sizes of each of the node matrices
    #     n <- nrow(nodes_1)
    #     k <- nrow(nodes_2)

    #     # Create indices to be repeated. These indices define which member of node_1
    #     # is connected to which member of node_2
    #     idx_1 <- rep(1:n, each = k)
    #     idx_2 <- rep(1:k, times = n)

    #     return(cbind(nodes_1[idx_1,], nodes_2[idx_2,]) |>
    #         setNames(c("from", "from_x", "from_y", "to", "to_x", "to_y")))

    # } else {
    #     n <- nrow(nodes_1)
    #     idx <- matrix(1:n, nrow = n, ncol = n)
    #     to_remain <- lower.tri(idx)
    
    #     idx_1 <- t(idx)[to_remain]
    #     idx_2 <- idx[to_remain]

    #     return(cbind(nodes_1[idx_1,], nodes_1[idx_2,]) |>
    #         setNames(c("from", "from_x", "from_y", "to", "to_x", "to_y")))
    # }
}

#' Compute the edges within a given setting
#' 
#' This function uses `create_edges` and then deletes agent and goal positions 
#' from the resulting graph. The output can then be used as precomputed edges, 
#' increasing the speed of searching for a route. 
#' 
#' @param background Background for which to compute the edges
#' @param space_between Space to keep between nodes and objects in the 
#' environment. Defaults to 2.5 times the maximal possible radius that the agents
#' can be.
#' @param many_nodes Logical denoting whether to create more than the minimal
#' number of nodes, allowing more flexibility in the agents. Defaults to `TRUE`.
#' 
#' @return List containing edges, edges_with_coords, and nodes
#' 
#' @export 
compute_edges <- function(background, 
                          space_between = 2.5 * max(params_from_csv[["params_bounds"]]["radius",]),
                          many_nodes = TRUE) {
    # Create the edges themselves with mock-positions of agent and goal
    edges <- create_edges(c(0, 0), 
                          c(0, 0), 
                          background,
                          space_between = space_between,
                          many_nodes = many_nodes)

    # Delete agent and goal positions from these edges, as these should be 
    # dynamic. 
    edges$edges <- edges$edges[!(edges$edges$from %in% c("agent", "goal")),]
    edges$edges <- edges$edges[!(edges$edges$to %in% c("agent", "goal")),]
    edges$nodes <- edges$nodes[!(edges$nodes$node_ID %in% c("agent", "goal")),]
    edges$edges_with_coords <- edges$edges_with_coords[!(edges$edges_with_coords$from %in% c("agent", "goal")),]
    edges$edges_with_coords <- edges$edges_with_coords[!(edges$edges_with_coords$to %in% c("agent", "goal")),]

    return(edges)
}