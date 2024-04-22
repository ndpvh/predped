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
                         many_options = FALSE) {

    obj <- objects(background)

    # Make each of the objects in the background a bit bigger. This way, agents
    # cannot take shortcuts
    new_obj <- lapply(obj, 
                      \(x) enlarge_object(x, space_between = space_between))

    # Create the nodes that will serve as potential path points
    nodes <- create_nodes(from, 
                          to, 
                          background, 
                          space_between = space_between,
                          many_options = many_options)

    # Now that we have the nodes, we can also create edges or pathways between 
    # them. Here, it is important to consider which edges are actually 
    # connectable, or specifically which one's are occluded by the objects in 
    # the background.
    #
    # Approach taken is to minimize the time it takes to do these computations:
    #   - Step 1: Find out which nodes are uniquely connectable to each other.
    #             First make all connections and then only retain the unique ones
    #             through identification in a triangular logical matrix
    #   - Step 2: Compute the distance between the nodes.
    #   - Step 3: Find out which nodes are reachable from one another. In other 
    #             words, if I stand at node 1, can I see node 2? In this step, 
    #             we also immediately compute the distance from one node to 
    #             another.

    # Step 1
    n_nodes <- nrow(nodes)
    edges <- cbind(nodes[rep(seq_len(n_nodes), each = n_nodes),],
                   nodes[rep(seq_len(n_nodes), times = n_nodes),])
    to_remain <- matrix(0, nrow = n_nodes, ncol = n_nodes) |>
        lower.tri() |>
        as.vector()
    edges <- edges[to_remain,] |>
        as.data.frame() |>
        setNames(c("from", "from_x", "from_y", 
                   "to", "to_x", "to_y"))

    # Step 2: Note, we use squared distances as the cost for efficiency purposes
    # (taking the square root is computationally more expensive). Should not matter 
    # to the results we get from the routing algorithm
    edges$cost <- (edges$from_x - edges$to_x)^2 + (edges$from_y - edges$to_y)^2

    # Step 3: Check which nodes can be seen at each location
    idx <- prune_edges(obj,
                       as.matrix(edges[,c("from_x", "from_y", "to_x", "to_y")]))

    # Only retain what you need
    edges <- edges[idx, c("from", "to", "cost")]

    # For robustness, delete all edges that have NA values associated to them
    idx <- !is.na(edges$from) & !is.na(edges$to) & !is.na(edges$cost)
    edges <- edges[idx,]

    return(list(edges = edges, nodes = nodes))
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
#
# TO DO:
#   - Vectorize the creation of the diagonal nodes in rectangles and circles and
#     find a way to vectorize this for polygons (i.e., get rid of the while loop)
create_nodes <- function(from, 
                         to, 
                         background, 
                         space_between = 0.5,
                         many_options = FALSE) {
                            
    # Create a matrix of coordinates that fill up the complete space. This will 
    # allow agents to take whatever route to their destination 
    shp <- shape(background)

    if(many_options) {
        if(inherits(shp, "circle")) {
            xlim <- center(shp)[,1] + c(-1, 1) * radius(shp)
            ylim <- center(shp)[,2] + c(-1, 1) * radius(shp)
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

    if(many_options) {
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
    new_obj <- lapply(obj, 
                      \(x) enlarge_object(x, space_between = space_between))

    to_delete <- in_object(shp, nodes, outside = TRUE)
    for(i in seq_along(new_obj)) {
        to_delete <- to_delete | in_object(new_obj[[i]], nodes, outside = FALSE)
    }

    nodes <- nodes[!to_delete,]

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
    nodes <- cbind.data.frame(ids, 
                              as.numeric(nodes[,1]), 
                              as.numeric(nodes[,2])) |>
        setNames(c("node_ID", "X", "Y"))

    # For robustness, delete all nodes that have NA values associated to them
    idx <- !is.na(nodes$node_ID) & !is.na(nodes$X) & !is.na(nodes$Y)
    nodes <- nodes[idx,]

    return(nodes)
}

# Utility function that will enlarge an object based on the spacing provided 
# by the argument
enlarge_object <- function(object, 
                           space_between = 0.5) {
    # Dispatch on the kind of object we are talking about
    if(inherits(object, "circle")) {
        # Extend the radius with space_between - 1e-4. Ensures that we don't
        # delete route points that are `space_between` far from the circle
        return(circle(center = center(object), 
                      radius = radius(object) + space_between - 1e-4))

    } else if(inherits(object, "rectangle")) {
        # Extend the size of the rectangle with the factor 
        # sqrt{space_between^2 / 2}, as we do in the `add_nodes` function. 
        # Again correct with factor 1e-4
        extension <- sqrt(space_between^2 / 2)
        return(rectangle(center = center(object), 
                         size = object@size + 2 * (extension - 1e-4)))

    } else if(inherits(object, "polygon")) {
        # Simply find the nodes of the polygon and use these new nodes as 
        # the points of the outer polygon.
        points <- add_nodes(object, 
                            space_between = space_between - 1e-4,
                            only_corners = TRUE)
        return(polygon(points = points))

    } else {
        stop(paste0("The object provided is not recognized: ", class(object)))
    }
}

# Make a vectorized alternative to m4ma::seesGoal that will loop over the objects
# but looks at intersections in a vectorized manner
prune_edges <- function(objects, segments) {
    # Loop over the objects in the environment
    all_intersections <- matrix(FALSE, nrow = nrow(segments), ncol = length(objects))
    for(i in seq_along(objects)) {
        # Dispatch based on the kind of object we are talking about
        if(inherits(objects[[i]], "polygon")) {
            # If there is an intersection with the points of the polygon/rectangle,
            # then the agent cannot see the location we are talking about and 
            # the function should return false
            points <- objects[[i]]@points
            edges <- cbind(points, points[c(2:nrow(points), 1),])
            intersections <- line_line_intersection(segments, edges, return_all = TRUE)
            
            # Rework the intersections matrix so that it has the values for each
            # of the segments of the polygon/rectangle in its columns. Then find
            # out whether the segments intersect with any of the segments of the 
            # polygon/rectangle by using rowSums, which should be equal to 0 if
            # there are no intersections
            intersections <- matrix(intersections, 
                                    ncol = nrow(edges),
                                    byrow = T)
            all_intersections[,i] <- rowSums(intersections) > 0L

        } else if(inherits(objects[[i]], "circle")) {
            # Here we just use the line_intersections function that has been 
            # designed for circles. 
            all_intersections[,i] <- line_intersection(objects[[i]], 
                                                       segments, 
                                                       return_all = TRUE)
        }
    }

    # I want to only retain those that do not intersect, meaning that the complete
    # row should be FALSE
    return(rowSums(all_intersections) == 0L)
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
                         precomputed_edges) {

    obj <- objects(background)
    nodes <- precomputed_edges$nodes
    edges <- precomputed_edges$edges

    from_to <- rbind(c("agent", as.numeric(from)), 
                     c("goal", as.numeric(to))) |>
        as.data.frame() |>
        setNames(colnames(nodes))

    from_to$X <- as.numeric(from_to$X)
    from_to$Y <- as.numeric(from_to$Y)

    # Add the from and to coordinates to the nodes
    new_nodes <- rbind(nodes, from_to)

    # Create new pathways that go from the agent and goal to all of the other 
    # edges and bind them together with the already existing ones.
    #
    # Approach taken is to minimize the time it takes to do these computations:
    #   - Step 1: Create the unique edges between agent, goal, and already 
    #             existing nodes
    #   - Step 2: Compute the distance between the nodes.
    #   - Step 3: Find out which nodes are reachable from one another. In other 
    #             words, if I stand at node 1, can I see node 2? In this step, 
    #             we also immediately compute the distance from one node to 
    #             another.

    # Step 1
    n_nodes <- nrow(nodes)
    new_edges <- cbind(from_to[rep(seq_len(2), each = n_nodes),],
                       nodes[rep(seq_len(n_nodes), times = 2),]) |>
        as.data.frame() |>
        setNames(c("from", "from_x", "from_y", 
                   "to", "to_x", "to_y"))

    # Step 2: Note, we use squared distances as the cost for efficiency purposes
    # (taking the square root is computationally more expensive). Should not matter 
    # to the results we get from the routing algorithm
    new_edges$cost <- (new_edges$from_x - new_edges$to_x)^2 + (new_edges$from_y - new_edges$to_y)^2

    # Step 3: Check which nodes can be seen at each location
    idx <- prune_edges(obj,
                       as.matrix(new_edges[,c("from_x", "from_y", "to_x", "to_y")]))

    # Only retain what you need and bind it together with the precomputed variant
    new_edges <- new_edges[idx, c("from", "to", "cost")]
    new_edges <- rbind(edges, new_edges)
    return(list(edges = new_edges, nodes = new_nodes))
}
