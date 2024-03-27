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
                         space_between = 0.5) {

    obj <- objects(background)

    # Make each of the objects in the background a bit bigger. This way, agents
    # cannot take shortcuts
    new_obj <- lapply(obj, 
                      \(x) enlarge_object(x, space_between = space_between))

    # Create the nodes that will serve as potential path points
    nodes <- create_nodes(from, 
                          to, 
                          background, 
                          space_between = space_between)

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
#   - Extend `in_object` to deal with matrices in a vectorized way and get rid 
#     of the loop over nodes
#   - Vectorize the creation of the diagonal nodes in rectangles and circles and
#     find a way to vectorize this for polygons (i.e., get rid of the while loop)
create_nodes <- function(from, 
                         to, 
                         background, 
                         space_between = 0.5) {
                            
    # Create a matrix of coordinates close to the edge of the shape of the object.
    # These will serve as the first nodes of the network.
    #
    # For this, we will several columns/rows of nodes that close in on the 
    # center of the figure. The algorithm used here is not ideal, but is better
    # than nothing (especially for irregular polygons, this algorithm may fail) 
    shp <- shape(background)

    # nodes <- add_nodes(shp, 
    #                    space_between = space_between, 
    #                    outside = FALSE)

    # Add nodes along the edges of each of the objects
    obj <- objects(background)
    obj_nodes <- lapply(obj, 
                        \(x) add_nodes(x, 
                                       space_between = space_between,
                                       only_corners = TRUE))

    # nodes <- rbind(nodes,
    #                do.call("rbind", obj_nodes))
    nodes <- do.call("rbind", obj_nodes)

    # Check which nodes are contained within the environment and only retain 
    # those. Furthermore delete all nodes that fall within an object. For this,
    # we first create slightly bigger objects so that nodes close to each object
    # are deleted. This ensures that the agents will leave some space between 
    # them and the object.
    new_obj <- lapply(obj, 
                      \(x) enlarge_object(x, space_between = space_between))

    for(i in seq_len(nrow(nodes))) {
        if(in_object(shp, nodes[i,], outside = TRUE)) {
            nodes[i,] <- NA
        } else {
            for(j in seq_along(new_obj)) {
                if(in_object(new_obj[[j]], nodes[i,], outside = FALSE)) {
                    nodes[i,] <- NA
                    break
                }
            } 
        }
    }

    nodes <- nodes[!is.na(nodes[,1]),]

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














# Takes ps, a matrix of xy cordinate columns, with named rows for entry (G0), 
# exit ("Gend"), mustVisit goals (G1, G2, ...), path points (P1, P2, ...), 
# objects (used to figure out which points can see each other with seesGoal), 
# and returns cppRouting edges makegraph object with distances (optionally 
# plots with iGraph)
# makeGraph = FALSE returns raw (one way) point1, point2, 
# distance connection matrix
# oneWay specifies regions where flow is one way
getEdges <- function(ps, objects, plotIt = FALSE, main = "", makeGraph = TRUE,
                     oneWay = NULL, pointcol = "#92C5DE", 
                     linkcol = "#969696") {
  
    # Matrix to store which points can be seen from each other and their distance
    out <- matrix(nrow = 0, ncol = 3, 
                    dimnames = list(NULL, c("from", "to", "dist")))
    nams <- row.names(ps)
    
    # For each combination of points
    for (i in 1:(dim(ps)[1] - 1)) {
        for (j in (i + 1):dim(ps)[1]) {
        # Check if there is a line of sight between points
        if (seesGoal(as.numeric(ps[i, ]), as.numeric(ps[j, ]), objects)) {
            # Compute distance between points
            d <- sqrt(sum((ps[i, ] - ps[j, ])^2))
            
            # Check if point i is left of point j
            left1 <- ps[i, 1] < ps[j, 1]
            
            # Create two entries LR and RL with distance
            if (left1) {
            LR <- c(nams[i], nams[j], d)
            RL <- c(nams[j], nams[i], d)
            } else {
            RL <- c(nams[i], nams[j], d)
            LR <- c(nams[j], nams[i], d)
            }
            
            # Check if there are one-way directions
            if (!is.null(oneWay)) {
            inAisle <- passThrough(oneWay, p = as.numeric(ps[i, ]), 
                                    P = as.numeric(ps[j, ]))
            if (!any(inAisle)) {
                badLR <- badRL <- FALSE 
            } else {
                badLR <- any(names(inAisle[inAisle]) == "<") 
                badRL <- any(names(inAisle[inAisle]) == ">") 
            }
            } else { 
            badLR <- badRL <- FALSE
            }
            if (badLR) {
            LR <- NULL 
            }
            if (badRL) {
            RL <- NULL 
            }
            out <- rbind(out, LR, RL)
        }
        }
    }
    
    # Convert out to data frame and make distances numeric
    out <- data.frame(out, stringsAsFactors = FALSE)
    out$dist <- as.numeric(out$dist)
    
    # Make and plot graph
    if (makeGraph) {
        out <- makegraph(out, directed = TRUE,
        coords = cbind.data.frame(node_ID = row.names(ps),X = ps$x, Y = ps$y))
        if (plotIt) {
        igr1 <- graph_from_data_frame(out$data)
        plot.igraph(igr1, edge.arrow.size = .3, main = main, 
                    vertex.color = pointcol,
                    vertex.frame.color = pointcol,
                    vertex.label.family = "Helvetica",
                    vertex.label.cex = 1.2,
                    edge.color = linkcol,
                    edge.width = 1)
        }
    }
    
    return(out)
}

# Adds edges for new point P0 to existing edges object, except for any goals in 
# bad
addEdge <- function(P0, edges, objects, oneWay = NULL) {
    out <- matrix(nrow = 0, ncol = 3, 
                    dimnames = list(NULL, c("from", "to", "dist")))
    ok <- substr(edges$dict$ref, 1, 1) != "G"
    ps <- edges$coords[ok, -1]
    nams <- edges$coords[ok, 1]
    for (j in 1:dim(ps)[1]) {
        if (seesGoal(P0, as.numeric(ps[j, ]), objects)) {
        d <- sqrt(sum((P0 - ps[j, ])^2))
        left1 <- P0[1] < ps[j, 1]
        if (left1) {
            LR <- c("P0", nams[j], d)
            RL <- c(nams[j], "P0", d)
        } else {
            RL <- c("P0", nams[j], d)
            LR <- c(nams[j], "P0", d)
        }
        if(!is.null(oneWay)) {
            inAisle <- passThrough(oneWay, p = P0, P = as.numeric(ps[j, ]))
            if (!any(inAisle)) {
            badLR <- badRL <- FALSE 
            } else {
            badLR <- any(names(inAisle[inAisle]) == "<") 
            badRL <- any(names(inAisle[inAisle]) == ">") 
            }
        } else {
            badLR <- badRL <- FALSE
        }
        if (badLR) {
            LR <- NULL 
        }
        if (badRL) {
            RL <- NULL 
        }
        out <- rbind(out, LR, RL)
        }
    }
    out <- data.frame(out, stringsAsFactors = FALSE)
    row.names(out) <- NULL
    out$dist <- as.numeric(out$dist)
    edges$dict <- rbind(edges$dict, c(0, edges$nbnode)) 
    edges$nbnode <- edges$nbnode + 1
    edges$dict[edges$nbnode, 1] <- "P0"
    edges$coords <- rbind(edges$coords, c(0, P0))
    edges$coords[edges$nbnode, 1] <- "P0"
    dict <- as.numeric(edges$dict[, 2])
    names(dict) <- edges$dict[, 1]
    out$to <- dict[out$to]
    out$from <- dict[out$from]
    edges$data <- rbind(edges$data, out)
    
    edges
}

# Remove edges that intersect blocker profiles
removeEdges <- function(edges, nBlockers) {
    
    blocked <- function(P12, ends, coords) {
        
        isIntersect <- function(L2, P1, P2) {
        any(is.finite(line.line.intersection(P1, P2, P3 = L2[, 1], P4 = L2[, 2], 
                                            interior.only = TRUE)))
        }
        
        any(apply(ends, 1, isIntersect,
                P1 = as.numeric(coords[coords[, 1] == as.character(P12[1]),2:3]),
                P2 = as.numeric(coords[coords[, 1] == as.character(P12[2]),2:3])))
    }
    
    if (nBlockers == 0) {
        return(edges) 
    } else {
        ends <- attr(nBlockers, "ends")
        dict <- edges$dict
        row.names(dict) <- dict[, 2]
        data <- edges$data
        data$from <- dict[as.character(data$from), "ref"]
        data$to <- dict[as.character(data$to), "ref"]
        edges$data <- edges$data[!apply(data, 1, blocked, ends = ends, 
                                        coords = edges$coords), ]
        return(edges)
    }
}