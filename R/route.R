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
    new_obj <- list()
    for(i in seq_along(obj)) {
        if(inherits(obj[[i]], "circle")) {
            new_obj[[i]] <- circle(center = center(obj[[i]]), 
                                   radius = radius(obj[[i]]) + space_between)
        } else if(inherits(obj[[i]], "polygon")) {
            points <- add_nodes(obj[[i]], 
                                space_between = space_between,
                                only_corners = TRUE)
            new_obj[[i]] <- polygon(points = points)
        } else if(inherits(obj[[i]], "rectangle")) {
            new_obj[[i]] <- rectangle(center = center(obj[[i]]), 
                                      size = obj[[i]]@size + space_between)
        } else {
            stop(paste0("The object provided is not recognized: ", class(obj[[i]])))
        }
    }

    # Create the nodes that will serve as potential path points
    nodes <- create_nodes(from, to, background, space_between = space_between)

    # Now that we have the nodes, we can also create edges or pathways between 
    # them. Here, it is important to consider which edges are actually 
    # connectable, or specifically which one's are occluded by the objects in 
    # the background.
    edges <- list(from = list(), to = list(), cost = list())
    idx <- 1
    for(i in seq_len(nrow(nodes) - 1)) {
        for(j in (i + 1):nrow(nodes)) {
            co_1 <- as.numeric(nodes[i, c("X", "Y")])
            co_2 <- as.numeric(nodes[j, c("X", "Y")])
            ids <- c(nodes$node_ID[i], nodes$node_ID[j])

            # Check if the nodes can be connected to each other: If occluded, 
            # that means something is standing in the way
            if(!m4ma::seesGoal(co_1, co_2, new_obj) & !any(ids %in% c("agent", "goal"))) {
                next
            }

            if(!m4ma::seesGoal(co_1, co_2, obj)) {
                next
            }

            # If they passed all the checks, then we can connect them to each
            # other and compute the distance between both points. We have to save
            # the rownames of the nodes in the dataframe
            edges$from[[idx]] <- nodes$node_ID[i]
            edges$to[[idx]] <- nodes$node_ID[j]
            edges$cost[[idx]] <- sqrt((co_1[1] - co_2[1])^2 + (co_1[2] - co_2[2])^2)

            idx <- idx + 1
        }
    }

    idx <- sapply(edges$from, 
                  \(x) which(nodes$node_ID == x))
    idy <- sapply(edges$to, 
                  \(x) which(nodes$node_ID == x))

    tmp_edges <- cbind(nodes$X[idx], 
                       nodes$Y[idx],
                       nodes$X[idy], 
                       nodes$Y[idy]) |>
        as.data.frame() |>
        setNames(c("x", "y", "xend", "yend"))

    # Transform this list to a dataframe, as required by cppRouting
    from <- rbind(edges$from) |> t()
    to <- rbind(edges$to) |> t()
    cost <- rbind(edges$cost) |> t()

    edges <- cbind.data.frame(from, 
                              to, 
                              as.numeric(cost)) |>
        setNames(c("from", "to", "cost"))

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
create_nodes <- function(from, 
                         to, 
                         background, 
                         space_between = 0.5) {
    
    # Create a matrix of coordinates close to the edge of the shape of the object.
    # These will serve as the first nodes of the network.
    shp <- shape(background)
    nodes <- add_nodes(shp, 
                       space_between = space_between,
                       outside = FALSE)

    # Add nodes along the edges of each of the objects
    obj <- objects(background)
    obj_nodes <- lapply(obj, 
                        \(x) add_nodes(x, space_between = space_between))

    nodes <- rbind(nodes,
                   do.call("rbind", obj_nodes))

    # Check which nodes are contained within the environment and only retain 
    # those. Furthermore delete all nodes that fall within an object. For this,
    # we first create slightly bigger objects so that nodes close to each object
    # are deleted. This ensures that the agents will leave some space between 
    # them and the object.
    new_obj <- list()
    for(i in seq_along(obj)) {
        if(inherits(obj[[i]], "circle")) {
            new_obj[[i]] <- circle(center = center(obj[[i]]), 
                                   radius = radius(obj[[i]]) + space_between)
        } else if(inherits(obj[[i]], "polygon")) {
            points <- add_nodes(obj[[i]], 
                                space_between = space_between,
                                only_corners = TRUE)
            new_obj[[i]] <- polygon(points = points)
        } else if(inherits(obj[[i]], "rectangle")) {
            new_obj[[i]] <- rectangle(center = center(obj[[i]]), 
                                      size = obj[[i]]@size + space_between)
        } else {
            stop(paste0("The object provided is not recognized: ", class(obj[[i]])))
        }
    }

    for(i in seq_len(nrow(nodes))) {
        if(in_object(shp, nodes[i,])) {
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