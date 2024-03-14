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

    # Create the nodes that will serve as potential path points
    nodes <- create_nodes(from, to, background, space_between = space_between)

    # Now that we have the nodes, we can also create edges or pathways between 
    # them. Here, it is important to consider which edges are actually 
    # connectable, or specifically which one's are occluded by the objects in 
    # the background.
    edges <- list(from = list(), to = list(), cost = list())
    idx <- 1
    for(i in seq_len(nrow(nodes))) {
        for(j in (i + 1):nrow(nodes)) {
            # Check if the nodes can be connected to each other: If occluded, 
            # that means something is standing in the way
            if(!m4ma::seesGoal(nodes[i,], nodes[j,], obj)) {
                next
            }

            # If they passed all the checks, then we can connect them to each
            # other and compute the distance between both points
            edges$from[[idx]] <- nodes[i,]
            edges$to[[idx]] <- nodes[j,]
            edges$cost[[idx]] <- sqrt((nodes[i,1] - nodes[j,1])^2 + (nodes[i,2] - nodes[j,2])^2)

            idx <- idx + 1
        }
    }

    # Transform this list to a dataframe, as required by cppRouting
    from <- rbind(edges$from) |> t()
    to <- rbind(edges$to) |> t()
    cost <- rbind(edges$cost) |> t()

    edges <- cbind(from, to, cost) |>
        as.data.frame() |>
        setNames(c("from", "to", "cost"))

    return(edges)
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
    # Define a rectangle within which we will generate the different path points.
    #
    # Depending on the shape of the background, we need a slightly different 
    # approach
    shp <- shape(background)
    if(class(shp) == "circle") {
        x_range <- c(center(shp)[1] - radius(shp),
                     center(shp)[1] + radius(shp))
        y_range <- c(center(shp)[2] - radius(shp),
                     center(shp)[2] + radius(shp))
    } else {
        x_range <- range(shp@points[,1])
        y_range <- range(shp@points[,2])
    }

    # Create a matrix of all coordinates of equidistant locations in the 
    # background. These will serve as the nodes of our network
    x_co <- seq(x_range[1] + space_between / 2, 
                x_range[2] - space_between / 2,
                space_between)
    y_co <- seq(y_range[1] + space_between / 2,
                y_range[2] - space_between / 2,
                space_between)
    nodes <- cbind(rep(x_co, each = length(y_co)),
                   rep(y_co, times = length(x_co)))

    # Add nodes along the edges of each of the objects
    obj <- objects(background)
    obj_nodes <- lapply(obj, 
                        \(x) add_nodes(x, space_between = space_between))

    nodes <- rbind(nodes,
                   do.call("rbind", obj_nodes))

    # Check which nodes are contained within the environment and only retain 
    # those. Furthermore delete all nodes that fall within an object.
    for(i in seq_len(nrow(nodes))) {
        if(in_object(shp, nodes[i,])) {
            nodes[i,] <- NA
        } else {
            for(j in seq_along(obj)) {
                if(in_object(obj[[j]], nodes[i,], outside = FALSE)) {
                    nodes[i,] <- NA
                    break
                }
            } 
        }
    }

    nodes <- nodes[!is.na(nodes[,1]),]

    # Add the person and the goal to the list of nodes
    nodes <- rbind(nodes, from, to)

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