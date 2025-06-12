#' Create edges of graph to walk on
#' 
#' This function creates a list of nodes and edges that define the paths that 
#' an agent might walk on to reach their goal. The output is then used in 
#' \code{\link[predped]{find_path}} to find the shortest path from the 
#' agent to the goal.
#' 
#' @details 
#' This function depends on many other functions to do its work, and works in 
#' the following way. First, it will create many potential nodes and evaluate 
#' whether these nodes can be accessed, that is, that they do not fall outside 
#' of the \code{shape} specified in \code{background} or inside of objects that 
#' can be found in the \code{objects} slot of the \code{background}. This part 
#' of the computation is handled by the \code{\link[predped]{create_nodes}}
#' function.
#' 
#' Then, each of the nodes is combined to each other to form edges, that is 
#' paths from one node to another that the agent might use to walk to their 
#' goals. This is handled by the \code{\link[predped]{combine_nodes}} function.
#' 
#' The appropriateness of these connections is then checked by the 
#' \code{\link[predped]{evaluate_edges}} function. Specifically, this function 
#' checks whether the connections that are made do not intersect with any of the 
#' \code{objects} in the \code{background}, that is whether none of these 
#' objects obstructs the path. Note that this computation also accounts for 
#' potential limits in the bidirectionality of the ways, as defined by the 
#' \code{limited_access} slot in \code{background} (see 
#' \code{\link[predped]{limit_access}}).
#' 
#' Once this is done, the result is put in a list as required by the 
#' \code{\link[predped]{find_path}} to perform its computations.
#' 
#' Note that all edges are unidirectional. This is enforced to allow for one-
#' directional flow as controlled through the \code{limited_access} slot in 
#' \code{background}.
#' 
#' @param from Numeric denoting the coordinate from which to begin routes.
#' @param to Numeric denoting the coordinate from which to end routes.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param space_between Numeric denoting the space to leave between the 
#' circumference of the object and the nodes created under the hood (see
#' \code{\link[predped]{add_nodes}}). Defaults to \code{0.5}.
#' @param many_nodes Logical denoting whether to create many nodes or leave 
#' it at the minimum. If \code{FALSE}, nodes are only added at the outlines of 
#' the objects contained within the \code{objects} slot of \code{background}. 
#' If \code{TRUE}, 400 additional nodes are added at an equal distance in the 
#' x-direction (20 nodes) and an equal distance in the y-direction (20 nodes), 
#' making the 20 x 20 = 400 additional nodes. Defaults to \code{FALSE}.
#' 
#' @return List containing a dataframe with the surviving nodes under 
#' \code{"nodes"}, a dataframe with the surviving connections between nodes 
#' under \code{"edges"}, and a similar dataframe to the previous one but with 
#' the coordinates still in there under \code{"edges_with_coords"}.
#' 
#' @examples 
#' # Define a background in which the agent can move
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(rectangle(center = c(0, 0), 
#'                                                      size = c(1, 1))))
#' 
#' edges <- create_edges(c(-0.75, 0), 
#'                       c(0.75, 0), 
#'                       my_background, 
#'                       space_between = 0.25)
#' 
#' # Check the nodes and edges created
#' head(edges$nodes)
#' head(edges$edges)
#' head(edges$edges_with_coords)
#' 
#' @seealso 
#' \code{\link[predped]{background-class}},
#' \code{\link[predped]{add_nodes}},
#' \code{\link[predped]{find_path}},
#' \code{\link[predped]{limit_access}},
#' \code{\link[predped]{combine_nodes}},
#' \code{\link[predped]{create_nodes}},
#' \code{\link[predped]{evaluate_edges}}
#' 
#' @rdname create_edges
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
    return(append(evaluate_edges(segments, background, space_between - 1e-4), 
                  list(nodes = nodes)))
}

#' Adjust edges of graph to walk on
#' 
#' This function adjusts a previously created list of nodes and edges to include
#' new nodes and edges and/or reevaluate the appropriateness of the old ones.
#' The output is then used in the \code{\link[predped]{find_path}} to 
#' find the shortest path from the agent to the goal.
#' 
#' @details 
#' Goes through a similar type of steps as \code{\link[predped]{create_edges}},
#' with the exception that \code{adjust_edges} takes in already computed and 
#' evaluated edges and adjusts those for the current purposes of the 
#' environment, there where \code{\link[predped]{create_edges}} creates these 
#' same edges from scratch.
#' 
#' @param from Numeric denoting the coordinate from which to begin routes.
#' @param to Numeric denoting the coordinate from which to end routes.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param precomputed_edges Previously computed edges, usually the output of 
#' \code{\link[predped]{compute_edges}}.
#' @param space_between Numeric denoting the space to leave between the 
#' circumference of the object and the nodes created under the hood (see
#' \code{\link[predped]{add_nodes}}). Defaults to \code{0.5}.
#' @param new_objects List of instances of the \code{\link[predped]{object-class}}
#' that were previously not contained in the \code{objects} slot of the 
#' \code{background} when computing the edges provided in 
#' \code{precomputed_edges}. Typically, these are other pedestrians that an 
#' agent wants to account for when rerouting. Defaults to \code{NULL}, so that
#' only \code{from} and \code{to} are added to the already existing edges.
#' @param reevaluate Logical denoting whether to reevaluate the appropriateness
#' of the edges that are contained in the \code{precomputed_edges}. Defaults to 
#' \code{TRUE} when \code{new_objects} is not empty, and to \code{FALSE} 
#' whenever it is.
#' 
#' @return List containing a dataframe with the surviving nodes under 
#' \code{"nodes"}, a dataframe with the surviving connections between nodes 
#' under \code{"edges"}, and a similar dataframe to the previous one but with 
#' the coordinates still in there under \code{"edges_with_coords"}.
#' 
#' @examples 
#' # Define a background in which the agent can move
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(rectangle(center = c(0, 0), 
#'                                                      size = c(1, 1))))
#' 
#' # Create precomputed edges
#' edges <- compute_edges(my_background, 
#'                        space_between = 0.25,
#'                        many_nodes = FALSE)
#' head(edges$edges)
#' 
#' # Adjust these edges and provide values for the from and to arguments
#' adjusted_edges <- adjust_edges(c(-0.75, 0), 
#'                                c(0.75, 0), 
#'                                my_background, 
#'                                edges,
#'                                space_between = 0.25)
#' head(adjusted_edges$edges)
#' 
#' @seealso 
#' \code{\link[predped]{background-class}},
#' \code{\link[predped]{add_nodes}},
#' \code{\link[predped]{find_path}},
#' \code{\link[predped]{limit_access}},
#' \code{\link[predped]{combine_nodes}},
#' \code{\link[predped]{compute_edges}},
#' \code{\link[predped]{create_edges}},
#' \code{\link[predped]{create_nodes}},
#' \code{\link[predped]{evaluate_edges}}
#' 
#' @rdname adjust_edges
#' 
#' @export 
adjust_edges <- function(from, 
                         to, 
                         background,
                         precomputed_edges,                  
                         space_between = 0.5,
                         new_objects = NULL, 
                         reevaluate = !is.null(new_objects)) {

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
                            \(x) in_object(enlarge(x, extension), obj_nodes))
        to_delete <- Reduce("|", to_delete) | out_object(shape(background), obj_nodes)

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
    edges <- evaluate_edges(segments, background, space_between - 1e-4)

    # If there hadn't been a reevaluation before, we need to bind these edges
    # to the already computed ones
    if(!reevaluate) {
        edges$edges <- rbind(edges$edges, precomputed_edges$edges)
        edges$edges_with_coords <- rbind(edges$edges_with_coords, edges_with_coords)
    }

    return(append(edges, 
                  list(nodes = rbind(nodes, from_to))))
}





#' Create nodes that serve as path points
#' 
#' This function creates a dataframe of nodes that can serve as path points for 
#' the agent to use when walking to their goal. The output is used in 
#' \code{\link[predped]{create_edges}} to create paths the agent can take, and 
#' ultimately in \code{\link[predped]{find_path}} for finding the 
#' shortest path from the agent to the goal.
#' 
#' @param from Numeric denoting the coordinate from which to begin routes.
#' @param to Numeric denoting the coordinate from which to end routes.
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param space_between Numeric denoting the space to leave between the 
#' circumference of the object and the nodes created under the hood (see
#' \code{\link[predped]{add_nodes}}). Defaults to \code{0.5}.
#' @param many_nodes Logical denoting whether to create many nodes or leave 
#' it at the minimum. If \code{FALSE}, nodes are only added at the outlines of 
#' the objects contained within the \code{objects} slot of \code{background}. 
#' If \code{TRUE}, 400 additional nodes are added at an equal distance in the 
#' x-direction (20 nodes) and an equal distance in the y-direction (20 nodes), 
#' making the 20 x 20 = 400 additional nodes. Defaults to \code{FALSE}.
#' 
#' @return Dataframe with the surviving nodes, containing the node id under 
#' \code{"node_ID"} and its coordinates under \code{"X"} and \code{"Y"}.
#' 
#' @examples 
#' # Define a background in which the agent can move
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(rectangle(center = c(0, 0), 
#'                                                      size = c(1, 1))))
#' 
#' nodes <- create_nodes(c(-0.75, 0), 
#'                       c(0.75, 0), 
#'                       my_background, 
#'                       space_between = 0.25)
#' 
#' # Check the nodes
#' head(nodes)
#' 
#' @seealso 
#' \code{\link[predped]{background-class}},
#' \code{\link[predped]{add_nodes}},
#' \code{\link[predped]{find_path}},
#' \code{\link[predped]{in_object}},
#' \code{\link[predped]{create_edges}},
#' \code{\link[predped]{out_object}},
#' 
#' @rdname create_nodes
#' 
#' @export 
#
# TO DO 
#   - Make the creation of nodes smarter, so that if space_between is too wide, 
#     it will make nodes in the middle between two objects or between the objects
#     and the shape of the environment
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
    shp_nodes <- add_nodes(shp, 
                           space_between = space_between, 
                           only_corners = TRUE,
                           outside = FALSE)

    if(many_nodes) {
        nodes <- rbind(nodes,
                       do.call("rbind", obj_nodes),
                       shp_nodes)
    } else {
        nodes <- rbind(do.call("rbind", obj_nodes),
                       shp_nodes)
    }

    # Check which nodes are contained within the environment and only retain 
    # those. Furthermore delete all nodes that fall within an object. For this,
    # we first create slightly bigger objects so that nodes close to each object
    # are deleted. This ensures that the agents will leave some space between 
    # them and the object.
    extension <- space_between - 1e-4

    if(length(obj) == 0) {
        to_delete <- out_object(shp, nodes)
    } else {
        to_delete <- lapply(obj, 
                        \(x) in_object(enlarge(x, extension), nodes))
        to_delete <- Reduce("|", to_delete) | out_object(shp, nodes)
    }   

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
#' This function evaluates whether the connections that are made between nodes 
#' are reachable, that is that the paths are not blocked by any objects. This 
#' also accounts for the one-way blockages that are present in the 
#' \code{limited_access} slot of the \code{\link[predped]{background-class}}.
#' 
#' @details
#' In this function, a lot of the heavy lifting is done by the 
#' \code{\link[predped]{line_intersection}} method and the 
#' \code{\link[predped]{prune_edges}} function.
#' 
#' @param segments Named matrix or dataframe containing the ids of the nodes 
#' under column names \code{"from"} and \code{"to"}, and their coordinates under 
#' \code{"from_x"}, \code{"from_y"}, \code{"to_x"}, and \code{"to_y"}.
#' @param objects List of objects that extend the 
#' \code{\link[predped]{object-class}}.
#' 
#' @return List containing a dataframe with the surviving connections between 
#' the nodes under \code{"edges"} and a similar dataframe also containing the 
#' coordinates under \code{"edges_with_coords"}.
#' 
#' @examples 
#' # Let's create a background
#' my_background <- background(shape = rectangle(shape = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(rectangle(shape = c(0, 0), 
#'                                                      size = c(1, 1))))
#' 
#' # Create some segments that do and do not go though the objects in the 
#' # background
#' potential_edges <- data.frame(from = c("node 1", "node 2", "node 3"), 
#'                               from_x = c(-0.75, -0.75, -0.75), 
#'                               from_y = c(0, 0, 0),
#'                               to = c("node 4", "node 5", "node 6"),
#'                               to_x = c(-0.75, 0.75, 0.95), 
#'                               to_y = c(0.75, 0, 0.95))
#' head(potential_edges)
#' 
#' # Only retain the edges that don't intersect the object
#' surviving_edges <- evaluate_edges(potential_edges, my_background)
#' head(surviving_edges$edges_with_coords)
#' 
#' @seealso 
#' \code{\link[predped]{adjust_edges}},
#' \code{\link[predped]{create_edges}},
#' \code{\link[predped]{prune_edges}},
#' \code{\link[predped]{line_intersection}}
#' 
#' @rdname evaluate_edges
#' 
#' @export
evaluate_edges <- function(segments, 
                           background,
                           space_between) {

    obj <- lapply(objects(background),
                  \(x) enlarge(x, space_between))
    lim <- limited_access(background)

    # Step 1: Note, we use squared distances as the cost for efficiency purposes
    # (taking the square root is computationally more expensive). Should not matter 
    # to the results we get from the routing algorithm
    cost <- (segments$from_x - segments$to_x)^2 + (segments$from_y - segments$to_y)^2

    # Step 2: Check which nodes can be seen at each location
    if(length(obj) == 0) {
        idx <- rep(TRUE, nrow(segments))
    } else {
        idx <- prune_edges(
            obj, 
            segments[, c("from_x", "from_y", "to_x", "to_y")],
            coord_specific = objects(background)
        )
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





#' Evaluate intersections with objects
#' 
#' In a sense, this is a vectorized alternative to \code{\link[m4ma]{seesGoal}}, 
#' but is applied more broadly to evaluate whether there are intersections 
#' between the edges that were created and the objects in the environment. 
#' 
#' @details
#' In this function, a lot of the heavy lifting is done by the 
#' \code{\link[predped]{line_intersection}} method.
#' 
#' Note that this function is kept separate from 
#' \code{\link[predped]{evaluate_edges}} so that it can still be used for the 
#' same purposes as \code{\link[m4ma]{seesGoal}} was originally used for. 
#' 
#' @param objects List of objects that extend the 
#' \code{\link[predped]{object-class}}.
#' @param segments Numerical matrix of size N x 4 containing the coordinates of 
#' the line segments in order x_1, y_1, x_2, y_2.
#' 
#' @return Logical vector denoting whether a given edge can be retained 
#' (\code{TRUE}) or should be discarded (\code{FALSE})
#' 
#' @examples 
#' # Let's create a list of objects
#' objects <- list(rectangle(shape = c(0, 0), size = c(1, 1)))
#' 
#' # Create some segments that do and do not go though the objects in the 
#' # background
#' segments <- cbind(c(-0.75, -0.75, -0.75), 
#'                   c(0, 0, 0),
#'                   c(-0.75, 0.75, 0.95), 
#'                   c(0.75, 0, 0.95))
#' segments
#' 
#' # Do the test
#' prune_edges(objects, segments)
#' 
#' @seealso 
#' \code{\link[predped]{adjust_edges}},
#' \code{\link[predped]{create_edges}},
#' \code{\link[predped]{evaluate_edges}},
#' \code{\link[predped]{line_intersection}}
#' 
#' @rdname prune_edges
#' 
#' @export
#
# NOTE: Tried a completely vectorized alternative, but this was not helpful. 
# This form seems to be the fastest this function can work.
prune_edges <- function(objects, segments, coord_specific = NULL) {
    # If there are no objects, then there can be no intersections
    if(length(objects) == 0) {
        return(rep(TRUE, nrow(segments)))
    }

    # If there are no segments, there can be no intersections
    if(length(segments) == 0) {
        return(logical(0))
    }

    # Loop over the objects in the environment and check their intersections 
    # with the lines in `segments`
    #
    # Also check whether the coordinates themselves fall within the objects. If 
    # so, then we make an exception: None of the nodes that make up the edges 
    # should themselves be contained within the enlarged object except when these
    # represent goals and agents. For these cases, we make exceptions whenever 
    # there is an intersection.
    #
    # In practice comes down to also checking whether the coordinates lie outside
    # of the objects that you intersect them with (in `all_intersections`) and
    # later checking only for these exceptions with an alternative set of 
    # objects (in `coord_intersection`)
    if(is.null(coord_specific)) {
        all_intersections <- lapply(objects, 
                                    \(x) line_intersection(x, segments, return_all = TRUE))

        test_1 <- Reduce("|", all_intersections)
        test_2 <- logical(nrow(segments))
    } else {
        all_intersections <- lapply(objects, 
                                    \(x) line_intersection(x, segments, return_all = TRUE) &
                                         (out_object(x, segments[, 1:2]) & 
                                          out_object(x, segments[, 3:4])))
        test_1 <- Reduce("|", all_intersections)

        if(length(coord_specific) != length(objects)) {
            test_2 <- logical(nrow(segments))
        } else { 
            coord_intersection <- lapply(seq_along(coord_specific), 
                                         \(i) line_intersection(coord_specific[[i]], segments, return_all = TRUE) &
                                              (in_object(objects[[i]], segments[, 1:2]) | 
                                               in_object(objects[[i]], segments[, 3:4])))
            test_2 <- Reduce("|", coord_intersection)
        }
    }

    # I want to only retain those that do not intersect, meaning that the complete
    # row should be FALSE. We therefore check whether any of the sides is TRUE and 
    # then reverse the operation, so that none of them can be
    return(!(test_1 | test_2))
}

#' Make combinations of different nodes
#' 
#' This function connects all the provided nodes to each other, effectively 
#' creating all the potential paths that the agent can use to move to their 
#' goal.
#' 
#' @details 
#' How this function works depends on whether only one dataframe or two 
#' dataframes are provided to the function. If one dataframe is provided, all 
#' possible combinations between the nodes inside of this dataframe are created.
#' If two dataframes are provided, all possible combinations of the nodes across
#' dataframes are created.
#' 
#' @param nodes_1 Dataframe of nodes with columns \code{"node_ID"}, \code{"X"}, 
#' and \code{"Y"}.
#' @param nodes_2 Dataframe of the different nodes than \code{nodes_1}, but 
#' with the same structure. When \code{NULL}, triggers the function to 
#' make all possible combinations between the nodes inside of \code{nodes_1}. 
#' If not \code{NULL}, all possible combinations between the nodes in 
#' \code{nodes_1} with those of \code{nodes_2} will be made. Defaults to 
#' \code{NULL}.
#' 
#' @return Dataframe containing the combinations of the nodes, where all 
#' information is contained within columns \code{"from"}, \code{"from_x"}, 
#' \code{"from_y"}, \code{"to"}, \code{"to_x"}, and \code{"to_y"}.
#' 
#' @examples 
#' # Let's create a dataframe containing some nodes
#' nodes <- data.frame(node_ID = c("node 1", "node 2", "node 3"), 
#'                     X = 1:3, 
#'                     Y = 1:3)
#' 
#' # Combine all nodes in this dataframe to each other
#' edges <- combine_nodes(nodes)
#' head(edges)
#' 
#' # Create a second dataframe and combine all its nodes to the ones in the 
#' # first dataframe
#' nodes_2 <- data.frame(node_ID = c("node 4", "node 5", "node 6"), 
#'                       X = 4:6, 
#'                       Y = 4:6)
#' 
#' edges <- combine_nodes(nodes, nodes_2)
#' head(edges) 
#' 
#' @seealso 
#' \code{\link[predped]{create_edges}},
#' \code{\link[predped]{create_nodes}}
#' 
#' @rdname combine_nodes
#' 
#' @export
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
#' This function combines all non-dynamically determined nodes into a 
#' comprehensive graph that can then be used in the simulation as 
#' \code{precomputed_edges}. Effectively uses \code{\link[predped]{create_edges}}
#' to create the edges and then deletes the \code{agent} and \code{goal} nodes 
#' from the graph. 
#' 
#' @param background Object of the \code{\link[predped]{background-class}}.
#' @param space_between Numeric denoting the space to leave between the 
#' circumference of the object and the nodes created under the hood (see
#' \code{\link[predped]{add_nodes}}). Defaults to 2.5 times the maximal 
#' possible radius from the default \code{\link[predped]{params_from_csv}}.
#' @param many_nodes ogical denoting whether to create many nodes or leave 
#' it at the minimum. If \code{FALSE}, nodes are only added at the outlines of 
#' the objects contained within the \code{objects} slot of \code{background}. 
#' If \code{TRUE}, 400 additional nodes are added at an equal distance in the 
#' x-direction (20 nodes) and an equal distance in the y-direction (20 nodes), 
#' making the 20 x 20 = 400 additional nodes. Defaults to \code{FALSE}.
#' 
#' @return List containing a dataframe with the surviving nodes under 
#' \code{"nodes"}, a dataframe with the surviving connections between nodes 
#' under \code{"edges"}, and a similar dataframe to the previous one but with 
#' the coordinates still in there under \code{"edges_with_coords"}.
#' 
#' @examples
#' # Define a background in which the agent can move
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(rectangle(center = c(0, 0), 
#'                                                      size = c(1, 1))))
#' 
#' edges <- compute_edges(my_background, 
#'                        space_between = 0.25)
#' 
#' # Check the nodes and edges created
#' head(edges$nodes)
#' head(edges$edges)
#' head(edges$edges_with_coords)
#' 
#' @seealso 
#' \code{\link[predped]{adjust_edges}}
#' \code{\link[predped]{create_edges}}
#' \code{\link[predped]{create_nodes}}
#' \code{\link[predped]{find_path}}
#' \code{\link[predped]{simulate-predped}}
#' \code{\link[predped]{simulate-state}}
#' 
#' @rdname compute_edges 
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