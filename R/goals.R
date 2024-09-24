#' An S4 Class to Represent Goals
#' 
#' Defines the \code{goal} class, which contains all characteristics of a goal that 
#' the agents can pursue.
#'
#' @slot id Character that serves as an identifier for the goal.
#' @slot position Numerical vector denoting the position of the goal.
#' @slot path Numerical matrix of size n x 2 denoting the different intermediate
#' path points between an agent and a goal. In effect defines which path the 
#' agent will take to move towards the goal.
#' @slot busy Logical denoting whether the goal is currently being interacted 
#' with.
#' @slot done Logical denoting whether a goal has been completed.
#' @slot counter Numeric denoting the number of time steps the agent should 
#' interact with the goal before the goal has been completed. 
#' 
#' @rdname goal-class
#' 
#' @seealso 
#' \code{\link[predped]{busy-method}},
#' \code{\link[predped]{counter-method}},
#' \code{\link[predped]{done-method}},
#' \code{\link[predped]{id-method}},
#' \code{\link[predped]{path-method}},
#' \code{\link[predped]{position-method}},
#' \code{\link[predped]{initialize-goal}}
#'
#' @export
#
# TO DO: Phase out coordinate
goal <- setClass("goal", list(id = "character",
                              position = "coordinate",
                              path = "matrix",
                              busy = "logical",
                              done = "logical",
                              counter = "numeric"))

#' Constructor for the \code{\link[predped]{goal-class}}
#' 
#' @param id Character that serves as an identifier for the goal. Defaults to 
#' an empty character, triggering the random generation of an id.
#' @param position Numerical vector denoting the position of the goal. Defaults 
#' to \code{c(0, 0)}.
#' @param path Numerical matrix of size n x 2 denoting the different intermediate
#' path points between an agent and a goal. In effect defines which path the 
#' agent will take to move towards the goal. Defaults to an empty matrix of size 
#' 2 x 2.
#' @param busy Logical denoting whether the goal is currently being interacted 
#' with. Defaults to \code{FALSE}.
#' @param done Logical denoting whether a goal has been completed. Defaults to 
#' \code{FALSE}.
#' @param counter Numeric denoting the number of time steps the agent should 
#' interact with the goal before the goal has been completed. Defaults to \code{5}.
#' 
#' @return Object of the \code{\link[predped]{goal-class}}
#' 
#' @examples
#' # Initialize agent
#' my_goal <- goal(id = "my goal", position = c(1, 1))
#' 
#' # Access the two slots that were specified
#' my_goal@id
#' my_goal@position
#' 
#' @seealso 
#' \code{\link[predped]{goal-class}},
#' \code{\link[predped]{busy-method}},
#' \code{\link[predped]{counter-method}},
#' \code{\link[predped]{done-method}},
#' \code{\link[predped]{id-method}},
#' \code{\link[predped]{path-method}},
#' \code{\link[predped]{position-method}}
#' 
#' @rdname initialize-goal-method
#' 
#' @export
setMethod("initialize", "goal", function(.Object,
                                         id = character(0),
                                         position = numeric(2),
                                         path = matrix(0, nrow = 2, ncol = 2),
                                         busy = FALSE,
                                         done = FALSE,
                                         counter = 5) {

    .Object@id <- if(length(id) == 0) paste("goal", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id
    .Object@position <- coordinate(position)
    .Object@busy <- busy
    .Object@done <- done
    .Object@counter <- counter
    .Object@path <- path

    return(.Object)
})

#' Show method for the \code{\link[predped]{goal-class}}
#' 
#' @export
setMethod("show", "goal", function(object) {
    cat(crayon::bold("Goal Attributes"), "\n")
    cat("busy:", object@busy, "\n")
    cat("counter:", object@counter, "\n")
    cat("done:", object@done, "\n")
    cat("id:", object@id, "\n")
    cat("path:\n")
    print(object@path)
    cat("\nposition:", object@position, "\n")
    cat("\nFor more detailed information, please extract the wanted information from the background directly.\n")
})

#' Add a goal to an object
#' 
#' Takes in an instance of \code{\link[predped]{object-class}} and uses its 
#' characteristics to generate an instance of \code{\link[predped]{goal-class}}
#' that is attached to it. In practice, uses one of the objects in the 
#' \code{objects} slot of a \code{\link[predped]{background-class}} instance. 
#' 
#' Is defined for all instances of \code{\link[predped]{object-class}} except for 
#' \code{\link[predped]{segment-class}}, which cannot contain goals due to its 
#' primary function.
#'
#' @param object Object or instance of the \code{\link[predped]{object-class}}.
#' @param background Object of \code{\link[predped]{background-class}} containing
#' all objects in the environment that can have a goal attached to them in its 
#' \code{objects} slot.
#' @param id Character that serves as an identifier for the goal. Defaults to 
#' an empty character, triggering the random generation of an id.
#' @param counter Numeric denoting the number of time steps the agent should 
#' interact with the goal before the goal has been completed. Defaults to \code{5}.
#' @param ... Arguments passed on to \code{\link[predped]{rng_point-method}}
#'
#' @return Object of \code{\link[predped]{goal-class}}.
#' 
#' @examples 
#' # Create a background
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)))
#' 
#' # Adjust the objects in the background for each of the different objects to 
#' # showcase how add_goals works for each
#' objects(my_background) <- list(circle(center = c(0, 0), radius = 0.5))
#' add_goal(my_background)
#' 
#' objects(my_background) <- list(rectangle(center = c(0, 0), size = c(1, 1)))
#' add_goal(my_background)
#' 
#' objects(my_background) <- list(polygon(points = cbind(c(0.5, 0.5, -0.5, -0.5), 
#'                                                       c(0.5, -0.5, -0.5, 0.5)))
#' add_goal(my_background)
#' 
#' @seealso 
#' \code{\link[predped]{background-class}}
#' \code{\link[predped]{circle-class}}
#' \code{\link[predped]{goal-class}}
#' \code{\link[predped]{polygon-class}}
#' \code{\link[predped]{rectangle-class}}
#' \code{\link[predped]{segment-class}}
#' \code{\link[predped]{rng_point-method}}
#' 
#' @rdname add_goal-method
#' 
#' @export
# 
# Thought it would be more logical to place this method to the objects here, 
# even though it is not a method of the `goal` class.
setGeneric("add_goal", function(object, ...) standardGeneric("add_goal"))

setMethod("add_goal", signature(object = "polygon"), function(object, 
                                                              background,
                                                              id = character(0),
                                                              counter = 5,
                                                              ...){
    # Create an ever so slightly bigger polygon
    #
    # Is in response to a bug that appears when `m4ma::seesGoal` is used to 
    # check whether the location of the goal can be seen from a given node 
    # when making the path points: Given that the goal is part of the object, 
    # it is always unseen. With a slightly bigger object, this is not the case
    # anymore
    points(object) <- add_nodes(object, 
                                only_corners = TRUE, 
                                space_between = 1e-2)

    # Generate a random goal and check whether or not this goal is not contained
    # within another object within the environment
    obj <- objects(background)
    shp <- shape(background)

    continue <- TRUE
    while(continue) {
        # Generate a random point on the polygon
        coord <- rng_point(object, ...)

        # Check whether this point is not contained within any other objects, 
        # but is still contained within the setting
        check <- sapply(obj, \(x) in_object(x, coord, outside = FALSE))
        continue <- in_object(shp, coord, outside = TRUE) | any(check)
    }
    
    return(goal(id = id,
                position = coord, 
                counter = counter))    
})

setMethod("add_goal", signature(object = "circle"), function(object, 
                                                             background,
                                                             id = character(0),
                                                             counter = 5,
                                                             ...){

    # Create an ever so slightly bigger circle
    #
    # Is in response to a bug that appears when `m4ma::seesGoal` is used to 
    # check whether the location of the goal can be seen from a given node 
    # when making the path points: Given that the goal is part of the object, 
    # it is always unseen. With a slightly bigger object, this is not the case
    # anymore
    radius(object) <- radius(object) + 1e-2

    # Generate a random goal and check whether or not this goal is not contained
    # within another object within the environment
    obj <- objects(background)
    shp <- shape(background)

    continue <- TRUE
    while(continue) {
        # Generate a random point on the polygon
        coord <- rng_point(object, ...)

        # Check whether this point is not contained within any other objects, 
        # but is still contained within the setting
        check <- sapply(obj, \(x) in_object(x, coord, outside = FALSE))
        continue <- in_object(shp, coord, outside = TRUE) | any(check)
    }
    
    return(goal(id = id,
                position = coord, 
                counter = counter))   
})

#' Find path to a goal
#' 
#' Creates a numerical matrix containing coordinates of path points which, 
#' together, form a strategic path to the goal specified in this function. This
#' forms one of the functions that operate on the strategic and tactical level 
#' of the model.
#' 
#' To create this path, the function takes the following approach. First, it 
#' will create a network of nodes that are connected through unidirectional 
#' edges, as created by the \code{\link[predped]{create_edges}} function. 
#' Nodes are created based on the objects in the environment, so that the nodes
#' are always strategically placed at a certain distance away from the objects
#' in the environment (at a distance \code{space_between}). If the argument
#' \code{many_nodes = TRUE}, then a grid of other nodes are added to this 
#' list of initial nodes. This grid consists of nodes that are placed at an 
#' equal distance apart in the x- and y-direction, and spans 20 rows and 20 
#' columns. Once the nodes have been generated, those nodes that fall within 
#' any of the objects in the environment or falls outside of the environment 
#' are deleted. Unidirectional edges connect the different nodes and are 
#' similarly pruned based on intersections with objects or, when defined, 
#' intersections with instances of the \code{\link[predped]{segment-class}}.
#' 
#' Once the network of nodes and edges has been created, we translate this 
#' network into a graph that can be used by the \code{ccpRouting} package. 
#' Specifically, we use \code{\link[cppRouting]{makegraph}} for this purpose.
#' We then use the \code{\link[cppRouting]{get_path_pair}} function of the 
#' \code{cppRouting} package to find the shortest route to the goal. The user 
#' can specify which algorithm to use for this optimization through the argument 
#' \code{algorithm}, but the algorithm should be unidirectional to ensure that 
#' one-way walking can be enforced (as controlled through the \code{limited_access}
#' slot in the \code{\link[predped]{background-class}}).
#' 
#' Finally, the optimal path is checked for any redundancy and formatted so that 
#' the path points are contained in an n x 2 matrix. 
#'
#' @param object Object of \code{\link[predped]{goal-class}} for which the path
#' should be computed.
#' @param agent Object of \code{\link[predped]{agent-class}} who wants to move 
#' towards the goal defined in argument \code{object}.
#' @param background Object of \code{\link[predped]{background-class}} within 
#' which the goal and agent are contained.
#' @param space_between Numeric denoting the amount of space to leave between 
#' an object and a path point. Within this function, the default is the radius 
#' of the agent that is provided. However, in the \code{\link[predped]{simulate-method}}
#' function, this default is overwritten to be equal to \code{2.5} times this 
#' radius.
#' @param many_nodes Logical denoting whether to create many nodes or leave 
#' it at the minimum. Defaults to \code{FALSE}.
#' @param algorithm Character denoting the algorithm to be used for optimizing 
#' the path towards the goal. Is provided to the 
#' \code{\link[cppRouting]{get_path_pair}} function in the \code{cppRouting}
#' package. Defaults to \code{"Dijkstra"}. Note that one-way routing as 
#' controlled by the \code{limited_access} slot in the \code{background} may not 
#' work if a bidirectional algorithm is used.
#' @param precomputed_edges List containing the network of nodes and edges 
#' for the environment, as created by \code{\link[predped]{compute_edges}}. 
#' Defaults to \code{NULL}, triggering the creation of these edges again.
#' @param new_objects List of instances of \code{\link[predped]{object-class}} 
#' that are not yet contained in the \code{objects} slot in the argument 
#' \code{background}. Usually consists of other agents that the \code{agent}
#' has to account for when planning their route. Defaults to \code{NULL}.
#' @param reevaluate Logical denoting whether to reevaluate the network that is
#' provided in \code{precomputed_edges}. Is useful whenever \code{new_objects}
#' is not \code{NULL}, allowing us to check whether some nodes and edges are now
#' occluded by the new objects. Defaults to \code{FALSE}.
#' 
#' @return Numerical matrix of coordinates representing the path points the 
#' agent will have to move to to reach the goal.
#' 
#' @examples
#' # Create a setting
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)),
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5)))
#' 
#' # Create an agent that is walking around there and a goal that the agent 
#' # will move to
#' my_goal <- add_goal(objects(my_background)[[1]], my_background)
#' my_agent <- agent(center = c(0.7, 0.7), radius = 0.25)
#' 
#' # Find the path of the agent to his goal
#' find_path(my_goal, 
#'           my_agent,
#'           my_background)
#' 
#' @seealso
#' \code{\link[predped]{agent-class}}
#' \code{\link[predped]{background-class}}
#' \code{\link[predped]{goal-class}}
#' \code{\link[predped]{object-class}}
#' \code{\link[predped]{simulate-method}}
#' \code{\link[predped]{adjust_edges}}
#' \code{\link[predped]{compute_edges}}
#' \code{\link[predped]{create_edges}}
#'
#' @rdname find_path-method
#' 
#' @export
setGeneric("find_path", function(object, ...) standardGeneric("find_path"))

setMethod("find_path", "goal", function(object, 
                                        agent,
                                        background,
                                        space_between = radius(agent),
                                        many_nodes = FALSE,
                                        algorithm = "Dijkstra",
                                        precomputed_edges = NULL,
                                        new_objects = NULL,
                                        reevaluate = FALSE) {
                                            
    # If there are no objects in the environment, you can just make a direct 
    # path between the agent and their goal
    if(length(objects(background)) == 0) {
        path_points <- matrix(position(object), nrow = 1)
        rownames(path_points) <- NULL
        colnames(path_points) <- c("x", "y")

        return(path_points)
    } 

    # Create the edges that are taken in by `makegraph`
    if(is.null(precomputed_edges)) {
        # If they are not precomputed, we should add the new objects to the 
        # objects in the environment before we can make the path
        if(!is.null(new_objects)) {
            objects(background) <- append(objects(background), 
                                          new_objects)
        }

        edges <- create_edges(position(agent),
                              position(object), 
                              background,
                              space_between = space_between,
                              many_nodes = many_nodes)
    } else {
        # If the edges are precomputed, adjust them to fit our current purpose
        edges <- adjust_edges(position(agent),
                              position(object),
                              background,
                              space_between = space_between,
                              new_objects = new_objects,
                              precomputed_edges = precomputed_edges,
                              reevaluate = reevaluate)
    }

    # If edges is actually NULL, we need to return this 
    if(is.null(edges)) {
        return(matrix(nrow = 0, ncol = 2))
    }
    
    # Create a graph that can be used by `cppRouting`. In constrast to Andrew, 
    # I put the directed argument to TRUE as the order of the nodes in the edges
    # matters
    graph <- cppRouting::makegraph(edges$edges, 
                                   directed = TRUE,
                                   coords = edges$nodes)

    # Make a check of whether "agent" and "goal" are contained within the nodes.
    # Might be deleted because of blocking, or because `make_graph` deletes them.
    if(!all(c("agent", "goal") %in% graph$coords$node_ID)) {
        return(matrix(nrow = 0, ncol = 2))
    }
    
    # Use cppRouting to do the strategic planning in this function
    path_points <- cppRouting::get_path_pair(graph,
                                             "agent", 
                                             "goal", 
                                             algorithm = algorithm,
                                             long = TRUE)

    # Once planned, transform the IDs of the nodes to actual coordinates to 
    # which the agent can walk. When merging, we should turn of `sort` to ensure 
    # that the order of the path_points is not changed.
    nodes <- edges$nodes |>
        setNames(c("node", "x", "y"))
    path_points <- merge(path_points, 
                         nodes, 
                         by = "node",
                         sort = FALSE)

    # Only retain the coordinates of interest (where the agent has to walk to) 
    # and transform to a matrix instead of a dataframe
    path_points <- path_points[-1, c("x", "y")] |>
        as.matrix()

    # Check whether you can delete some of the path points to make it a bit more 
    # concise. This only applies if there is more than one path point
    if(nrow(path_points) > 1) {
        # Set the original reference point to the position of the agent
        ref <- position(agent)

        # Loop over the path points starting at number 2 (number 1 being one 
        # that is guaranteed to be seen)
        for(i in 2:nrow(path_points)) {
            # Check whether agents can see the next path point starting from 
            # the reference point
            obj <- objects(background)
            if(length(obj) == 0) {
                seen <- TRUE
            } else {
                seen <- all(prune_edges(objects(background), 
                                        matrix(c(ref, path_points[i,]),
                                               nrow = 1)))
            }
            
            # If the path point is seen, you can just directly go to this path 
            # point instead of making a stop in the intermediate one
            if(seen) {
                path_points[i - 1,] <- NA

            # If the path point is not seen, that means the previous path point
            # is a critical one, and will therefore serve as the new reference
            } else {
                ref <- path_points[i - 1,]
            }
        }

        # Remove all unnecessary path points from the list
        path_points <- matrix(path_points[!is.na(path_points[,1]),],
                              ncol = 2)
    }

    # Some additional changes for convention
    rownames(path_points) <- NULL
    colnames(path_points) <- c("x", "y")    

    return(path_points)
})

#' Interact with goal
#' 
#' Defines the interaction with an object of \code{\link[predped]{goal-class}}. 
#' Subtracts \code{1} from the counter and indicates whether the goal has been 
#' accomplished (whenever the counter drops below 0).
#'
#' @param object Object of \code{\link[predped]{goal-class}}.
#' 
#' @return Object of \code{\link[predped]{goal-class}} with adjusted \code{counter}
#' slot and, if the goal has been completed, an adjusted \code{done} slot
#' 
#' @seealso 
#' \code{\link[predped]{goal-class}}
#' 
#' @rdname interact-method
#'
#' @export
setGeneric("interact", function(object) standardGeneric("interact"))

setMethod("interact", "goal", function(object) {
    # Decrease the counter and adjust the done slot
    object@counter <- object@counter - 1
    object@done <- object@counter <= 0

    return(object)
})

#' Replace a goal
#' 
#' Replaces an existing object of \code{\link[predped]{goal-class}} with an 
#' alternative object of the same class. The new goal is randomly generated or 
#' randomly drawn from a list of options (if provided). 
#' 
#' This function allows for a dynamical assignment of goals to agents.
#' 
#' @param object Object of \code{\link[predped]{goal-class}} which should be 
#' replaced by this function. 
#' @param setting Object of \code{\link[predped]{background-class}} within which
#' the goal should be created. Ignored if a list of potential goals is provided.
#' @param goal_list List containing instances of \code{\link[predped]{goal-class}}
#' from which the new goal should be chosen. Defaults to \code{NULL}, triggering 
#' the generation of a random goal.
#' @param counter_generator Function that takes in no arguments and generates 
#' a single numerical value that will be used as the counter of the goal. 
#' See \code{\link[predped]{generate_goal_stack}} for details on this argument.
#' Defaults to \code{\() rnorm(1, 10, 2)}.
#' 
#' @return Object of \code{\link[predped]{goal-class}}.
#' 
#' @seealso 
#' \code{\link[predped]{background-class}}
#' \code{\link[predped]{goal-class}}
#' \code{\link[predped]{generate_goal_stack}}
#' 
#' @rdname replace-method
#' 
#' @export 
setGeneric("replace", function(object,...) standardGeneric("replace"))

setMethod("replace", "goal", function(object, 
                                      setting,
                                      goal_list = NULL,
                                      counter_generator = \() rnorm(1, 10, 2)) {

    # If a goal-list is defined, we need to draw one of these goals as an 
    # alternative to the one being replaced.
    if(!is.null(goal_list)) {
        return(sample(goal_list, 1)[[1]])

    # Otherwise, generate a new one.
    } else {
        return(generate_goal_stack(1, setting, counter_generator)[[1]])
    }
})


######## LEFT OFF HERE #########################################################

#' Generate a goal stack
#' 
#' Use the defined environment or setting to generate the stack of goals an agent 
#' should complete.
#' 
#' @param n Integer denoting the number of goals to generate
#' @param setting List containing all objects in the environment/setting
#' @param counter_generator Function that defines how the counter for each goal 
#' should be generated. Defaults to a normal distribution with mean 10 and 
#' standard deviation 2.
#' 
#' @export 
generate_goal_stack <- function(n, 
                                setting,
                                counter_generator = \(x) rnorm(x, 10, 2),
                                agent_position = NULL,
                                precomputed_edges = NULL,
                                precompute_goal_paths = TRUE,
                                space_between = 0.5,
                                order_goal_stack = TRUE) {
    # Select the objects in the environment that can contain a goal
    potential_objects <- list()
    for(i in seq_along(setting@objects)) {
        if(setting@objects[[i]]@interactable) {
            potential_objects <- append(potential_objects, 
                                        setting@objects[[i]])
        }
    }

    # Throw an error if no objects are interactable
    if(length(potential_objects) == 0) {
        stop("None of the objects in the environment can contain a goal.")
    }

    # Randomly sample `n` objects from the `potential_objects` and assign a goal 
    # on one of its edges, as handled by the `add_goal` method. 
    sampled_objects <- sample(potential_objects, n, replace = TRUE)
    goal_stack <- lapply(sampled_objects, 
                         \(x) add_goal(x, 
                                       setting,
                                       id = character(0),
                                       counter = counter_generator(1)))

    # If you want the goal stack to be ordered according to distance, do so
    if(order_goal_stack) {
        # Compute the distance starting from the entrance (through which the agent
        # enters the space)
        if(is.null(agent_position)) {
            start <- entrance(setting)
        } else {
            start <- agent_position
        }

        distances <- sapply(goal_stack, 
                            \(x) (start[1] - position(x)[1])^2 + (start[2] - position(x)[2])^2)
        old <- sapply(goal_stack, \(x) x@id)
        

        # Sort the list of goals based on how close they are to the entrance
        distances <- cbind(1:n, distances)
        idx <- distances[order(distances[,2]), 1]

        goal_stack <- goal_stack[idx]
    }

    # If you don't want to or cannot precompute the goal paths, return the 
    # goal stack as is
    if(!precompute_goal_paths | length(goal_stack) == 1) {
        return(goal_stack)
    }

    # If you want to precompute the goal paths, loop over the last n - 1 goals
    # in the goal stack and find the path starting at the location of the 
    # previous goal. The path of the first goal is computed in the `add_agent`
    # function and does not need to be addressed here.
    dummy <- agent(center = c(0, 0), radius = space_between)
    for(i in 2:length(goal_stack)) {
        position(dummy) <- position(goal_stack[[i - 1]])
        goal_stack[[i]]@path <- find_path(goal_stack[[i]], 
                                          dummy, 
                                          setting,
                                          space_between = space_between,
                                          precomputed_edges = precomputed_edges)
    }

    return(goal_stack)
}

#' Simulate multiple goal stacks
#' 
#' Create multiple goal stacks that can be used in the simulation. Importantly, 
#' everything is precomputed here, meaning that all paths towards the goals are
#' already filled out in the simulation.
#' 
#' @param n Integer denoting the number of goal stacks to create
#' @param setting List containing all objects in the environment/setting
#' @param goal_number Integer, vector of integers, or function that determines 
#' how many goals each of the agents should receive. Defaults to a function that 
#' draws `x` numbers from a normal distribution with mean 10 and standard 
#' deviation 2. 
#' @param goal_duration Function that defines how the counter for each goal 
#' should be generated. Defaults to a normal distribution with mean 10 and 
#' standard deviation 2.
#' @param space_between Numeric denoting the amount of space that is needed 
#' between a path point and an object
#' @param order_goal_stack Logical denoting whether to order the goal stack 
#' based on distance.
#' 
#' @export 
# TO DO:
#   - Find a new name for this function: Not happy with how close it is to 
#     generate_goal_stack
simulate_goal_stack <- function(n, 
                                setting,
                                goal_number = \(x) rnorm(x, 10, 2), 
                                goal_duration = \(x) rnorm(x, 10, 2),
                                space_between = 0.5,
                                order_goal_stack = TRUE) {

    print("Precomputing goal stacks")

    # Define the number of goals per goal-stack
    goal_number <- draw_number(goal_number, n)

    # If `goal_duration` is not a function, make it a function anyway (assumed
    # by the `goal` class: To be changed)
    if(typeof(goal_duration) != "closure") {
        number <- goal_duration[1]
        goal_duration <- function(x) number
    }

    # Precompute the edges so that this might already take a bit less time
    edges <- create_edges(c(0, 0), 
                          c(0, 0), 
                          setting,
                          space_between = space_between)
    edges$edges <- edges$edges[!(edges$edges$from %in% c("agent", "goal")),]
    edges$edges <- edges$edges[!(edges$edges$to %in% c("agent", "goal")),]
    edges$nodes <- edges$nodes[!(edges$nodes$node_ID %in% c("agent", "goal")),]

    # Loop over the number of goal stacks to create
    goal_stack <- list()
    for(i in seq_len(n)) {
        print(paste0("Generating goal stack ", i))

        # Create the entrance through which the agent will have to enter to 
        # start the goal stack
        entrance_idx <- sample(seq_len(nrow(setting@entrance)), 1)

        # Generate the complete goal stack and precompute all the paths to the 
        # goals
        goal_stack[[i]] <- generate_goal_stack(goal_number[i], 
                                               setting,
                                               counter_generator = goal_duration,
                                               precomputed_edges = edges,
                                               precompute_goal_paths = TRUE,
                                               space_between = space_between,
                                               order_goal_stack = order_goal_stack)

        # The first goal within the goal stack does not have a path yet. Use the
        # entrance as the position at which the agent starts
        dummy <- agent(center = entrance(setting)[entrance_idx,], radius = space_between)
        goal_stack[[i]][[1]]@path <- find_path(goal_stack[[i]][[1]], 
                                               dummy, 
                                               setting,
                                               space_between = space_between,
                                               precomputed_edges = edges)
    }

    return(goal_stack)
}





################################################################################
# GETTERS AND SETTERS

#' Getter/Setter for the position-slot
#' 
#' @rdname position-method
#' 
#' @export
setGeneric("position", function(object, return_matrix = FALSE) standardGeneric("position"))

#' @rdname position-method
#' 
#' @export
setGeneric("position<-", function(object, value) standardGeneric("position<-"))

setMethod("position", "goal", function(object) {
    return(object@position)
})

setMethod("position<-", "goal", function(object, value) {
    object@position <- as(value, "coordinate")
    return(object)
})


setMethod("id", "goal", function(object) {
    return(object@id)
})

setMethod("id<-", "goal", function(object, value) {
    object@id <- value
    return(object)
})

#' Getter/Setter for the path-slot
#' 
#' @rdname path-method
#' 
#' @export
setGeneric("path", function(object) standardGeneric("path"))

#' @rdname path-method
#' 
#' @export
setGeneric("path<-", function(object, value) standardGeneric("path<-"))

setMethod("path", "goal", function(object) {
    return(object@path)
})

setMethod("path<-", "goal", function(object, value) {
    object@path <- value
    return(object)
})

#' Getter/Setter for the counter-slot
#' 
#' @rdname counter-method
#' 
#' @export
setGeneric("counter", function(object) standardGeneric("counter"))

#' @rdname counter-method
#' 
#' @export
setGeneric("counter<-", function(object, value) standardGeneric("counter<-"))

setMethod("counter", "goal", function(object) {
    return(object@counter)
})

setMethod("counter<-", "goal", function(object, value) {
    object@counter <- value
    return(object)
})

#' Getter/Setter for the busy-slot
#' 
#' @rdname busy-method
#' 
#' @export
setGeneric("busy", function(object) standardGeneric("busy"))

#' @rdname busy-method
#' 
#' @export
setGeneric("busy<-", function(object, value) standardGeneric("busy<-"))

setMethod("busy", "goal", function(object) {
    return(object@busy)
})

setMethod("busy<-", "goal", function(object, value) {
    object@busy <- value
    return(object)
})

#' Getter/Setter for the done-slot
#' 
#' @rdname done-method
#' 
#' @export
setGeneric("done", function(object, return_matrix = FALSE) standardGeneric("done"))

#' @rdname done-method
#' 
#' @export
setGeneric("done<-", function(object, value) standardGeneric("done<-"))

setMethod("done", "goal", function(object) {
    return(object@done)
})

setMethod("done<-", "goal", function(object, value) {
    object@done <- value
    return(object)
})