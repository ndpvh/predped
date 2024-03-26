#' An S4 Class to Represent Goals
#' 
#' Defines the `goal` class, which contains all characteristics of a goal that 
#' the agents can pursue.
#'
#' @slot id A numerical index for the agent
#' @slot position A coordinate
#' @slot busy A logical denoting whether it is currently being interacted with
#' @slot counter An integer denoting the number of time steps the agent should 
#' interact with the goal before moving on to the next one
#'
#' @export
goal <- setClass("goal", list(id = "character",
                              position = "coordinate",
                              path = "matrix",
                              busy = "logical",
                              done = "logical",
                              counter = "numeric"))

setMethod("initialize", "goal", function(.Object,
                                         id = character(0),
                                         position = numeric(2),
                                         path = matrix(0, nrow = 2, ncol = 2),
                                         busy = FALSE,
                                         done = FALSE,
                                         counter = 5,
                                         ...
) {

    .Object@id <- if(length(id) == 0) paste("goal", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id
    .Object@position <- coordinate(position)
    .Object@busy <- busy
    .Object@done <- done
    .Object@counter <- counter
    .Object@path <- path

    return(.Object)
})

setGeneric("interact", function(object) standardGeneric("interact"))

#' Interact with goal
#' 
#' Defines the interaction with the goal. In effect changes the counter until 
#' it reaches 0, after which it deletes the goal from the goal stack.
#'
#' @param goal The goal with which the agent is interacting
#'
#' @export
setMethod("interact", "goal", function(object) 
{
    if(object@counter <= 0) {
        object@done <- TRUE
        return(object)
    } else {
        object@counter <- object@counter - 1
        return(object)
    }
})


setGeneric("replace", function(object,...) standardGeneric("replace"))

#' Replace a goal
#' 
#' Replace the existing goal with an alternative. This allows for the dynamically
#' assigning goals, as we do in the experiment (one goal at the time).
#' 
#' @param goal The goal that should be replaced
#' @param setting List containing all objects in the environment/setting
#' 
#' @export 
setMethod("replace", "goal", function(object, 
                                      setting,
                                      counter_generator = \(x) rnorm(x, 10, 2)
) {
    return(generate_goal_stack(1, setting, counter_generator)[[1]])
})

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
                                counter_generator = \(x) rnorm(x, 10, 2)) {
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

    return(goal_stack)
}

#' Add a Goal to an Object
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#' @param id Character denoting the name of the goal. Defaults to an empty 
#' character, creating a random one under the hood.
#' @param counter Integer denoting the number of iterations an agent has to 
#' interact with the goal in order to complete it
#' @param ... Arguments passed on to \code{\link[predped]{rng_point-method}}
#'
#' @return The goal that was assigned to the object
#' @export
#' @name add_goal-method
# 
# Thought it would be more logical to place this method to the objects here, 
# even though it is not a method of the `goal` class.
setGeneric("add_goal", function(object, ...) standardGeneric("add_goal"))

#'@rdname add_goal-method
#'
setMethod("add_goal", signature(object = "polygon"), function(object, 
                                                              background,
                                                              id = character(0),
                                                              counter = 5,
                                                              middle_edge = FALSE,
                                                              forbidden = NULL){
    # Create an ever so slightly bigger polygon
    #
    # Is in response to a bug that appears when `m4ma::seesGoal` is used to 
    # check whether the location of the goal can be seen from a given node 
    # when making the path points: Given that the goal is part of the object, 
    # it is always unseen. With a slightly bigger object, this is not the case
    # anymore
    new_points <- add_nodes(object, only_corners = TRUE, space_between = 1e-2)
    new_object <- polygon(points = new_points)

    # Generate a random goal and check whether or not this goal is not contained
    # within another object within the environment
    obj <- objects(background)
    shp <- shape(background)

    not_okay <- TRUE
    while(not_okay) {
        co <- rng_point(new_object, 
                    middle_edge = middle_edge, 
                    forbidden = forbidden)

        check <- sapply(obj,
                        \(x) in_object(x, co, outside = FALSE))
        not_okay <- in_object(shp, co, outside = TRUE) | any(check)
    }
    
    return(goal(id = id,
                position = coordinate(co), 
                counter = counter))    
})

#'@rdname add_goal-method
#'
setMethod("add_goal", signature(object = "circle"), function(object, 
                                                             id = character(0),
                                                             counter = 5,
                                                             forbidden = NULL
){
    # Create an ever so slightly bigger circle
    #
    # Is in response to a bug that appears when `m4ma::seesGoal` is used to 
    # check whether the location of the goal can be seen from a given node 
    # when making the path points: Given that the goal is part of the object, 
    # it is always unseen. With a slightly bigger object, this is not the case
    # anymore
    new_object <- circle(center = center(object), 
                         radius = radius(object) + 1e-2)

    co <- rng_point(new_object, 
                    forbidden = forbidden)
    return(goal(id = id,
                position = coordinate(co), 
                counter = counter))   
})

#' Find path to a goal
#' 
#' Defines a list of coordinates that create a strategic path to the goal of the
#' agents.
#'
#' @param goal The goal with which the agent is interacting
#' @param agent The agent that wants to go to the goal.
#' @param background The background/setting that is being used.
#' @param algorithm Algorithm to use for creating the path-points. Is provided
#' to the `get_path_pair` function of `cppRouting`. Defaults to "bi", using 
#' bidirectional Dijkstra.
#'
#' @export
#' @name find_path-method
setGeneric("find_path", function(object, ...) standardGeneric("find_path"))

#' @rdname find_path-method
#' 
setMethod("find_path", "goal", function(object, 
                                        agent,
                                        background,
                                        algorithm = "bi",
                                        space_between = radius(agent)) {
                                            
    # Create the edges that are taken in by `makegraph`
    edges <- create_edges(position(agent),
                          position(object), 
                          background,
                          space_between = space_between)
    
    # Create a graph that can be used by `cppRouting`. In constrast to Andrew, 
    # I put the directed argument to FALSE so that agents are free to decide on
    # their own which way to go (otherwise, order of nodes matters)
    graph <- cppRouting::makegraph(edges$edges, 
                                   directed = FALSE,
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

    rownames(path_points) <- NULL
    colnames(path_points) <- c("x", "y")    

    return(path_points)
})



# Getters and setters

#' @rdname goal-class
setGeneric("position", function(object, return_matrix = FALSE) standardGeneric("position"))

#' @rdname goal-class
#' 
#' @export
setGeneric("position<-", function(object, value) standardGeneric("position<-"))

#' @rdname goal-class
#' 
#' @export
setMethod("position", "goal", function(object) {
    return(object@position)
})

#' @rdname goal-class
#' 
#' @export
setMethod("position<-", "goal", function(object, value) {
    object@position <- value
    return(object)
})


setGeneric("id", function(object) standardGeneric("id"))

setGeneric("id<-", function(object, value) standardGeneric("id<-"))

#' @rdname goal-class
#'
#' @export
setMethod("id", "goal", function(object) {
    return(object@id)
})

#' @rdname goal-class
#' 
#' @export
setMethod("id<-", "goal", function(object, value) {
    object@id <- value
    return(object)
})

#' @rdname goal-class
#' 
#' @export
setGeneric("path", function(object) standardGeneric("path"))

#' @rdname goal-class
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

#' @rdname goal-class
#' 
#' @export
setGeneric("counter", function(object) standardGeneric("counter"))

#' @rdname goal-class
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

#' @rdname goal-class
#' 
#' @export
setGeneric("busy", function(object) standardGeneric("busy"))

#' @rdname goal-class
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

#' @rdname goal-class
setGeneric("done", function(object, return_matrix = FALSE) standardGeneric("done"))

#' @rdname goal-class
#' 
#' @export
setGeneric("done<-", function(object, value) standardGeneric("done<-"))

#' @rdname goal-class
#' 
#' @export
setMethod("done", "goal", function(object) {
    return(object@done)
})

#' @rdname goal-class
#' 
#' @export
setMethod("done<-", "goal", function(object, value) {
    object@done <- value
    return(object)
})