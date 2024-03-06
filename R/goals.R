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
                              busy = "logical",
                              counter = "numeric"))

setMethod("initialize", "goal", function(.Object,
                                         id = character(0),
                                         position = numeric(2),
                                         busy = FALSE,
                                         counter = 5,
                                         ...
) {

    .Object@id <- if(length(id) == 0) paste("goal", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id
    .Object@position <- coordinate(position)
    .Object@busy <- busy
    .Object@counter <- counter

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
        return(NULL)
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

    View(potential_objects)

    # Throw an error if no objects are interactable
    if(length(potential_objects) == 0) {
        stop("None of the objects in the environment can contain a goal.")
    }

    # Randomly sample `n` objects from the `potential_objects` and assign a goal 
    # on one of its edges, as handled by the `add_goal` method. 
    sampled_objects <- sample(potential_objects, n, replace = TRUE)
    goal_stack <- lapply(sampled_objects, 
                         \(x) add_goal(x, 
                                       id = character(0),
                                       counter = counter_generator(1)))

    return(goal_stack)
}

#' Add a Goal to an Object
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
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
                                                              id = character(0),
                                                              counter = 5
){
    co <- rng_point(object)
    return(goal(id = id,
                position = coordinate(co), 
                counter = counter))    
})

#'@rdname add_goal-method
#'
setMethod("add_goal", signature(object = "circle"), function(object, 
                                                             id = character(0),
                                                             counter = 5
){
    co <- rng_point(object)
    return(goal(id = id,
                position = coordinate(co), 
                counter = counter))   
})

