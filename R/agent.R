#' An S4 Class to Represent Agents.
#' 
#' Defines the `agent` class, which contains all characteristics of an agent. 
#' Some of these characteristics are time-independent (e.g., parameters and 
#' set of goals) while others change during each time step in the simulation or 
#' estimation (e.g., speed and orientation).
#'
#' @slot id A numerical index for the agent
#' @slot position A numerical vector of two elements denoting the position of
#' the agent in 2D space
#' @slot size A numeric indicating the size of the radius of the agent
#' @slot orientation A numeric denoting the orientation of the agent in degrees
#' @slot interacting A logical denoting whether the agent is interacting with
#' something in its environment
#' @slot parameters A named list that contains the individual-specific parameters
#' for the agent
#' @slot goals A list containing the id's of the goals that are assigned to the
#' agent
#' 
#' @slot status A character that denotes the status of the agent, or what the 
#' agent is doing at the moment. Can take on `"move"`, `"stop"`, `"reorient"`, 
#' and `"exit"`
#'
#' @export
agent <- setClass("agent", list(id = "character",
                                speed = "numeric",
                                orientation = "numeric",
                                group = "numeric",
                                cell = "numeric",
                                status = "character",
                                parameters = "data.frame",
                                goals = "list",
                                current_goal = "goal",
                                waiting_counter = "numeric",
                                color = "character"), contains = c("circle"))

setMethod("initialize", "agent", function(.Object,
                                          id = character(0),
                                          speed = 1,
                                          orientation = 0,
                                          group = 0,
                                          cell = 0,
                                          status = "move",
                                          moveable = TRUE,
                                          interactable = TRUE,
                                          color = "black",
                                          waiting_counter = 0,
                                          ...
) {
    .Object <- callNextMethod(.Object, moveable = moveable, interactable = interactable, ...)

    .Object@id <- if (length(id) == 0) paste(sample(letters, 5, replace = TRUE), collapse = "") else id
    .Object@speed <- speed
    .Object@orientation <- orientation
    .Object@group <- group
    .Object@cell <- cell
    .Object@status <- status
    .Object@color <- color
    .Object@waiting_counter <- waiting_counter

    return(.Object)
})

setMethod("show", "agent", function(object) {
    cat(crayon::bold("Agent Attributes"), "\n", "\n")
    cat("Agent ID:", object@id, "\n")
    cat("Agent Speed:", object@speed, "\n")
    cat("Agent Orientation:", object@orientation, "\n")
    cat("Agent Group:", object@group, "\n")
    cat("Agent Cell:", object@cell, "\n")
    cat("Agent Status:", object@status, "\n")
    cat("Agent Parameters:", "\n")
    print(object@parameters[, 1:12])
    cat("Agent Color:", object@color, "\n")
    cat("\n", "\n", "Message: to view all agent parameters call", "\n",
    "`object@parameters` to see the full data.frame", "\n", "\n")
    cat("Agent Goals:", length(object@goals), "\n")
    cat("Message: more detailed information on agent goals", "\n",
    "can be obtained by calling `object@goals` ", "\n", "\n")
    cat("Agent Center:", object@center, "\n")
})


#' @rdname agent-class
#'
#' @export
setMethod("id", "agent", function(object) {
    return(setNames(object@id, object@id))
})

#' @rdname agent-class
#'
#' @export
setMethod("id<-", "agent", function(object, value) {
    object@id <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setMethod("position", "agent", function(object, return_matrix = FALSE) {
    if (return_matrix) {
        return(matrix(object@center, nrow = 1, ncol = 2, dimnames = list(object@id, names(object@center))))
    }
    return(object@center)
})

#' @rdname agent-class
#' 
#' @export
setMethod("position<-", "agent", function(object, value) {
    object@center <- as(value, "coordinate")
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setMethod("size", "agent", function(object) {
    return(setNames(object@radius, object@id))
})

#' @rdname agent-class
#'
#' @export 
setMethod("size<-", "agent", function(object, value) {
    object@radius <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setGeneric("speed", function(object) standardGeneric("speed"))

#' @rdname agent-class
#' 
#' @export
setGeneric("speed<-", function(object, value) standardGeneric("speed<-"))

setMethod("speed", "agent", function(object) {
    return(setNames(object@speed, object@id))
})

setMethod("speed<-", "agent", function(object, value) {
    object@speed <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setMethod("orientation", "agent", function(object) {
    return(setNames(object@orientation, object@id))
})

#' @rdname agent-class
#' 
#' @export
setMethod("orientation<-", "agent", function(object, value) {
    object@orientation <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setGeneric("group", function(object) standardGeneric("group"))

#' @rdname agent-class
#' 
#' @export
setGeneric("group<-", function(object, value) standardGeneric("group<-"))

setMethod("group", "agent", function(object) {
    return(setNames(object@group, object@id))
})

setMethod("group<-", "agent", function(object, value) {
    object@group <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setGeneric("cell", function(object) standardGeneric("cell"))

#' @rdname agent-class
#' 
#' @export
setGeneric("cell<-", function(object, value) standardGeneric("cell<-"))

setMethod("cell", "agent", function(object) {
    return(setNames(object@cell, object@id))
})

setMethod("cell<-", "agent", function(object, value) {
    object@cell <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setGeneric("status", function(object) standardGeneric("status"))

#' @rdname agent-class
#' 
#' @export
setGeneric("status<-", function(object, value) standardGeneric("status<-"))

setMethod("status", "agent", function(object) {
    return(setNames(object@status, object@id))
})

setMethod("status<-", "agent", function(object, value) {
    stopifnot(value %in% c("move", "replan", "reorient", "completing goal", "exit", "wait"))
    object@status <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setGeneric("parameters", function(object) standardGeneric("parameters"))

#' @rdname agent-class
#' 
#' @export
setGeneric("parameters<-", function(object, value) standardGeneric("parameters<-"))

setMethod("parameters", "agent", function(object) {
    return(object@parameters)
})

setMethod("parameters<-", "agent", function(object, value) {
    object@parameters <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setGeneric("goals", function(object) standardGeneric("goals"))

#' @rdname agent-class
#' 
#' @export
setGeneric("goals<-", function(object, value) standardGeneric("goals<-"))

setMethod("goals", "agent", function(object) {
    return(object@goals)
})

setMethod("goals<-", "agent", function(object, value) {
    object@goals <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setGeneric("current_goal", function(object) standardGeneric("current_goal"))

#' @rdname agent-class
#' 
#' @export
setGeneric("current_goal<-", function(object, value) standardGeneric("current_goal<-"))

setMethod("current_goal", "agent", function(object) {
    return(object@current_goal)
})

setMethod("current_goal<-", "agent", function(object, value) {
    object@current_goal <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setGeneric("color", function(object) standardGeneric("color"))

#' @rdname agent-class
#' 
#' @export
setGeneric("color<-", function(object, value) standardGeneric("color<-"))

setMethod("color", "agent", function(object) {
    return(setNames(object@color, object@id))
})

setMethod("color<-", "agent", function(object, value) {
    object@color <- value
    return(object)
})

#' @rdname agent-class
#' 
#' @export
setGeneric("waiting_counter", function(object) standardGeneric("waiting_counter"))

#' @rdname agent-class
#' 
#' @export
setGeneric("waiting_counter<-", function(object, value) standardGeneric("waiting_counter<-"))

setMethod("waiting_counter", "agent", function(object) {
    return(setNames(object@waiting_counter, object@id))
})

setMethod("waiting_counter<-", "agent", function(object, value) {
    object@waiting_counter <- value
    return(object)
})