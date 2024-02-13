#' An S4 Class to Represent Agents
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
#' @export
agent <- setClass("agent", list(id = "character",
                                speed = "numeric",
                                orientation = "numeric",
                                group = "numeric",
                                cell = "numeric",
                                status = "character",
                                parameters = "numeric",
                                goals = "matrix",
                                current_goal = "matrix"
                                ), contains = c("circle"))

setMethod("initialize", "agent", function(.Object,
                                          id = character(0),
                                          speed = 1,
                                          orientation = 0,
                                          group = 0,
                                          cell = 0,
                                          status = "move",
                                          moveable = TRUE,
                                          interactable = TRUE,
                                          ...
) {
    .Object <- callNextMethod(.Object, moveable = moveable, interactable = interactable, ...)

    .Object@id <- if (length(id) == 0) paste(sample(letters, 5, replace = TRUE), collapse = "") else id
    .Object@speed <- speed
    .Object@orientation <- orientation
    .Object@group <- group
    .Object@cell <- cell
    .Object@status <- status

    return(.Object)
})

#' @rdname agent
#'
#' @export
setGeneric("id", function(object) standardGeneric("id"))

#' @rdname agent
#'
#' @export
setGeneric("id<-", function(object, value) standardGeneric("id<-"))

setMethod("id", "agent", function(object) {
    return(setNames(object@id, object@id))
})

setMethod("id<-", "agent", function(object, value) {
    object@id <- value
    return(object)
})

#' @export
setGeneric("position", function(object, return_matrix = FALSE) standardGeneric("position"))

#' @export
setGeneric("position<-", function(object, value) standardGeneric("position<-"))

setMethod("position", "agent", function(object, return_matrix = FALSE) {
    if (return_matrix) {
        return(matrix(object@center, nrow = 1, ncol = 2, dimnames = list(object@id, names(object@center))))
    }
    return(object@center)
})

setMethod("position<-", "agent", function(object, value) {
    object@center <- as(value, "coordinate")
    return(object)
})

#' @export
setGeneric("size", function(object) standardGeneric("size"))

setMethod("size", "agent", function(object) {
    return(setNames(object@radius, object@id))
})

#' @export
setGeneric("speed", function(object) standardGeneric("speed"))

#' @export
setGeneric("speed<-", function(object, value) standardGeneric("speed<-"))

setMethod("speed", "agent", function(object) {
    return(setNames(object@speed, object@id))
})

setMethod("speed<-", "agent", function(object, value) {
    object@speed <- value
    return(object)
})

#' @export
setGeneric("orientation", function(object) standardGeneric("orientation"))

#' @export
setGeneric("orientation<-", function(object, value) standardGeneric("orientation<-"))

setMethod("orientation", "agent", function(object) {
    return(setNames(object@orientation, object@id))
})

setMethod("orientation<-", "agent", function(object, value) {
    object@orientation <- value
    return(object)
})

#' @export
setGeneric("group", function(object) standardGeneric("group"))

#' @export
setGeneric("group<-", function(object, value) standardGeneric("group<-"))

setMethod("group", "agent", function(object) {
    return(setNames(object@group, object@id))
})

setMethod("group<-", "agent", function(object, value) {
    object@group <- value
    return(object)
})

#' @export
setGeneric("cell", function(object) standardGeneric("cell"))

#' @export
setGeneric("cell<-", function(object, value) standardGeneric("cell<-"))

setMethod("cell", "agent", function(object) {
    return(setNames(object@cell, object@id))
})

setMethod("cell<-", "agent", function(object, value) {
    object@cell <- value
    return(object)
})

#' @export
setGeneric("status", function(object) standardGeneric("status"))

#' @export
setGeneric("status<-", function(object, value) standardGeneric("status<-"))

setMethod("status", "agent", function(object) {
    return(setNames(object@status, object@id))
})

setMethod("status<-", "agent", function(object, value) {
    stopifnot(value %in% c("move", "stop", "reorient"))
    object@status <- value
    return(object)
})

#' @export
setGeneric("parameters", function(object) standardGeneric("parameters"))

#' @export
setGeneric("parameters<-", function(object, value) standardGeneric("parameters<-"))

setMethod("parameters", "agent", function(object) {
    return(object@parameters)
})

setMethod("parameters<-", "agent", function(object, value) {
    object@parameters <- value
    return(object)
})

#' @export
setGeneric("goals", function(object) standardGeneric("goals"))

#' @export
setGeneric("goals<-", function(object, value) standardGeneric("goals<-"))

setMethod("goals", "agent", function(object) {
    return(object@goals)
})

setMethod("goals<-", "agent", function(object, value) {
    object@goals <- value
    return(object)
})

#' @export
setGeneric("current_goal", function(object) standardGeneric("current_goal"))

#' @export
setGeneric("current_goal<-", function(object, value) standardGeneric("current_goal<-"))

setMethod("current_goal", "agent", function(object) {
    return(object@current_goal)
})

setMethod("current_goal<-", "agent", function(object, value) {
    object@current_goal <- value
    return(object)
})
