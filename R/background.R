#' An S4 Class to Represent the Background
#'
#' @slot shape An object of a type that extends \code{\link[predped]{object-class}}
#' defining the shape of the background.
#' @slot objects A list of objects of a type that extends
#' \code{\link[predped]{object-class}} defining the objects in the background.
#' @slot entry_exit_width A specification for the desired entrance and exit size
#' to be plotted when visualising the environment.
#' 
#' @export
background <- setClass("background", list(shape = "object", 
                                          objects = "list",
                                          entrance = "coordinate", 
                                          exit = "coordinate",
                                          entry_exit_width = "numeric"))

setMethod("initialize", "background", function(.Object, 
                                               shape,
                                               objects = list(),
                                               entrance = NULL,
                                               exit = NULL,
                                               entry_exit_width = 0.4,
                                               same_exit = TRUE,
                                               ...) {
    # If uncommented, callNextMethod() will throw an error because of same_exit
    # not being a slot in the `background` class. Therefore manual assignment 
    # done in this piece of code
    # .Object <- callNextMethod()

    .Object@shape <- shape 
    .Object@objects <- objects
    if (!all(sapply(.Object@objects, is, class2 = "object"))) {
        stop("All elements in slot 'objects' must be of type 'object'")
    }

    if(is.null(entrance)) {
        entrance <- rng_point(.Object@shape)
    }
    .Object@entrance <- entrance

    if(same_exit) {
        exit <- entrance
    } else if(is.null(exit)) {
        exit <- rng_point(.Object@shape)
    }
    .Object@exit <- exit
    .Object@entry_exit_width <- entry_exit_width

    return(.Object)
})

#' @export
setGeneric("shape", function(object) standardGeneric("shape"))

#' @export
setGeneric("shape<-", function(object, value) standardGeneric("shape<-"))

setMethod("shape", "background", function(object) {
    return(object@shape)
})

setMethod("shape<-", "background", function(object, value) {
    object@shape <- value
    return(object)
})

#' @export
setGeneric("objects", function(object) standardGeneric("objects"))

#' @export
setGeneric("objects<-", function(object, value) standardGeneric("objects<-"))

setMethod("objects", "background", function(object) {
    return(object@objects)
})

setMethod("objects<-", "background", function(object, value) {
    object@objects <- value
    return(object)
})

#' @export
setGeneric("entrance", function(object) standardGeneric("entrance"))

#' @export
setGeneric("entrance<-", function(object, value) standardGeneric("entrance<-"))

setMethod("entrance", "background", function(object) {
    return(object@entrance)
})

setMethod("entrance<-", "background", function(object, value) {
    object@entrance <- value
    return(object)
})

#' @export
setGeneric("exit", function(object) standardGeneric("exit"))

#' @export
setGeneric("exit<-", function(object, value) standardGeneric("exit<-"))

setMethod("exit", "background", function(object) {
    return(object@exit)
})

setMethod("exit<-", "background", function(object, value) {
    object@exit <- value
    return(object)
})