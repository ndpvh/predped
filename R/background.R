#' An S4 Class to Represent the Background
#'
#' @slot shape An object of a type that extends \code{\link[predped]{object-class}}
#' defining the shape of the background.
#' @slot objects A list of objects of a type that extends
#' \code{\link[predped]{object-class}} defining the objects in the background.
#' @slot entrance A coordinate specifying the location of the entrance.
#' @slot exit A coordinate specifying the location of the exit. Ignored if 
#' `same_exit = TRUE`.
#' 
#' @seealso \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{rectangle-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{circle-class}}
#' 
#' @rdname background-class
#' 
#' @export
background <- setClass("background", list(shape = "object", 
                                          objects = "list",
                                          entrance = "coordinate", 
                                          exit = "coordinate"))

setMethod("initialize", "background", function(.Object, 
                                               shape,
                                               objects = list(),
                                               entrance = NULL,
                                               exit = NULL,
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
    .Object@entrance <- coordinate(entrance)

    if(same_exit) {
        exit <- entrance
    } else if(is.null(exit)) {
        exit <- rng_point(.Object@shape)
    }
    .Object@exit <- coordinate(exit)

    return(.Object)
})

#' @rdname background-class
#' 
#' @export
setGeneric("shape", function(object) standardGeneric("shape"))

#' @rdname background-class
#' 
#' @export
setGeneric("shape<-", function(object, value) standardGeneric("shape<-"))

setMethod("shape", "background", function(object) {
    return(object@shape)
})

setMethod("shape<-", "background", function(object, value) {
    object@shape <- value
    return(object)
})

#' @rdname background-class
#' 
#' @export
setGeneric("objects", function(object) standardGeneric("objects"))

#' @rdname background-class
#' 
#' @export
setGeneric("objects<-", function(object, value) standardGeneric("objects<-"))

setMethod("objects", "background", function(object) {
    return(object@objects)
})

setMethod("objects<-", "background", function(object, value) {
    object@objects <- value
    return(object)
})

#' @rdname background-class
#' 
#' @export
setGeneric("entrance", function(object) standardGeneric("entrance"))

#' @rdname background-class
#' 
#' @export
setGeneric("entrance<-", function(object, value) standardGeneric("entrance<-"))

setMethod("entrance", "background", function(object) {
    return(object@entrance)
})

setMethod("entrance<-", "background", function(object, value) {
    object@entrance <- as(value, "coordinate")
    return(object)
})

#' @rdname background-class
#' 
#' @export
setGeneric("exit", function(object) standardGeneric("exit"))

#' @rdname background-class
#' 
#' @export
setGeneric("exit<-", function(object, value) standardGeneric("exit<-"))

setMethod("exit", "background", function(object) {
    return(object@exit)
})

setMethod("exit<-", "background", function(object, value) {
    object@exit <- as(value, "coordinate")
    return(object)
})