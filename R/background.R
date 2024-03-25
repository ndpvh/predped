#' An S4 Class to Represent the Background
#'
#' @slot shape An object of a type that extends \code{\link[predped]{object-class}}
#' defining the shape of the background.
#' @slot objects A list of objects of a type that extends
#' \code{\link[predped]{object-class}} defining the objects in the background.
#'
#' @export
background <- setClass("background", list(shape = "object", 
                                          objects = "list",
                                          entrance = "circle", 
                                          exit = "circle"))

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
        rng_pnt <- rng_point(.Object@shape)
        entrance <- circle(center = c(rng_pnt[1], rng_pnt[2]),
                           radius = 0.2)
    }
    .Object@entrance <- entrance

    if(same_exit) {
        exit <- entrance
    } else if(is.null(exit)) {
        rng_pnt <- rng_point(.Object@shape)
        exit <- circle(center = c(rng_pnt[1], rng_pnt[2]),
                       radius = 0.2)
    }
    .Object@exit <- exit

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