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
                                          limited_access = "list",
                                          entrance = "matrix", 
                                          exit = "matrix"))

setMethod("initialize", "background", function(.Object, 
                                               shape,
                                               objects = list(),
                                               limited_access = list(),
                                               entrance = NULL,
                                               exit = NULL,
                                               same_exit = TRUE,
                                               ...) {
    # If uncommented, callNextMethod() will throw an error because of same_exit
    # not being a slot in the `background` class. Therefore manual assignment 
    # done in this piece of code
    .Object@shape <- shape 
    .Object@objects <- objects
    .Object@limited_access <- limited_access

    # Some checks on the objects
    if(!all(sapply(.Object@objects, is, class2 = "object"))) {
        stop("All elements in slot 'objects' must be of type 'object'")
    }

    if(any(sapply(.Object@objects, is, class2 = "segment"))) {
        stop(paste0("None of the elements in slot 'objects' can be of type `segment`. ",
                    "Please add these to the slot `limited_access`."))
    }

    if(!all(sapply(.Object@limited_access, is, class2 = "segment"))) {
        stop("All elements in slot 'limited_access' must be of type 'segment'")
    }

    # Entrances should either be provided or randomly generated. We furthermore
    # need to transform the entrance to a matrix, allowing for multiple entrances
    if(is.null(entrance)) {
        entrance <- rng_point(.Object@shape)
    }

    if(!is.matrix(entrance)) {
        entrance <- matrix(entrance, ncol = 2)
    }

    .Object@entrance <- entrance

    # Exits are handled the same as entrances, with the exception that we may 
    # also use the entrance as an exit whenever specified so.
    if(same_exit) {
        exit <- entrance
    } else if(is.null(exit)) {
        exit <- rng_point(.Object@shape)
    }

    if(!is.matrix(exit)) {
        exit <- matrix(exit, ncol = 2)
    }

    .Object@exit <- exit

    return(.Object)
})

#' Getter/Setter for the shape-slot
#' 
#' @rdname shape-method
#' 
#' @export
setGeneric("shape", function(object) standardGeneric("shape"))

#' @rdname shape-method
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

#' Getter/Setter for the objects-slot
#' 
#' @rdname objects-method
#' 
#' @export
setGeneric("objects", function(object) standardGeneric("objects"))

#' @rdname objects-method
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

#' Getter/Setter for the limited_access-slot
#' 
#' @rdname limited_access-method
#' 
#' @export
setGeneric("limited_access", function(object) standardGeneric("limited_access"))

#' @rdname limited_access-method
#' 
#' @export
setGeneric("limited_access<-", function(object, value) standardGeneric("limited_access<-"))

setMethod("limited_access", "background", function(object) {
    return(object@limited_access)
})

setMethod("limited_access<-", "background", function(object, value) {
    object@limited_access <- value
    return(object)
})

#' Getter/Setter for the entrance-slot
#' 
#' @rdname entrance-method
#' 
#' @export
setGeneric("entrance", function(object) standardGeneric("entrance"))

#' @rdname entrance-method
#' 
#' @export
setGeneric("entrance<-", function(object, value) standardGeneric("entrance<-"))

setMethod("entrance", "background", function(object) {
    return(object@entrance)
})

setMethod("entrance<-", "background", function(object, value) {
    if(!is.matrix(value)) {
        value <- matrix(value, ncol = 2)
    }

    object@entrance <- value
    return(object)
})

#' Getter/Setter for the exit-slot
#' 
#' @rdname exit-method
#' 
#' @export
setGeneric("exit", function(object) standardGeneric("exit"))

#' @rdname exit-method
#' 
#' @export
setGeneric("exit<-", function(object, value) standardGeneric("exit<-"))

setMethod("exit", "background", function(object) {
    return(object@exit)
})

setMethod("exit<-", "background", function(object, value) {
    if(!is.matrix(value)) {
        value <- matrix(value, ncol = 2)
    }

    object@exit <- value
    return(object)
})