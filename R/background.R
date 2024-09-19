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
                                          precomputed_limited_access = "list",
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

    # If they make it through, then we should create a precomputed limited_access
    # list. This will contain all the shapes that will be added to the `objects`
    # if they block the access for an agent.
    .Object@precomputed_limited_access <- lapply(limited_access, 
                                                 compute_limited_access)

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

#' Add the limited_access to objects
#' 
#' @rdname limit_access-method
#' 
#' @export 
setGeneric("limit_access", function(object, x, ...) standardGeneric("limit_access"))

setMethod("limit_access", "background", function(object, 
                                                 x,
                                                 return_list = FALSE) {

    # Check whether there are any segments that might limit the access of agents
    # in the first place. If not, we can return the background directly
    possible_blockages <- limited_access(object)

    if(length(possible_blockages) == 0) {
        if(return_list) {
            return(list())
        } else {
            return(object)
        }
    }

    # Create a checking function that will differ for agent vs coordinates. The 
    # idea of the check is that if the agent intersects the segment (or if a 
    # coordinate is contained in it), we need to delete the segment from the list, 
    # as we want it to be non-walkthrough only when you are fully at one side of 
    # the line.
    #
    # Creating this function is a bit silly, but it does help maintain everything
    # within this one `limit_access` function instead of creating two separate 
    # functions that do almost the same thing
    if(inherits(x, "object")) {
        check <- \(y) intersects(y, x)
        co <- center(x)
    } else {
        check <- \(y) in_object(y, x, outside = FALSE)
        co <- as.numeric(x)
    }

    # Loop over all possible access limitations
    idx <- sapply(possible_blockages, 
                  function(x) {
                      # Compute the relative angle of the agent compared to the start of 
                      # the segment and subtract the orientation of the line.
                      angle <- atan2(co[2] - from(x)[2],
                                     co[1] - from(x)[1])
                      angle <- angle - orientation(x)

                      # Return a logical denoting whether this one blocks or not
                      return(!check(x) & sin(angle) < pi & sin(angle) > 0)
                  })

    # If the user wants to return the list of none-accessible objects, provide 
    # it to them
    if(return_list) {
        return(object@precomputed_limited_access[idx])
    }

    # Once done, we can add these polygons to the objects in the background
    objects(object) <- append(objects(object), 
                              object@precomputed_limited_access[idx])
    return(object)
})

#' Transform the limited_access segments to polygons
#' 
#' @param segment Object of the class `segment`
#' 
#' @export 
compute_limited_access <- function(segment) {
    coords <- points(segment)   
                   
    alpha <- orientation(segment) + pi / 2
    R <- c(cos(alpha), sin(alpha), -sin(alpha), cos(alpha)) |>
       matrix(nrow = 2, ncol = 2)
    new_coords <- coords[2:1,] + rep(R %*% c(1e-2, 0), each = 2)

    return(polygon(points = rbind(coords, new_coords)))
}

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
    object@precomputed_limited_access <- lapply(value, 
                                                compute_limited_access)

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