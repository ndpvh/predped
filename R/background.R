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

setMethod("limit_access", "background", function(object, x) {

    # Check whether there are any segments that might limit the access of agents
    # in the first place. If not, we can return the background directly
    possible_blockages <- limited_access(object)

    if(length(possible_blockages) == 0) {
        return(logical(0))
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
        # If we are looking at an agent, then we will just have one set of 
        # coordinates
        co <- matrix(center(x), nrow = 1)
        check <- \(y) intersects(y, x)

    } else {
        # If we are looking at a set of coordinates, then we want to allow for 
        # multiple coordinates to be related to a different set of objects that 
        # they might pass through. 
        #
        # Limits the looping that we have to do in the one use-case that we use 
        # coordinates in (being in `evaluate_edges`)
        if(is.null(ncol(x))) {
            co <- matrix(ncol = 2)
        } else {
            co <- as.matrix(x)
        }

        check <- \(y) in_object(y, co, outside = FALSE)
    }

    # Loop over all possible access limitations
    return(sapply(possible_blockages, 
                  function(y) {
                      # Compute the relative angle of the agent compared to the start of 
                      # the segment and subtract the orientation of the line.
                      angle <- atan2(co[,2] - from(y)[2],
                                     co[,1] - from(y)[1])
                      angle <- angle - orientation(y)

                      # Return a logical denoting whether this one blocks or not
                      return(!check(y) & sin(angle) < pi & sin(angle) > 0)
                  }))
})

#' Transform the limited_access segments to polygons
#' 
#' @param segment Object of the class `segment`
#' 
#' @export 
compute_limited_access <- function(segment) {
    # Get the coordinates of the segment
    coords <- points(segment)   
                   
    # Find out with which set of coordinates you can create a rectangle starting
    # from the coordinates of the segment itself. Done through rotation so that 
    # the rectangle itself is 1e-2 wide.
    #
    # Instead of object-class `rectangle`, we use `polygon` here as the typical
    # functions are a bit more efficient for this object.
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