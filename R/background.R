#' An S4 Class to Represent the Background
#'
#' Defines the \code{background} class, which contains all characteristics of 
#' the setting in which agents will be simulated to walk. Currently consists of 
#' the shape of the simulated room and objects that are contained within the 
#' room.
#' 
#' @slot shape Object of a type that extends \code{\link[predped]{object-class}}
#' defining the shape of the background. Importantly, the shape cannot be of 
#' the \code{\link[predped]{segment-class}}.
#' @slot objects List of objects of a type that extends
#' \code{\link[predped]{object-class}} defining the objects that are present in 
#' the simulated room (e.g., tables and cabinets). Importantly, objects cannot 
#' be of the \code{\link[predped]{segment-class}}.
#' @slot limited_access List of objects of the \code{\link[predped]{segment-class}}
#' which define the routes that can only be taken in one direction (see the 
#' documentation of \code{\link[predped]{segment-class}}).
#' @slot precomputed_segment_class List of non-penetrable objects based on the 
#' values of slot \code{limited_access}. Is not meant to be changed by the user,
#' but is used as a slot to significantly speed up computations.
#' @slot entrance Numeric matrix specifying the location(s) of the entrance(s), 
#' where the first column denotes the x-coordinate and the second column the 
#' y-coordinate.
#' @slot exit Numeric matrix specifying the location(s) of the exit(s), 
#' where the first column denotes the x-coordinate and the second column the 
#' y-coordinate.
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{entrance}},
#' \code{\link[predped]{exit}},
#' \code{\link[predped]{limited_access}},
#' \code{\link[predped]{objects}},
#' \code{\link[predped]{shape}},
#' \code{\link[predped]{initialize,background-method}},
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

#' Constructor for the \code{\link[predped]{background-class}}
#' 
#' @param shape Object of a type that extends \code{\link[predped]{object-class}}
#' defining the shape of the background. Importantly, the shape cannot be of 
#' the \code{\link[predped]{segment-class}}.
#' @param objects List of objects of a type that extends
#' \code{\link[predped]{object-class}} defining the objects that are present in 
#' the simulated room (e.g., tables and cabinets). Importantly, objects cannot 
#' be of the \code{\link[predped]{segment-class}}. Defaults to an empty list.
#' @param limited_access List of objects of the \code{\link[predped]{segment-class}}
#' which define the routes that can only be taken in one direction (see the 
#' documentation of \code{\link[predped]{segment-class}}). Defaults to an empty 
#' list.
#' @param entrance Numeric vector or matrix specifying the location(s) of the 
#' entrance(s), where the first value or column denotes the x-coordinate and the 
#' second value or column the y-coordinate. Defaults to NULL, letting the 
#' location of the entrance be randomly generated.
#' @param exit Numeric vector or matrix specifying the location(s) of the 
#' exit(s), where the first value or column denotes the x-coordinate and the 
#' second value or column the y-coordinate. Defaults to NULL, letting the 
#' location of the exit be randomly generated. However, if \code{same_exit = TRUE},
#' then the location(s) of the exit(s) will be the same as the location(s) of the
#' entrance(s).
#' @param same_exit Logical denoting whether the location(s) of the exit(s) 
#' should be the same as for the entrance(s). Defaults to \code{TRUE} if no 
#' exit is provided and to \code{FALSE} if an exit is provided.
#' 
#' @return Object of the \code{\link[predped]{background-class}}
#' 
#' @examples
#' # Initialize setting
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list())
#' 
#' # Access the two slots that were specified
#' my_background@shape
#' my_background@objects
#' 
#' @seealso 
#' \code{\link[predped]{background-class}},
#' \code{\link[predped]{entrance}},
#' \code{\link[predped]{exit}},
#' \code{\link[predped]{limited_access}},
#' \code{\link[predped]{objects}},
#' \code{\link[predped]{shape}}
#' 
#' @rdname initialize-background-method
#' 
#' @export
setMethod("initialize", "background", function(.Object, 
                                               shape,
                                               objects = list(),
                                               limited_access = list(),
                                               entrance = NULL,
                                               exit = NULL,
                                               same_exit = is.null(exit)) {

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

#' Show method for the \code{\link[predped]{background-class}}
#' 
#' @export
setMethod("show", "background", function(object) {
    cat(crayon::bold("Background Attributes"), "\n")
    cat("entrance:", "\n")
    print(object@entrance)
    cat("\nexit:", object@exit, "\n")
    print(object@exit)
    cat("\nlimited_access (number):", length(object@limited_access), "\n")
    cat("objects (number):", length(object@objects), "\n")
    cat("shape: (a) position:", object@shape@center, "(b) size:", size(object@shape), "\n")    
    cat("\nFor more detailed information, please extract the wanted information from the background directly.\n")
})

#' Compute which segments can be crossed
#' 
#' Method that determines whether one can cross the segments that are 
#' contained within the \code{limited_access} slot of the 
#' \code{\link[predped]{background-class}}. Determines this ability from the 
#' standpoint of \code{x}, which can either be a matrix of coordinates or an 
#' instance of the \code{\link[predped]{agent-class}}.
#'
#' @details
#' This method takes in an object of the \code{\link[predped]{background-class}}
#' and checks whether one can cross the \code{segment}s in the \code{limited_access}
#' slot from the standpoint taken in \code{x} (if \code{x} is a matrix) or from 
#' the position of \code{x} (if \code{x} is an \code{agent}).
#' 
#' Returns a Logical vector or matrix denoting which of the segments in the 
#' \code{limited_access} of the \code{background} object can be passed through 
#' from the standpoint of the the coordinates in \code{x} (\code{TRUE} means 
#' that the segment cannot be passed through while \code{FALSE} means that it 
#' can be passed through). With multiple coordinates and multiple segments, 
#' logicals are ordered with coordinates in the rows and segments in the columns.
#' 
#' If \code{limited_access} is an empty list, it returns an empty logical vector.
#' If a coordinate or agent is currently intersecting a segment, they are not 
#' blocked (i.e., then they receive the logical value \code{FALSE}).
#' 
#' @param object Object of the \code{\link[predped]{background-class}}.
#' @param x Either a numeric vector or matrix containing x- and y-coordinates, 
#' or an object of the \code{\link[predped]{agent-class}}
#' 
#' @return Logical vector or matrix denoting whether \code{segment}s can be 
#' passed through or not.
#' 
#' @examples 
#' # Create a setting where you can only cross the diagonal of the space if you
#' # are in the lower-right part of the space
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(), 
#'                             limited_access = list(segment(from = c(-1, -1), 
#'                                                           to = c(1, 1))))
#' 
#' # Create two agents of whom the access may be limited or not
#' agent_not_limited <- agent(center = c(0.75, -0.75), radius = 0.25)
#' agent_limited <- agent(center = c(-0.75, 0.75), radius = 0.25)
#' 
#' limit_access(my_background, agent_not_limited)
#' limit_access(my_background, agent_limited)
#' 
#' # Create a matrix of coordinates of which the first is not limited and the 
#' # second is
#' coords <- rbind(c(0.75, -0.75), c(-0.75, 0.75))
#' limit_access(my_background, coords)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}
#' \code{\link[predped]{background-class}}
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

        check <- \(y) in_object(y, co)
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

#' Transform \code{segment}s to \code{polygon}s
#' 
#' This function is used to transform an object of the 
#' \code{\link[predped]{segment-class}} to an object of the 
#' \code{\link[predped]{polygon-class}}. Done as a precomputation in the 
#' constructor for the \code{\link[predped]{background-class}} (filling the 
#' \code{precomputed_limited_access} slot based on the \code{limited_access}
#' slot.
#' 
#' @param segment Object of the \code{\link[predped]{segment-class}}.
#' 
#' @return Object of the \code{\link[predped]{polygon-class}}.
#' 
#' @seealso 
#' \code{\link[predped]{background-class}}
#' \code{\link[predped]{polygon-class}}
#' \code{\link[predped]{segment-class}}
#'
#' @rdname compute_limited_access
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





################################################################################
# GETTERS AND SETTERS

#' @rdname entrance-method
setMethod("entrance", "background", function(object) {
    return(object@entrance)
})

#' @rdname entrance-method
setMethod("entrance<-", "background", function(object, value) {
    if(!is.matrix(value)) {
        value <- matrix(value, ncol = 2)
    }

    object@entrance <- value
    return(object)
})



#' @rdname exit-method
setMethod("exit", "background", function(object) {
    return(object@exit)
})

#' @rdname exit-method
setMethod("exit<-", "background", function(object, value) {
    if(!is.matrix(value)) {
        value <- matrix(value, ncol = 2)
    }

    object@exit <- value
    return(object)
})



#' @rdname limited_access-method
setMethod("limited_access", "background", function(object) {
    return(object@limited_access)
})

#' @rdname limited_access-method
setMethod("limited_access<-", "background", function(object, value) {
    object@limited_access <- value
    object@precomputed_limited_access <- lapply(value, 
                                                compute_limited_access)

    return(object)
})



#' @rdname objects-method
setMethod("objects", "background", function(object) {
    return(object@objects)
})

setMethod("objects<-", "background", function(object, value) {
    object@objects <- value
    return(object)
})



#' @rdname shape-method
setMethod("shape", "background", function(object) {
    return(object@shape)
})

#' @rdname shape-method
setMethod("shape<-", "background", function(object, value) {
    object@shape <- value
    return(object)
})