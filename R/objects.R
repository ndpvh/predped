################################################################################
# S4 CLASSES AND INITIALIZATION

#' An S4 Class to Represent Coordinate Vectors
#'
#' Defines the \code{coordinate} class, which contains all characteristics of a
#' coordinate or a single point. WARNING: Niels wants to phase the use of 
#' \code{coordinate}s out of \code{predped}, as they greatly hamper the 
#' flexibility of the low-level functions.
#'
#' @slot x Numerical vector denoting the coordinate.
#' 
#' @seealso 
#' \code{\link[predped]{rotate}},
#' \code{\link[predped]{initialize,coordinate-method}}
#' 
#' @rdname coordinate-class
#'
#' @export
coordinate <- setClass("coordinate", contains = "numeric")

#' Constructor for the \code{\link[predped]{coordinate-class}}
#' 
#' @param x Numerical vector of size 2 denoting the coordinate.
#' 
#' @return Object of the \code{\link[predped]{coordinate-class}}
#' 
#' @examples
#' # Initialize a coordinate
#' my_coordinate <- coordinate(c(1, 1))
#' 
#' # Access the numeric vector again
#' my_coordinate@.Data
#' 
#' @seealso 
#' \code{\link[predped]{coordinate-class}},
#' \code{\link[predped]{rotate}}
#' 
#' @rdname initialize-coordinate-method
#' 
#' @export
setMethod("initialize", "coordinate", function(.Object, ...) {

    .Object <- callNextMethod()

    # Perform check and add names
    if(length(.Object) != 2) {
        stop("Coordinate vector must have length two (x and y)")
    }
    names(.Object) <- c("x", "y")

    return(.Object)
})

#' An S4 Abstract Base Class to Represent Objects
#'
#' @slot id Character denoting the name of the object.
#' @slot moveable Logical denoting whether the object can be moved from its
#' position.
#' @slot interactable Logical indicating whether the object can be interacted
#' with.
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}}
#' 
#' @rdname object-class
#' @family objects 
#'
#' @export
setClass("object", 
        list(id = "character",
             moveable = "logical",
             interactable = "logical"),
         contains = "VIRTUAL")

#' Constructor for the \code{\link[predped]{object-class}}
#' 
#' @param id Character denoting the identifier of the object Defaults to an 
#' empty string, triggering the creation of a random identifier.
#' @param moveable Logical denoting whether the position of the object can be 
#' changed. Defaults to \code{FALSE}.
#' @param interactable Logical denoting whether the object can be interacted 
#' with. Defaults to \code{TRUE}.
#' 
#' @return Object of the \code{\link[predped]{object-class}}
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{object-class}},
#' \code{\link[predped]{polygon-class}},
#' \code{\link[predped]{rectangle-class}}
#' 
#' @rdname initialize-object-method
#' 
#' @export
setMethod("initialize", "object", function(.Object, 
                                            id = character(0),
                                            moveable = FALSE, 
                                            interactable = TRUE,
                                            ...) {

    .Object <- callNextMethod(.Object, ...)

    # If no id is provided to the object, create a random one
    if(length(id) == 0) {
        id <- paste("object", 
                    paste0(sample(letters, 5, replace = TRUE), 
                           collapse = ""))
    }

    # Adjust the unique slots to the object class
    .Object@id <- id
    .Object@moveable <- moveable
    .Object@interactable <- interactable 

    return(.Object)
})

#' An S4 class to Represent Polygon Objects
#'
#' Polygons can be used to create flexible shapes and are defined through a set
#' of points. The last point is automatically connected with the first point.
#'
#' @slot points Numerical matrix with two columns containing the x- and y-
#' coordinates of the points that define the polygon.
#' @slot center Numerical vector containing the position of the center of the 
#' polygon. For now, the mean of all coordinates in \code{points} used as an 
#' indication of the center of the polygon (as the center of a polygon is not 
#' easily defined).
#' @slot clock_wise Logical indicating whether the points define the polygon 
#' in a clockwise (\code{TRUE}) or counter-clockwise fashion (\code{FALSE}).
#' @slot forbidden Numerical vector containing the indices of those edges that
#' cannot be used to generate goals on.
#' @slot ... Slots shared with the \code{\link[predped]{object-class}}.
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{initialize,polygon-method}}
#'
#' @rdname polygon-class
#' @family objects
#' 
#' @export
polygon <- setClass("polygon", 
                    list(points = "matrix", 
                         center = "numeric",
                         clock_wise = "logical",
                         forbidden = "numeric"), 
                    contains = "object")

#' Constructor for the \code{\link[predped]{polygon-class}}
#' 
#' @param points Numerical matrix containing the coordinates that make up the 
#' polygon.
#' @param clock_wise Logical denoting whether the coordinates in \code{points} 
#' are defined in clockwise (\code{TRUE}) or counter-clockwise fashion 
#' (\code{FALSE}). Defaults to \code{TRUE}.
#' @param forbidden Numerical vector containing the indices of those edges that
#' cannot be used to generate goals on. Note that edges are created based on 
#' the \code{\link[predped]{points}} of the object. Defaults to an empty 
#' vector, making all edges worthy of goal generation.
#' @param ... Additional arguments passed to 
#' \code{\link[predped]{initialize,object-method}}.
#' 
#' @return Object of the \code{\link[predped]{polygon-class}}
#' 
#' @examples
#' # Initialize a polygon
#' my_polygon <- polygon(id = "my polygon", 
#'                       points = cbind(c(1, 1, -1, -1), 
#'                                      c(1, -1, -1, 1)), 
#'                       clock_wise = TRUE)
#' 
#' # Access the slots that you specified
#' my_polygon@points 
#' my_polygon@id
#' 
#' @seealso 
#' \code{\link[predped]{object-class}}
#' \code{\link[predped]{polygon-class}}
#' \code{\link[predped]{initialize,object-method}}
#' 
#' @rdname initialize-polygon-method
#' 
#' @export
setMethod("initialize", "polygon", function(.Object, 
                                            clock_wise = TRUE, 
                                            forbidden = numeric(0),
                                            ...) {
    
    # Create the polygon as defined in VIRTUAL and in the object-class
    .Object <- callNextMethod(.Object, ...)

    # Do a check of whether the points are accurately defined.
    if(ncol(.Object@points) != 2) {
        stop("All points must have an x- and y-coordinate (two-column matrix).")
    }

    # Add the unique slots of the polygon to the equation
    .Object@clock_wise <- clock_wise
    .Object@forbidden <- forbidden
    .Object@center <- coordinate(c(mean(range(.Object@points[,1])), 
                                   mean(range(.Object@points[,2]))))

    # Check whether all of the objects edges are forbidden or not. If so, then 
    # we need to make the object uninteractable. Only needed if the object is 
    # interactable in the first place
    if(.Object@interactable) {
        .Object@interactable <- length(.Object@forbidden) < nrow(.Object@points)
    }    

    return(.Object)
})

#' An S4 Class to Represent Rectangle Objects
#'
#' Special case of the \code{\link[predped]{polygon-class}}, defined through its 
#' four sides and right angles.
#'
#' @slot center Numeric vector denoting the coordinates of the center or 
#' position of the rectangle.
#' @slot size Numeric vector denoting the width and height of the rectangle.
#' @slot orientation Numeric indicating the orientation of the rectangle in 
#' radians.
#' @slot ... Slots shared with the \code{\link[predped]{object-class}} and the 
#' \code{\link[predped]{polygon-class}}.
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{polygon-class}}
#' 
#' @rdname rectangle-class
#' @family objects
#'
#' @export
#
# TO DO
#   - Currently, orientation is in radians. Might need to change to degrees, as 
#     everything else is in degrees
rectangle <- setClass("rectangle", 
                      list(center = "numeric",
                           size = "numeric",
                           orientation = "numeric"),
                      contains = c("polygon"))

#' Constructor for the \code{\link[predped]{rectangle-class}}
#' 
#' @param center Numeric vector denoting the coordinates of the center or 
#' position of the rectangle.
#' @param size Numeric vector denoting the width and height of the rectangle.
#' @param orientation Numeric denoting the orientation of the rectangle in 
#' radians. Defauls to \code{0}.
#' @param ... Additional arguments passed to 
#' \code{\link[predped]{initialize,object-method}}. Note that if the 
#' \code{points} or \code{clock_wise} arguments of the 
#' \code{\link[predped]{initialize,polygon-method}} are provided, that they will
#' not be used in the creation of the rectangle.
#' 
#' @return Object of the \code{\link[predped]{rectangle-class}}
#' 
#' @examples
#' # Initialize a rectangle
#' my_rectangle <- rectangle(id = "my rectangle", 
#'                           center = c(0, 0), 
#'                           size = c(2, 2))
#' 
#' # Access slots that are inherited from object, polygon, and rectangle
#' my_rectangle@size
#' my_rectangle@points 
#' my_rectangle@id
#' 
#' @seealso 
#' \code{\link[predped]{object-class}},
#' \code{\link[predped]{polygon-class}},
#' \code{\link[predped]{initialize,object-method}}
#' \code{\link[predped]{initialize,polygon-method}}
#' 
#' @rdname initialize-rectangle-method
#' 
#' @export
# 
# TO DO
#   - Figure out whether you would like there to be non-clockwise points to be 
#     possible in rectangles as well (as this is a functionality that is 
#     currently not supported)
#   - Figure out whether predped is robust against non-clockwise polygons or 
#     rectangles (my intuition says not, and might be a simple fix)
setMethod("initialize", "rectangle", function(.Object,
                                              center,
                                              size,
                                              orientation = 0,
                                            #   degrees = FALSE,
                                              ...) {

    # Do some initial checks
    if(length(center) != 2) {
        stop("Center position should have length two (x and y)")
    }
    
    if(length(size) != 2) {
        stop("Size vector must have length two (x and y)")
    } 

    if(any(size <= 0)) {
        stop("Size vector must be positive")
    }

    if(length(orientation) != 1) {
        stop("Orientation must be a single element")
    }

    # Create the points argument in clock_wise fashion
    lower <- center - size / 2
    upper <- center + size / 2

    points <- as.matrix(cbind(c(lower[1], lower[1], upper[1], upper[1]), 
                              c(lower[2], upper[2], upper[2], lower[2])))

    # if(degrees) {
    #     orientation <- orientation * pi / 180
    # }

    # If the rectangle has a non-zero orietation, then we need to rotate these 
    # points around the center of the rectangle
    if (orientation != 0) {
        points <- rotate(points, radians = orientation, center = center)
    }

    # if(!clock_wise) {
    #     points <- points[4:1,]
    # }

    # Pass the necessary information to polygon
    .Object <- callNextMethod(.Object, 
                              points = points, 
                              clock_wise = TRUE, 
                              ...)

    # Also add the rectangle-specific arguments to the object
    .Object@center <- as(center, "coordinate")
    .Object@size <- size
    .Object@orientation <- orientation

    return(.Object)
})

#' An S4 Class to Represent Circle Objects
#'
#' Special case of the \code{\link[predped]{object-class}} defined through its 
#' center and radius. 
#' 
#' @slot center Numeric vector denoting the center or position of the circle.
#' @slot radius Numeric denoting the radius of the circle.
#' @slot forbidden Numerical matrix containing the angles for which you cannot 
#' generate goals (in radians). 
#' @slot ... Slots shared with \code{\link[predped]{object-class}}.
#'
#' @seealso 
#' \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{initialize,circle-method}}
#' 
#' @rdname circle-class
#' @family objects
#' 
#' @export
circle <- setClass("circle", 
                   list(center = "numeric", 
                        radius = "numeric", 
                        forbidden = "matrix"), 
                   contains = c("object"))

#' Constructor for the \code{\link[predped]{circle-class}}
#' 
#' @param center Numeric vector denoting the coordinates of the center or 
#' position of the circle
#' @param radius Numeric denoting the radius of the circle.
#' @param forbidden Numerical matrix containing the angles for which you cannot 
#' generate goals (in radians). These angles are computed in the Euclidian space
#' and should be contained within the interval 0 and 2 * pi. Defaults to an 
#' empty matrix, making all angles worthy of goal generation.
#' @param ... Additional arguments passed to 
#' \code{\link[predped]{initialize,object-method}}.
#' 
#' @return Object of the \code{\link[predped]{circle-class}}
#' 
#' @examples
#' # Initialize a circle
#' my_circle <- circle(id = "my circle", 
#'                     center = c(0, 0), 
#'                     size = c(2, 2))
#' 
#' # Access slots that are inherited from object and circle
#' my_circle@size
#' my_circle@points 
#' my_circle@id
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{object-class}},
#' \code{\link[predped]{initialize,object-method}}
#' 
#' @rdname initialize-circle-method
#' 
#' @export
setMethod("initialize", "circle", function(.Object, 
                                           forbidden = matrix(nrow = 0, ncol = 0),
                                           ...) {

    # Pass inherited arguments to the VIRTUAL and object class
    .Object <- callNextMethod(.Object, ...)

    # Transform the center to a coordinate
    .Object@center <- as(.Object@center, "coordinate")
    
    # Perform a check on the radius
    if(length(.Object@radius) != 1) {
        stop("Slot 'radius' should be a single numeric value")
    }

    # Other things to add
    .Object@forbidden <- forbidden

    # Check whether all of the objects edges are forbidden or not. If so, then 
    # we need to make the object uninteractable. Only needed if the object is 
    # interactable in the first place
    if(.Object@interactable & length(forbidden) > 0) {
        forbidden_angles <- sapply(
            seq_len(nrow(forbidden)),
            \(i) seq(0, 2 * pi, 0.01) >= forbidden[i, 1] & seq(0, 2 * pi, 0.01) <= forbidden[i, 2]
        )

        if(ncol(forbidden_angles) > 1) {
            forbidden_angles <- rowSums(forbidden_angles) > 0
        } else {
            forbidden_angles <- as.logical(forbidden_angles)
        }

        .Object@interactable <- !all(forbidden_angles)
    }

    return(.Object)
})

#' An S4 class to Represent Lines
#'
#' Lines are used to determine intersections and to prevent pedestrians from 
#' walking certain ways.
#' 
#' @details 
#' It is important to realize exactly how it is determined whether agents can 
#' move through a segment or not before constructing or using these segments in 
#' your environment. For this, consider the following logic.
#' 
#' Consider the \code{from} slot to be the center of a circle. Furthermore 
#' consider the radius of this circle to be equal to the \code{size} slot, and 
#' the \code{orientation} of the segment to be determined by the positions of 
#' the \code{from} and \code{to} slots, so that we look at the orientation of 
#' the location of \code{to} relative to the location of \code{from}. This is 
#' how the \code{\link[predped]{segment-class}} is constructed, and is an 
#' important starting point for determining when pedestrians can and cannot 
#' move through a segment.
#' 
#' Consider another point in the Euclidian space, so that it has a position 
#' P = (x, y). Now we compute the relative angle of this coordinate within the 
#' circle that is defined by the \code{from} and \code{to} slots. In other words, 
#' we compute the absolute angle of coordinate P relative to \code{from} -- 
#' let's call this \eqn{\alpha} -- and then subtract the orientation of the line 
#' from this to get a relative angle within the circle defined by the 
#' segment -- let's call the result \eqn{\beta = \alpha - orientation}. 
#' We then block an agent whenever \eqn{\beta \in (0, pi)}, and let him pass 
#' through whenever \eqn{\beta \in [pi, 2 * pi]}.
#' 
#' To gain some intuition for this: If you create a segment that is vertical, 
#' where \code{from} has a lower y-coordinate than \code{to} (e.g., 
#' \code{from = c(0, 0)} and \code{to = c(0, 1)}), then agents will be blocked
#' whenever they find themselves to the left of this segment, while 
#' they will be able to pass through the segment whenever they are to the 
#' right of this segment. 
#'
#' @slot from Numeric vector denoting where the segment begins.
#' @slot to Numeric vector denoting where the segment ends.
#' @slot center Numeric vector denoting the center of the segment.
#' @slot size Numeric denoting the length of the segment.
#' @slot orientation Numeric denoting the orientation of the segment in radians.
#' @slot ... Slots that are inherited from \code{\link[predped]{object-class}}
#' 
#' @seealso 
#' \code{\link[predped]{object-class}},
#' \code{\link[predped]{initialize,segment-method}},
#' \code{\link[predped]{limit_access}}
#' 
#' @rdname segment-class
#'
#' @export
segment <- setClass("segment", 
                    list(from = "numeric", 
                         to = "numeric", 
                         center = "numeric",
                         size = "numeric",
                         orientation = "numeric"), 
                    contains = "object")

#' Constructor for the \code{\link[predped]{segment-class}}
#' 
#' @param from Numeric vector denoting the coordinates of the where the segment 
#' begins.
#' @param to Numeric vector denoting the coordinates of the where the segment 
#' ends.
#' @param ... Additional arguments passed to 
#' \code{\link[predped]{initialize,object-method}}.
#' 
#' @return Object of the \code{\link[predped]{segment-class}}
#' 
#' @examples
#' # Initialize a segment
#' my_segment <- segment(id = "my segment", 
#'                       from = c(0, 0), 
#'                       to = c(0, 1))
#' 
#' # Access slots that are inherited from object and segment
#' my_segment@from
#' my_segment@size
#' my_segment@id
#' 
#' @seealso 
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{object-class}},
#' \code{\link[predped]{initialize,object-method}}
#' 
#' @rdname initialize-segment-method
#' 
#' @export
setMethod("initialize", "segment", function(.Object, 
                                            from, 
                                            to, 
                                            ...) {

    # Pass all object-arguments to object-class
    .Object <- callNextMethod(.Object, ...)

    # Add all segment-specific slots to the object
    .Object@from <- from
    .Object@to <- to

    .Object@center <- from + 0.5 * (to - from)
    .Object@orientation <- atan2(to[2] - from[2], to[1] - from[1])
    .Object@size <- sqrt(sum((from - to)^2))

    return(.Object)
})





################################################################################
# METHODS

#' Rotate object around center
#' 
#' Retrieve an object that is rotated around its center with \code{degrees} or 
#' \code{radians}. Currently works for \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{coordinate-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}}, and
#' \code{\link[predped]{segment-class}} We additionally provide this 
#' method for R-native numeric vectors and matrices as well.
#'
#' @param object Object of \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{coordinate-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{rectangle-class}}, or, alternatively, a numeric vector 
#' or matrix containing coordinates.
#' @param radians Numeric denoting the degrees with which to rotate the object 
#' in radians. Defaults to \code{0}.
#' @param degrees Numeric denoting the degrees with which to rotate the object.
#' Defaults to \code{NULL}, triggering the use of \code{radians} instead. Whenever 
#' \code{degrees} is not \code{NULL}, \code{radians} will be ignored.
#' @param center Numeric vector denoting the x and y coordinates around which 
#' to rotate the object. Defaults differ for each object. For instances of the
#' \code{\link[predped]{coordinate-class}}, numerics, and matrices, this defaults 
#' to the origin (0, 0). For the other objects defined under 
#' \code{\link[predped]{object-class}}, this defaults to their own centers.
#' 
#' @return Object of the same class as the one provided
#' 
#' @examples 
#' # Let's create a numeric vector and degrees of rotation
#' x <- c(1, 1)
#' 
#' # Let's rotate this numeric 90 degrees around the origin
#' rotate(x, degrees = 90, center = c(0, 0))
#' 
#' # Let's create a numeric matrix and degrees of rotation
#' x <- cbind(c(1, 1, -1, -1), 
#'            c(1, -1, -1, 1))
#' x
#' 
#' # Let's rotate this matrix 90 degrees around the origin
#' rotate(x, degrees = 90, center = c(0, 0))
#' 
#' @seealso
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{coordinate-class}}, 
#' \code{\link[predped]{object-class}},
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}}
#' 
#' @docType method
#' 
#' @rdname rotate-method
#' 
#' @export
# 
# TO DO:
#   - Phase out coordinates
#   - Create tests for the rotates for the other objects. Currently not provided.
setGeneric("rotate",
           function(object, ...) standardGeneric("rotate"),
           signature = "object")

#' @rdname rotate-method
setMethod("rotate", signature(object = "numeric"), function(object, 
                                                            radians = 0, 
                                                            degrees = NULL,
                                                            center = c(0, 0)) {
    # Do some initial checks of both the numeric and the center
    if(length(object) != 2 | length(center) != 2) {
        stop("The provided coordinate or center does not have length 2.")
    }

    # If necessary, transform the degrees to radians
    if(!is.null(degrees)) {
        if(length(degrees) > 1) {
            stop("More than one degree provided.")
        }

        radians <- degrees * pi / 180
    }

    # Perform a check of the radians
    if(length(radians) > 1) {
        stop("More than one radians provided.")
    }

    # Step 1: Transform coordinates so that the center around which to rotate 
    # becomes the origin
    x <- object - center

    # Step 2: Create the rotation matrix R and rotate the point around the 
    # origin
    R <- rbind(c(cos(radians), -sin(radians)), 
               c(sin(radians), cos(radians)))
    y <- R %*% x

    # Step 3: Tranform the rotated coordinates to their original location and 
    # return as a coordinate
    y <- y + center

    return(coordinate(y))
})

# Make sure coordinate is handled in the same way as numerics for this method
setAs("numeric", "coordinate", function(from) new("coordinate", from))

#' @rdname rotate-method
setMethod("rotate", signature(object = "matrix"), function(object, 
                                                           radians = 0,
                                                           degrees = NULL,
                                                           center = c(0, 0)) {

    # Check whether the matrix has the correct dimensionality
    if((nrow(object) != 2) & (ncol(object) != 2)) {
        stop("Point matrix should have x- and y-coordinates: Neither rows nor columns have two values.")
    }

    # If necessary, transform the degrees to radians
    if(!is.null(degrees)) {
        if(length(degrees) != 1) {
            stop("More than one degree provided.")
        }

        radians <- degrees * pi / 180
    }

    # Perform a check of the radians
    if(length(radians) > 1) {
        stop("More than one radians provided.")
    }

    # Step 1: Transpose the matrix if necessary. For our rotation to work, we 
    # need the matrix to have dimensionality (2, N)
    transpose <- nrow(object) != 2
    if(transpose) {
        object <- t(object)
    }
    
    # Step 2: Transform coordinates so that the center around which to rotate 
    # becomes the origin. Important, we use some implicit R in here, where R 
    # automatically replicates the center and subtracts it column-wise from 
    # the object. This will only work if object has the correct dimensionality 
    # (2, N)
    x <- object - center 

    # Step 3: Create the rotation matrix R and rotate the point around the 
    # origin
    R <- rbind(c(cos(radians), -sin(radians)), 
               c(sin(radians), cos(radians)))
    y <- R %*% x

    # Step 4: Tranform the rotated coordinates to their original location and 
    # return as a coordinate
    y <- y + center

    # Step 5: If the matrix was originally transposed, we should tranpose it 
    # again to match the original dimensionality of the input.
    if(transpose) {
        y <- t(y)
    }

    return(y)
})

#' @rdname rotate-method
setMethod("rotate", signature(object = "polygon"), function(object, 
                                                            center = object@center,
                                                            ...) {
    # Change the points, as this is the only slot affected by the rotation. For 
    # the points, we rely on the rotation of a matrix, which eases our work
    object@points <- rotate(object@points,
                            center = center, 
                            ...)

    return(object)
})

#' @rdname rotate-method
setMethod("rotate", signature(object = "rectangle"), function(object, 
                                                              radians = 0, 
                                                              degrees = NULL,
                                                              center = object@center) {
    # If necessary, transform the degrees to radians
    if(!is.null(degrees)) {
        if(length(degrees) != 1) {
            stop("More than one degree provided.")
        }

        radians <- degrees * pi / 180
    }

    # Change the rectangles orientation and points, as these are the slots 
    # affected by the rotation. For the points, we rely on the rotation of a 
    # matrix, which eases our work
    object@orientation <- radians
    object@points <- rotate(object@points, 
                            radians = radians,
                            center = center)

    return(object)
})

#' @rdname rotate-method
setMethod("rotate", signature(object = "circle"), function(object, 
                                                           center = object@center,
                                                           ...) {

    # If the center is equal to the circle's center, we don't need to rotate as 
    # the result will be the same
    if(object@center == center) {
        return(object)
    }

    # Otherwise, we will rotate the center of the circle around the point 
    # specified. For this, we rely on the rotation of a numeric.
    object@center <- rotate(object@center, 
                            center = center, 
                            ...)

    return(object)
})

#' @rdname rotate-method
setMethod("rotate", signature(object = "segment"), function(object, 
                                                            center = object@center,
                                                            ...) {

    # Rotate the points of the segment around the point specified. For this, we 
    # rely on the rotation of a matrix, Note that the matrix should be 2 x N 
    # for the rotation in matrix to work, so that we have to put the coordinates
    # in the columns, not rows. 
    result <- rotate(cbind(object@from, object@to), 
                     center = center, 
                     ...)
    object@from <- result[,1]
    object@to <- result[,2]

    # Adjust the center and orientation as well, as these might have changed
    object@center <- object@from + (object@to - object@from) / 2
    object@orientation <- atan2(object@to[2] - object@from[2], object@to[1] - object@from[1])

    return(object)
})



#' Move object to new location
#' 
#' Move an object to a new coordinate. Currently works for 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{coordinate-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}}, and
#' \code{\link[predped]{segment-class}}.
#'
#' @param object Object of \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{coordinate-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{rectangle-class}}, or, 
#' \code{\link[predped]{segment-class}}.
#' @param coord Numeric denoting the location to which the object should be 
#' moved.
#' 
#' @return Object of the same class as the one provided.
#' 
#' @examples 
#' # Let's create an object
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' my_circle@center 
#' 
#' # Move the object to another location
#' moved_circle <- move(my_circle, coord = c(1, 1))
#' moved_circle
#' 
#' # Let's also do the same for a rectangle, showing in the process that moving
#' # a rectangle also changes its points
#' my_rectangle <- rectangle(center = c(0, 0), size = c(2, 2))
#' my_rectangle@points 
#' 
#' moved_rectangle <- move(my_rectangle, coord = c(1, 1))
#' moved_rectangle@points
#' 
#' @seealso
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{coordinate-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{center<-}},
#' \code{\link[predped]{position<-}}
#' 
#' @docType method
#' 
#' @rdname move-method
#' 
#' @export
setGeneric("move", function(object, ...) standardGeneric("move"))

#' @rdname move-method
setMethod("move", signature(object = "polygon"), function(object, 
                                                          coord) {

    # Check whether the object can be moved. If not, then we cannot change its 
    # position and have to return the same object
    if(!object@moveable) {
        warning("Object is not moveable. Keeping position unchanged.")
        return(object)
    }

    # Moving a polygon comes down to changing its points so that (a) they are 
    # centered around the origin (-center) and (b) adding the new coordinate 
    # to it. Additionally, change the center argument.
    x <- object@center

    object@points <- cbind(object@points[,1] - x[1] + coord[1], 
                           object@points[,2] - x[2] + coord[2])
    object@center <- coord

    return(object)
})

#' @rdname move-method
setMethod("move", signature(object = "rectangle"), function(object, 
                                                            coord) {
    
    # Check whether the object can be moved. If not, then we cannot change its 
    # position and have to return the same object
    if(!object@moveable) {
        warning("Object is not moveable. Keeping position unchanged.")
        return(object)
    }

    # Moving a rectangle uses the same logic as for polygons.
    x <- object@center

    object@points <- cbind(object@points[,1] - x[1] + coord[1], 
                           object@points[,2] - x[2] + coord[2])
    object@center <- coord

    return(object)
})

#' @rdname move-method
setMethod("move", signature(object = "circle"), function(object, 
                                                         coord) {
    
    # Check whether the object can be moved. If not, then we cannot change its 
    # position and have to return the same object
    if(!object@moveable) {
        warning("Object is not moveable. Keeping position unchanged.")
        return(object)
    }
    
    # Moving a circle comes down to just changing its center.
    object@center <- coord

    return(object)
})

#' @rdname move-method
setMethod("move", signature(object = "segment"), function(object, 
                                                          coord) {

    # Check whether the object can be moved. If not, then we cannot change its 
    # position and have to return the same object
    if(!object@moveable) {
        warning("Object is not moveable. Keeping position unchanged.")
        return(object)
    }
    
    # Moving a segment comes down to changing its coordinates and changing its 
    # center.
    diff <- coord - object@center

    object@from <- object@from + diff 
    object@to <- object@to + diff
    object@center <- coord

    return(object)
})



#' Calculate the Area of an Object
#' 
#' Currently defined for \code{\link[predped]{circle-class}} and 
#' \code{\link[predped]{rectangle-class}}.
#'
#' @param object Object of \code{\link[predped]{object-class}}.
#'
#' @return Numeric denoting the area of the object.
#' 
#' @examples 
#' # Create a circle
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' 
#' # Compute the area
#' area(my_circle)
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{objects-class}},
#' \code{\link[predped]{rectangle-class}}
#' 
#' @docType method
#' 
#' @rdname area-method
#' 
#' @export
setGeneric("area", function(object) standardGeneric("area"))

#' @rdname area-method
setMethod("area", signature(object = "rectangle"), function(object) prod(object@size))

#' @rdname area-method
setMethod("area", signature(object = "circle"), function(object) pi * object@radius^2)



#' Check Whether a Point Lies Within an Object
#' 
#' Currently works for all classes inside of the \code{\link[predped]{object-class}}.
#'
#' @param object Object of the \code{\link[predped]{object-class}}.
#' @param x Numeric vector or matrix containing x- and y-coordinates to be 
#' checked.
#' @param cpp Logical denoting whether to use the Rcpp alternative (\code{TRUE})
#' or the R alternative of this function (\code{FALSE}). Defaults to \code{FALSE}.
#'
#' @return Logical whether the point is inside of the object (\code{TRUE}) or 
#' outside of the object (\code{FALSE}).
#' 
#' @examples 
#' # Let's create an object
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' 
#' # Let's create a matrix of different coordinates of which the first is 
#' # inside of the object, the second on its circumference, and the third  
#' # outside of the object
#' coords <- rbind(c(0, 0), 
#'                 c(1, 0), 
#'                 c(2, 0))
#' 
#' # Let's do the test
#' in_object(my_circle, coords)
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{out_object}},
#' \code{\link[predped]{raycasting}}
#' 
#' @docType method
#' 
#' @rdname in_object-method
#' 
#' @export
setGeneric("in_object", function(object, x, ...) standardGeneric("in_object"))

#' @rdname in_object-method
setMethod("in_object", signature(object = "polygon"), function(object, 
                                                               x,
                                                               cpp = TRUE) {

    # If x is not a matrix, make it one. This will allow us to use `in_object`
    # in a vectorized manner (taking in a matrix of coordinates)
    if(is.data.frame(x)) {
        x <- as.matrix(x)
    }

    if(!is.matrix(x)) {
        x <- matrix(x, ncol = 2)
    }

    # Check whether cpp requested
    if(cpp) {
        return(in_object_rcpp(object, x))
    }

    # Use the raycasting algorithm to determine whether the points in x are 
    # contained in the polygon.
    return(raycasting(object@points, x))
})

#' @rdname in_object-method
setMethod("in_object", signature(object = "rectangle"), function(object, 
                                                                 x,
                                                                 cpp = TRUE) {

    # If x is not a matrix, make it one. This will allow us to use `in_object`
    # in a vectorized manner (taking in a matrix of coordinates)
    if(is.data.frame(x)) {
        x <- as.matrix(x)
    }
    
    if(!is.matrix(x)) {
        x <- matrix(x, ncol = 2)
    }

    # Check whether cpp requested
    if(cpp) {
        return(in_object_rcpp(object, x))
    }

    # Use the raycasting algorithm to determine whether the points in x are 
    # contained in the rectangle. Has been shown to be more efficient than doing the 
    # proper calculations for rectangle. For legacy reasons, still kept commented
    # in.
    return(raycasting(object@points, x))

    # # Rotate the rectangle and point in the same direction and around the same
    # # center (the center of the rectangle)
    # rect <- rotate(object, radians = -object@orientation)
    # x <- rotate(x, center = object@center, radians = -object@orientation)

    # # Determine the limits of the rectangle
    # xlims <- range(rect@points[,1])
    # ylims <- range(rect@points[,2])

    # # Check whether the point lies inside of the rectangle
    # check <- (x[,1] > xlims[1]) & (x[,1] < xlims[2]) &
    #          (x[,2] > ylims[1]) & (x[,2] < ylims[2])

    # if(outside) {
    #     check <- !check
    # }
    # return(check) # Important: Benchmark shows that the other algorithm is faster
})

#' @rdname in_object-method
setMethod("in_object", signature(object = "circle"), function(object, 
                                                              x,
                                                              cpp = TRUE) {

    # If x is not a matrix, make it one. This will allow us to use `in_object`
    # in a vectorized manner (taking in a matrix of coordinates)
    if(is.data.frame(x)) {
        x <- as.matrix(x)
    }
    
    if(!is.matrix(x)) {
        x <- matrix(x, ncol = 2)
    }

    # Check whether cpp requested
    if(cpp) {
        return(in_object_rcpp(object, x))
    }

    # Compute the distance between the coordinate and the center of the circle.
    # If this distance is smaller than the radius, the point is within the 
    # circle.
    y <- center(object)

    return((x[,1] - y[1])^2 + (x[,2] - y[2])^2 < radius(object)^2)
})

#' @rdname in_object-method
setMethod("in_object", signature(object = "segment"), function(object, 
                                                               x,
                                                               ...) {

    # If x is not a matrix, make it one. This will allow us to use `in_object`
    # in a vectorized manner (taking in a matrix of coordinates)
    if(is.data.frame(x)) {
        x <- as.matrix(x)
    }
    
    if(!is.matrix(x)) {
        x <- matrix(x, ncol = 2)
    }

    # For a point to be contained in a line, we need to verify that the distance
    # of the point to the start and end of the segment equals the distance from 
    # start to end (that is the size of the segment).
    distance_1 <- sqrt((from(object)[1] - x[,1])^2 + (from(object)[2] - x[,2])^2)
    distance_2 <- sqrt((to(object)[1] - x[,1])^2 + (to(object)[2] - x[,2])^2)

    return((distance_1 + distance_2) == size(object))
})



#' Check Whether a Point Lies Outside of an Object
#' 
#' Returns the opposite of the \code{\link[predped]{in_object-method}}. 
#' Currently works for all classes inside of the \code{\link[predped]{object-class}}.
#'
#' @param object Object of the \code{\link[predped]{object-class}}.
#' @param x Numeric vector or matrix containing x- and y-coordinates to be 
#' checked.
#' @param cpp Logical denoting whether to use the Rcpp alternative (\code{TRUE})
#' or the R alternative of this function (\code{FALSE}). Defaults to \code{FALSE}.
#'
#' @return Logical whether the point is outside of the object (\code{TRUE}) or 
#' inside of the object (\code{FALSE}).
#' 
#' @examples 
#' # Let's create an object
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' 
#' # Let's create a matrix of different coordinates of which the first is 
#' # inside of the object, the second on its circumference, and the third  
#' # outside of the object
#' coords <- rbind(c(0, 0), 
#'                 c(1, 0), 
#'                 c(2, 0))
#' 
#' # Let's do the test
#' out_object(my_circle, coords)
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{in_object}},
#' \code{\link[predped]{raycasting}}
#' 
#' @docType method
#' 
#' @rdname out_object-method
#' 
#' @export
setGeneric("out_object", function(object, x, ...) standardGeneric("out_object"))

#' @rdname out_object-method
setMethod("out_object", signature(object = "object"), function(object, x, ...) !in_object(object, x, ...))



#' Make an Object Larger
#' 
#' Returns an object that is larger than the originally provided one. Used under 
#' the hood for deleting nodes or potential path points that come too close to 
#' the objects in the environment. Works for all objects of the 
#' \code{\link[predped]{object-class}} with exception of 
#' \code{\link[predped]{segment-class}}.
#'
#' @param object An object of a type that extends 
#' \code{\link[predped]{object-class}}, with exception of the 
#' Works for all objects of the 
#' \code{\link[predped]{segment-class}}.
#' @param extension Numeric denoting the length with which to extend the object 
#' in all directions.
#'
#' @return Object of the same class as the original, but with a larger 
#' size.
#' 
#' @examples 
#' # Create an object
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' my_circle@size
#' 
#' # Increase the size of the object
#' larger_circle <- enlarge(my_circle, extension = 1)
#' larger_circle@size
#' 
#' # Decrease the size of the object
#' smaller_circle <- enlarge(my_circle, extension = -0.5)
#' smaller_circle@size
#' 
#' @seealso
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{size<-}}
#' 
#' @docType method
#' 
#' @rdname enlarge-method
#' 
#' @export
setGeneric("enlarge", function(object, extension) standardGeneric("enlarge"))

#' @rdname enlarge-method
setMethod("enlarge", signature(object = "polygon"), function(object, 
                                                             extension) {

    # Find the nodes of the polygon and use these new nodes as the points of 
    # the enlarged polygon.
    object@points <- add_nodes(object, 
                               space_between = extension, 
                               only_corners = TRUE)
    return(object)
})

#' @rdname enlarge-method
setMethod("enlarge", signature(object = "rectangle"), function(object, 
                                                               extension) {

    # Extend the size of the rectangle with the factor sqrt{space_between^2 / 2}, 
    # as we do in the `add_nodes` function.
    size(object) <- object@size + 2 * sqrt(extension^2 / 2)

    return(object)
})

#' @rdname enlarge-method
setMethod("enlarge", signature(object = "circle"), function(object, 
                                                            extension) {

    # Extend the radius with extension.
    radius(object) <- radius(object) + extension
    return(object)
})



#' Sample a Random Point on the Circumference
#' 
#' Currently works for all instances of \code{\link[predped]{object-class}}.
#' 
#' @details 
#' Note that while \code{\link[predped]{rectangle-class}} is not explicitly 
#' mentioned here, this method does work for this class of objects.
#' 
#' Furthermore note that \code{forbidden} is ignored for the 
#' \code{\link[predped]{segment-class}}.
#'
#' @param object Object of \code{\link[predped]{object-class}}.
#' @param middle_edge Logical denoting whether the point should lie in the middle
#' of a random edge. Ignored for circles. Defaults to \code{TRUE}.
#' @param forbidden Numeric indicating where the random point cannot be drawn.
#' For \code{\link[predped]{polygon-class}} and 
#' \code{\link[predped]{rectangle-class}}, this is the edge number on which the 
#' point should not be generated. The edge number is determined through the 
#' \code{points} slot, where two coordinates create an edge. For 
#' \code{\link[predped]{circle-method}}, this is either a vector or a matrix of 
#' angles between which the point should not be sampled (in radians). For the 
#' latter, it is important to note that the intervals created by the angles 
#' should not overlap. Defaults to \code{NULL}, ensuring that a point can be 
#' sampled anywhere on the circumference of the object.
#'
#' @return Numerical vector denoting a coordinate on the circumference of the 
#' provided object.
#' 
#' @examples 
#' # Create an object
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' 
#' # Generate a point on the circumference of the circle without limitations
#' rng_point(my_circle)
#' 
#' # Generate a point on the circumference of the circle with limitations, so 
#' # that it cannot lie between the angles (0, pi/2) and (pi, 3 * pi/2), 
#' # meaning the coordinate cannot have both positive or both negative values 
#' # in its coordinates (one always has to be positive, the other negative).
#' rng_point(my_circle, 
#'           forbidden = rbind(c(0, pi/2), 
#'                             c(pi, 3 * pi/2)))
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{add_nodes}},
#' \code{\link[predped]{nodes_on_circumference}}
#' 
#' @docType method
#' 
#' @rdname rng_point-method
#' 
#' @export
# 
# TO DO:
#   - Allow for forbidden to be specified at the object-level, so that this 
#     can be accounted for when sampling goals from them
setGeneric("rng_point", function(object, middle_edge = TRUE) standardGeneric("rng_point"))

#'@rdname rng_point-method
setMethod("rng_point", signature(object = "polygon"), function(object, 
                                                               middle_edge = TRUE) {

    forbidden <- forbidden(object)
    
    # Sample the edge on which to draw a random point. To make our lives easier, 
    # we first transform `points` so that it contains the values of the 
    # first coordinate (x_1, y_1) in the first two columns and the values of the
    # second coordiante (x_2, y_2) in the last two columns. Each edges is then 
    # defined in each row.
    #
    # Importantly, the forbidden edges should be deleted from the options.
    edges <- cbind(object@points, object@points[c(2:nrow(object@points), 1),])
    if(length(forbidden) != 0) {
        edges <- edges[-forbidden,]

        # Extra check if you delete all except one edge
        if(!is.matrix(edges)) {
            edges <- matrix(edges, ncol = 4)
        }
    }

    tryCatch(idx <- sample(seq_len(nrow(edges)), 1), error = function(e) browser())

    # If the middle of the edge should not be sampled, first sample a random 
    # number between 0 and 1. This number will determine how far along the 
    # edge the point will be put.
    if(middle_edge) {
        a <- 0.5
    } else {
        a <- runif(1, 0, 1)
    }

    # Return the sampled point as a numeric
    return(as.numeric(edges[idx, 1:2] + a * (edges[idx, 3:4] - edges[idx, 1:2])))
})   

#'@rdname rng_point-method
setMethod("rng_point", signature(object = "circle"), function(object, 
                                                              middle_edge = TRUE) {

    forbidden <- forbidden(object)

    # First check which intervals are allowed to be sampled and add a column 
    # that contains the relative weight of the interval to be sampled in
    if(length(forbidden) != 0) {
        # Convert to vector and sort
        forbidden <- sort(as.numeric(forbidden))

        # Create a matrix that contains all intervals that can be sampled in
        allowed <- c(0, forbidden, 2 * pi)
        allowed <- matrix(allowed, ncol = 2, byrow = TRUE)
        allowed <- cbind(allowed, 
                         (allowed[,2] - allowed[,1]) / sum(allowed))

        # Sample one of the intervals based on the weight of that interval
        idx <- sample(1:nrow(allowed), 1, prob = allowed[,3])

        # Now sample a random angle from this interval
        angle <- runif(1, allowed[idx,1], allowed[idx,2])
    } else {
        # If no intervals are forbidden, then we can just sample a random number
        # directly
        angle <- runif(1, 0, 2 * pi)
    }
    
    # Return the numerical value of the coordinate that was sampled
    return(as.numeric(object@center + object@radius * c(cos(angle), sin(angle))))
})   

#'@rdname rng_point-method
setMethod("rng_point", signature(object = "segment"), function(object,
                                                               middle_edge = TRUE) {

    if(middle_edge) {
        return(as.numeric(center(object)))
    } else {
        a <- runif(1, 0, 1)
        return(object@from + a * c(object@to[1] - object@from[1], object@to[2] - object@from[2]))
    }
})   



#' Add Nodes along the Outside or Inside of an Object
#' 
#' Used in the \code{\link[predped]{create_nodes}} function for creating path-
#' points the agents can use to walk around objects. Currently works for all 
#' instances of \code{\link[predped]{object-class}}, but only returns 
#' \code{NULL} for the \code{\link[predped]{segment-class}}.
#' 
#' @details 
#' This method is related to the \code{\link[predped]{nodes_on_circumference-method}},
#' but differs in the respect that 
#' \code{\link[predped]{nodes_on_circumference-method}} adds nodes directly on the 
#' circumference of an object, while \code{add_nodes} adds nodes on the outside 
#' of inside of an object.
#' 
#' Please note that if \code{only_corners = TRUE} for the 
#' \code{\link[predped]{circle-class}}, that the nodes are chosen in such a way
#' that they are at a distance of \code{space_between} away from the 
#' circumference of the circle AND so that this node is connected to its 
#' neighbours through a tangential line to the circle. In agent-based terms, 
#' this means that if an agent stands at one of the nodes that are created, 
#' they can also see alternative nodes around the circle. If 
#' \code{only_corners = FALSE}, the general logic is used to determine where the 
#' other nodes should end up.
#'
#' @param object Object of \code{\link[predped]{object-class}}.
#' @param space_between Numeric denoting the space to leave between the 
#' circumference of the object and the nodes to create. When \code{outside = TRUE},
#' \code{space_between} distance is created to the outside of the object, while
#' if \code{outside = FALSE} this same distance is created towards the inside 
#' of the object. Defaults to \code{0.5}.
#' @param only_corners Logical denoting whether to only add nodes at the corners 
#' of each object (\code{TRUE}) or to also add nodes between these newly created
#' corner-nodes (\code{FALSE}). When \code{only_corners = FALSE}, other nodes 
#' that are added have a distance of \code{space_between} between them. 
#' Defaults to \code{FALSE}.
#' @param outside Logical denoting whether the nodes should lie on the outside 
#' (\code{TRUE}) or inside (\code{FALSE}) of the object. Defaults to \code{TRUE}. 
#'
#' @return Numerical matrix containing the nodes that were created around/within
#' the provided object.
#' 
#' @examples 
#' # Create an object
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' 
#' # Generate nodes that fall around this circle with a distance of 1 around 
#' # the circle
#' add_nodes(my_circle, space_between = 1)
#' 
#' # Note that for segments, this function returns NULL
#' my_segment <- segment(from = c(0, 0), to = c(2, 2))
#' add_nodes(my_segment)
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{nodes_on_circumference}},
#' \code{\link[predped]{create_nodes}}
#' 
#' @docType method
#' 
#' @rdname add_nodes-method
#' 
#' @export
setGeneric("add_nodes", function(object, ...) standardGeneric("add_nodes"))

#'@rdname add_nodes-method
#
# Important; assumes that the intersection point between the two edges is on 
# the third and fourth column of `edge_1` or on the first and second column of 
# `edge_2`.
setMethod("add_nodes", signature(object = "polygon"), function(object, 
                                                               space_between = 0.5,
                                                               only_corners = FALSE,
                                                               outside = TRUE) {
    
    # Create a local function that will take in two coordinates and will return
    # the location of the new coordinate
    find_location <- function(edge_1, edge_2) {
        # Compute the slopes created by the two lines
        slope_1 <- (edge_1[4] - edge_1[2]) / (edge_1[3] - edge_1[1])
        slope_2 <- (edge_2[4] - edge_2[2]) / (edge_2[3] - edge_2[1])

        # Compute the angle between the edges. The slopes can be used for this 
        # identification, but they do not allow us to differentiate between the 
        # two potential angles that are pi radians apart. To differentiate 
        # between the alternatives, we need to use the following rule of thumb:
        #   - Slope 1: If the x-coordinate of the corner point (edge_1[3] or 
        #     edge_2[1]) is greater than the x-coordinate of the other point that 
        #     makes up the edge (edge_1[1]), then we should add pi to the angle
        #     retrieved from atan (edge opens up to the left).
        #   - Slope 2: Here, we add pi when the x-coordinate of the corner point 
        #     is smaller than the x-coordinate of the other point of the edge
        #     (edge again opens up to the left).
        angle_1 <- atan(slope_1)
        angle_1 <- ifelse(edge_1[3] >= edge_1[1], angle_1 + pi, angle_1)

        angle_2 <- atan(slope_2)
        angle_2 <- ifelse(edge_2[3] >= edge_2[1], angle_2, angle_2 + pi)

        # Define the actual angle at which the cosine and sine should be taken
        # and compute the point
        angle <- 0.5 * (angle_2 - angle_1) + angle_1
        
        point <- rbind(edge_2[1:2] + c(cos(angle), sin(angle)) * space_between,
                       edge_2[1:2] + c(cos(angle + pi), sin(angle + pi)) * space_between)

        return(point)
    }

    # Create the edges as needed by the `find_location` function. Important to 
    # note that we need to rbind the first edge to the matrix in order to find 
    # the node for the final point in the matrix.
    #
    # Note that in the first matrix, we start at the last defined edge in the 
    # points of the polygon and combine it with the very first edge. The reason
    # why we do this is to ensure that whenever `add_nodes` is called, we retain
    # the same order of the points inside the `points` matrix of the polygon. 
    # This in turn is done so that the function `enlarge` does not change the 
    # order of the points that make up the polygon, making sure that the 
    # mapping between the edges and their `forbidden` indices to remain the 
    # same.
    N <- nrow(object@points)
    idx <- c(N, 1:(N - 1))

    edges <- cbind(object@points[idx, ], object@points)
    edges <- rbind(edges, edges[1,])

    # Loop over the edges and do the necessary calculations. Then bind together
    # all nodes into a matrix and check which ones fall inside (or outside) of 
    # the object (to be deleted)
    nodes <- lapply(seq_len(nrow(edges) - 1), 
                    \(x) find_location(edges[x,], edges[x + 1,]))
    nodes <- do.call("rbind", nodes)

    idx <- in_object(object, nodes)
    if(outside) {
        idx <- !idx
    }

    nodes <- nodes[idx,]

    # Delete NAs in the nodes
    nodes <- nodes[!is.na(nodes[,1]),]

    # Return the nodes as is when you only want nodes to be created at the 
    # corners of the polygon
    if(only_corners) {
        return(nodes)
    }

    # We need to add some additional spaces so that there is `space_between` 
    # amount of space between each of the now created nodes. 
    corner_nodes <- rbind(nodes, nodes[1,])
    for(i in seq_len(nrow(corner_nodes) - 1)) {
        # Get the slope and size of the line defined by the two nodes. If the 
        # length of the line is smaller than `space_between`, we don't need to 
        # put in some additional points
        slope <- (corner_nodes[i,2] - corner_nodes[i + 1, 2]) / (corner_nodes[i,1] - corner_nodes[i + 1, 1])
        size <- sqrt((corner_nodes[i,1] - corner_nodes[i + 1, 1])^2 + (corner_nodes[i,2] - corner_nodes[i + 1, 2])^2)

        if(size < space_between) {
            next
        }

        # Divide the line in equal pieces proportional to the amount of space 
        # you have
        number_points <- ceiling(size / space_between)
        dist <- size / number_points

        # Find the points on the line that correspond to each of the distances 
        # and bind them to the `nodes` matrix. In order for this to work, we 
        # should add the distances from a starting point to the end point by using
        # the angle of the slope with a radius equal to the distance from the 
        # starting point to the end point. The starting point is always defined 
        # as that node for which x is minimal, except when the line drawn is 
        # vertical, in which case the y-coordinate matters
        angle <- atan(slope)
        if(slope == -Inf) {
            idx <- which.max(corner_nodes[i:(i + 1), 2])
        } else if(slope == Inf) {
            idx <- which.min(corner_nodes[i:(i + 1), 2])
        } else {
            idx <- which.min(corner_nodes[i:(i + 1), 1])
        }
        start <- corner_nodes[c(i:(i + 1))[idx],]

        new_nodes <- cbind(start[1] + 1:(number_points - 1) * cos(angle) * dist,
                           start[2] + 1:(number_points - 1) * sin(angle) * dist)
        nodes <- rbind(nodes, new_nodes)        
    }

    return(nodes)
})

# #'@rdname add_nodes-method
# setMethod("add_nodes", signature(object = "rectangle"), function(object, 
#                                                                  space_between = 0.5,
#                                                                  only_corners = FALSE,
#                                                                  outside = TRUE) {

#     # Approach will be to make a new rectangle that is greater than the original
#     # one by a given factor and then taking its points as the new nodes. If we
#     # want `space_between` space between the corners of the old rectangle and 
#     # the corners of the new one, we will have to create an `extension` factor
#     # based on the rule of Pythagoras, where we know that c^2 = `space_between`^2
#     # and a^2 = b^2 = `extension`^2.
#     extension <- sqrt(space_between^2 / 2)

#     # Use the setter for the size of a rectangle to change its size. This setter
#     # will automatically change the corner points inside of the rectangle.
#     #
#     # Importantly, the size of the rectangle changes with 2 times the extension
#     size(object) <- size(object) + ifelse(outside, 2, -2) * extension 
    
#     # Return the nodes as is when you only want nodes to be created at the 
#     # corners of the polygon
#     if(only_corners) {
#         return(object@points)
#     } 

#     # We need to add some additional spaces so that there is `space_between` 
#     # amount of space between each of the now created nodes. For this, we will 
#     # use logic that is the same as for polygons.
#     nodes <- object@points
#     edges <- cbind(nodes, nodes[c(2:nrow(nodes), 1),])

#     # Define the number of times each of the lines defined by the edges should 
#     # be divided by
#     sizes <- sqrt((edges[,1] - edges[,3])^2 + (edges[,2] - edges[,4])^2)

#     # Delete those sizes that are smaller than space_between
#     idx <- sizes > space_between 
#     sizes <- sizes[idx]
#     edges <- edges[idx,]

#     # Get the spaces that should be left inbetween the next points on the grid
#     number_points <- ceiling(sizes / space_between)
#     distances <- sizes / number_points

#     # Find the points on the line that correspond to each of the distances 
#     # and bind them to the `nodes` matrix. In order for this to work, we 
#     # should add the distances from a starting point to the end point by using
#     # the angle of the slope with a radius equal to the distance from the 
#     # starting point to the end point. The starting point is always defined 
#     # as that node for which x is minimal, except when the line drawn is 
#     # vertical, in which case the y-coordinate matters
#     # slopes <- (edges[,2] - edges[,4]) / (edges[,1] - edges[,3])
#     slopes <- (edges[,4] - edges[,2]) / (edges[,3] - edges[,1])
#     angles <- atan(slopes)

#     # In the last step, we loop over these values to create the new nodes
#     new_nodes <- list()
#     for(i in seq_len(nrow(nodes))) {
#         if(slopes[i] == 0) {
#             idx <- c(1, 3)[which.min(edges[i, c(1, 3)])]
#         } else {
#             idx <- 1
#         }

#         new_nodes[[i]] <- cbind(1:(number_points[i] - 1) * cos(angles[i]) * distances[i] + edges[i, idx],
#                                 1:(number_points[i] - 1) * sin(angles[i]) * distances[i] + edges[i, idx + 1])
#     }
#     nodes <- rbind(nodes, do.call("rbind", new_nodes))

#     return(nodes)
# })

#'@rdname add_nodes-method
setMethod("add_nodes", signature(object = "circle"), function(object, 
                                                              space_between = 0.5,
                                                              only_corners = FALSE,
                                                              outside = TRUE) {
    
    
    # Number of default nodes depends on a small calculation we made. Specifically,
    # if you have a circle of radius R and a point with a distance D between the 
    # point and the circumference of the circle, then we can draw a tangential
    # line to the circle which will define the next point that can be seen from 
    # that location (which is what we usually use `add_nodes` for). 
    #
    # The angle at which we ensure that this is possible is defined as: 
    #   \alpha = 2 * cos^ {-1}( \frac{R}{R + D} )
    #
    # Which we obtain through computing the cosine of the angle \alpha between 
    # the direction of the original point and the intersection point and then 
    # taking its inverse and multiplying by two (creating the angle at which the 
    # point symmetric to the intersection point should appear).
    #
    # It is useful to note that the original solution to a symmetric other point
    # involves doubling the original \alpha (as in the solution above). In our 
    # implementation, we do not do so, thus having twice the needed number of 
    # nodes added alongside the circle.
    fraction <- radius(object) / (radius(object) + space_between) 
    alpha <- acos(fraction)

    # Create the angles at which to put the nodes around the circle
    angles <- seq(0, 2 * pi, alpha)

    # Create a matrix of locations based on the center of the object, the radius,
    # and the drawn angles and return. Importantly, radius is extended with a
    # number `space_between` so that some space is left between the object and 
    # the path point
    adjusted_radius <- ifelse(outside, 
                              radius(object) + space_between,
                              radius(object) - space_between)
    nodes <- cbind(center(object)[1] + cos(angles) * adjusted_radius, 
                   center(object)[2] + sin(angles) * adjusted_radius)

    # The argument `only_corners` is quite the misnomer for the circle class, but
    # is chosen to be consistent with the other `add_nodes` functions. Basically,
    # the `only_corners` defines whether we just use the pi/4 radians points 
    # that are chosen, or whether we should add some additional points that 
    # make use of the `space_between` argument
    if(only_corners) {
        return(nodes)
    }

    # Compute the circumference of the circle for each pi/4 part 
    dist_part <- 2 * pi * adjusted_radius / 8

    # If this distance is smaller than `space_between`, we can just return the 
    # already acquired nodes
    if(dist_part < space_between) {
        return(nodes)
    }

    # Otherwise, we will examine how many additional points we can create and 
    # adjust the angle at which we sampled the nodes
    number_points <- ceiling(dist_part / space_between)
    adjusted_angle <- pi / (4 * number_points)

    # Now create the angles at which to sample and create the nodes in the way
    # we previously did
    angles <- seq(0, 2 * pi, adjusted_angle)
    nodes <- cbind(center(object)[1] + cos(angles) * adjusted_radius, 
                   center(object)[2] + sin(angles) * adjusted_radius)

    return(nodes)
})

#'@rdname add_nodes-method
setMethod("add_nodes", signature(object = "segment"), function(object, 
                                                               ...) {
    
    # Should not add nodes to a line
    return(NULL)
})



#' Add Nodes on the Circumference of an Object
#' 
#' Used in the \code{\link[predped]{overlap_with_objects}} function for creating 
#' nodes of which their presence within an agent can be checked in an efficient 
#' way (see \code{\link[predped]{moving_options-method}} and 
#' \code{\link[predped]{in_object-method}}). Currently works for all 
#' instances of \code{\link[predped]{object-class}}, but only returns 
#' \code{NULL} for the \code{\link[predped]{segment-class}}.
#' 
#' @details 
#' Related to the \code{\link[predped]{add_nodes-method}} with the main difference
#' being that the \code{\link[predped]{add_nodes-method}} adds nodes around or 
#' within an object, while \code{nodes_on_circumference} adds nodes directly on
#' the circumference of an object.
#' 
#' Note that while \code{\link[predped]{rectangle-class}} is not explicitly 
#' mentioned here, this method does work for this class of objects.
#'
#' @param object Object of \code{\link[predped]{object-class}}.
#' @param space_between Numeric denoting the space to leave between the 
#' circumference of the object and the nodes to create. When \code{outside = TRUE},
#' \code{space_between} distance is created to the outside of the object, while
#' if \code{outside = FALSE} this same distance is created towards the inside 
#' of the object. Defaults to \code{5e-2}.
#' @param cpp Logical denoting whether to use the Rcpp alternative (\code{TRUE})
#' or the R alternative of this function (\code{FALSE}). Defaults to \code{FALSE}.
#'
#' @return Numerical matrix containing the nodes that were created around/within
#' the provided object.
#' 
#' @examples 
#' # Create an object
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' 
#' # Generate nodes that fall around this circle with a distance of 1 around 
#' # the circle
#' nodes_on_circumference(my_circle, space_between = pi / 2)
#' 
#' # Note that for segments, this function returns NULL
#' my_segment <- segment(from = c(0, 0), to = c(2, 2))
#' nodes_on_circumference(my_segment)
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{add_nodes}},
#' \code{\link[predped]{in_object}}, 
#' \code{\link[predped]{moving_options}}
#' 
#' @docType method
#' 
#' @rdname nodes_on_circumference-method
#' 
#' @export
setGeneric("nodes_on_circumference", function(object, ...) standardGeneric("nodes_on_circumference"))

#'@rdname nodes_on_circumference-method
setMethod("nodes_on_circumference", signature(object = "polygon"), function(object, 
                                                                            space_between = 5e-2,
                                                                            cpp = FALSE) {

    if(cpp) {
        return(nodes_on_circumference_rcpp(object, space_between))
    }

    corners <- object@points 
    n <- nrow(corners)

    x_changes <- cbind(corners[,1], corners[c(2:n, 1), 1])
    y_changes <- cbind(corners[,2], corners[c(2:n, 1), 2])

    len_x <- (x_changes[,2] - x_changes[,1]) / space_between
    len_y <- (y_changes[,2] - y_changes[,1]) / space_between

    len <- sqrt(len_x^2 + len_y^2) + 1

    nodes <- cbind(as.numeric(unlist(multi_seq(x_changes[,1], 
                                               x_changes[,2],
                                               length.out = len))),
                   as.numeric(unlist(multi_seq(y_changes[,1], 
                                               y_changes[,2],
                                               length.out = len))))

    return(nodes)
})

#'@rdname nodes_on_circumference-method
setMethod("nodes_on_circumference", signature(object = "circle"), function(object, 
                                                                           space_between = 5e-2,
                                                                           cpp = FALSE) {

    if(cpp) {
        return(nodes_on_circumference_rcpp(object, space_between))
    }

    return(points(object, length.out = ceiling(2 * pi * radius(object) / space_between)))
})

#'@rdname nodes_on_circumference-method
setMethod("nodes_on_circumference", signature(object = "segment"), function(object, 
                                                                            ...) {

    # No circumference to add nodes to
    return(NULL)
})



#' Check whether an Object intersects with Other Object
#'
#' Currently works for all combinations of instances of 
#' \code{\link[predped]{object-class}}.
#' 
#' @details 
#' Note that this function is less efficient than the combination of 
#' \code{\link[predped]{nodes_on_circumference-method}} and 
#' \code{\link[predped]{in_object-method}}, which is why this combination is 
#' used in \code{\link[predped]{overlap_with_objects}} instead of the 
#' \code{intersects} method
#' 
#' Note that while \code{\link[predped]{rectangle-class}} is not explicitly 
#' mentioned here, this method does work for this class of objects.
#'
#' @param object Object of \code{\link[predped]{object-class}}.
#' @param other_object Object of \code{\link[predped]{object-class}} to check 
#' intersection with. 
#'
#' @return Numerical matrix containing the nodes that were created around/within
#' the provided object.
#' 
#' @examples 
#' # Create two objects that intersect with each other, and check their 
#' # intersection
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' my_rectangle <- rectangle(center = c(0, 0), size = c(0.75, 0.75))
#' 
#' intersects(my_circle, my_rectangle)
#' 
#' # Create two objects that do not intersect with each other, and check their 
#' # intersection
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' my_rectangle <- rectangle(center = c(0, 0), size = c(1.25, 1.25))
#' 
#' intersects(my_circle, my_rectangle)
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{in_object}}, 
#' \code{\link[predped]{moving_options}},
#' \code{\link[predped]{nodes_on_circumference}},
#' \code{\link[predped]{overlap_with_objects}}
#' 
#' @docType method
#' 
#' @rdname intersects-method
#' 
#' @export
setGeneric("intersects", function(object, other_object) standardGeneric("intersects"))

#'@rdname intersects-method
setMethod("intersects", 
          signature(object = "polygon", other_object = "polygon"), 
          function(object, other_object) {
              
    # Extract the points of the objects and create the edges to be 
    # evaluated
    edges_1 <- cbind(object@points, object@points[c(2:nrow(object@points), 1), ])
    edges_2 <- cbind(other_object@points, other_object@points[c(2:nrow(other_object@points), 1), ])

    # Use the line_line_intersection function
    return(line_line_intersection(edges_1, edges_2))
})

#'@rdname intersects-method
setMethod("intersects", 
          signature(object = "polygon", other_object = "circle"), 
          function(object, other_object) intersects(other_object, object))

#'@rdname intersects-method
setMethod("intersects", 
          signature(object = "polygon", other_object = "segment"), 
          function(object, other_object) intersects(other_object, object))

#'@rdname intersects-method
setMethod("intersects", 
          signature(object = "circle", other_object = "polygon"), 
          function(object, other_object) {

    # Add nodes to the other object
    other <- add_nodes(other_object, space_between = 1e-2)
    return(any(in_object(object, other)))

    # Commented out because of mistakes: Does not adequately find the 
    # intersection between a line segment and a circle, leading agents to 
    # walk through objects. This is unwanted behavior and therefore fix 
    # above was implemented. 
    #
    # TO DO: Review this code.
    #
    # # Create the edges of the polygon
    # points <- other_object@points
    # edges <- cbind(points, points[c(2:nrow(points), 1),])

    # # Return the result of the general function circle_line_intersection
    # return(line_intersection(object, edges))   
})

#' @rdname intersects-method
setMethod("intersects", 
          signature(object = "circle", other_object = "circle"), 
          function(object, other_object) {

    # This case is rather easy, as we just need to determine whether the 
    # distance between the centers of the circles is smaller or bigger than 
    # the sum of their radii. 
    #
    # However, to ensure that a circle can also be contained within the 
    # other circle, we also have to ensure that the distance between the 
    # centers is bigger than the difference between the radii
    distance <- m4ma::dist1(center(object), 
                            matrix(center(other_object), ncol = 2))

    return((distance <= radius(object) + radius(other_object)) & 
           (distance >= abs(radius(object) - radius(other_object))))
})

#' @rdname intersects-method
setMethod("intersects", 
          signature(object = "circle", other_object = "segment"), 
          function(object, other_object) return(other_object, object))

#'@rdname intersects-method
setMethod("intersects", 
          signature(object = "segment", other_object = "polygon"), 
          function(object, other_object) {

    # Here, we will loop over all points that make up the edges of the polygon
    # or rectangle and check whether they intersect with the 
    # line.line.intersection function
    edges <- cbind(other_object@points, 
                   other_object@points[c(2:nrow(points), 1),])
    return(line_line_intersection(matrix(c(object@from, object@to), nrow = 1),
                                  edges))

    return(any(idx)) 
})

#'@rdname intersects-method
setMethod("intersects", 
          signature(object = "segment", other_object = "circle"), 
          function(object, other_object) {

    # Originally, this made use of some very difficult calculations. However, 
    # when trying to maximize speed and simplicity, it might be more 
    # instrumental to use an approximate method. 
    #
    # We add equidistant points to the segment and then check whether these 
    # are contained within the agent
    by <- (object@size / 5e-2)^(-1)
    steps <- matrix(seq(0, 1, by = by), ncol = 1) 
    coords <- rep(object@from, each = length(steps)) + steps %*% matrix(object@to - object@from, nrow = 1)

    return(any(in_object(other_object, coords)))
})

#'@rdname intersects-method
setMethod("intersects", 
          signature(object = "segment", other_object = "segment"), 
          function(object, other_object) m4ma::line.line.intersection(object@from, 
                                                                      object@to, 
                                                                      other_object@from,
                                                                      other_object@to,
                                                                      interior.only = TRUE))



#' Check whether an Object intersects with Line Segments
#'
#' Generalization of the \code{\link[predped]{intersects-method}} for segments, 
#' allowing for vectorized checking of intersections with segments. Is often 
#' used within the \code{\link[predped]{intersects-method}}, especially when 
#' checking the intersections of \code{\link[predped]{polygon-class}} and 
#' \code{\link[predped]{rectangle-class}}. Currently works for all instances 
#' of \code{\link[predped]{object-class}}.
#' 
#' @details 
#' Note that while \code{\link[predped]{rectangle-class}} is not explicitly 
#' mentioned here, this method does work for this class of objects.
#' 
#' @param object Object of a type that extends \code{\link[predped]{object-class}}.
#' @param segments Numerical matrix of size N x 4 containing the coordinates of 
#' the line segments in order x_1, y_1, x_2, y_2.
#' @param return_all Logical denoting whether to return a vector of logicals 
#' that denote intersections of each line separately (\code{TRUE}) or whether 
#' to only return a single logical denoting whether any intersections 
#' occurred (\code{FALSE}). Defaults to \code{FALSE}.
#'
#' @return Logical vector (\code{return_all = TRUE}) or logical 
#' (\code{return_all = FALSE}) denoting whether the lines intersect with the 
#' provided object. Whenever there is an intersection, the logical is \code{TRUE}
#' and otherwise, the logical is \code{FALSE}.
#' 
#' @examples 
#' # Create two objects that intersect with each other, and check their 
#' # intersection
#' my_circle <- circle(center = c(0, 0), radius = 1)
#' my_lines <- rbind(c(0, 0, 2, 2), 
#'                   c(0, 2, 2, 2))
#' 
#' line_intersection(my_circle, my_lines)
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{polygon-class}},  
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}},
#' \code{\link[predped]{intersects}}
#' 
#' @docType method
#' 
#' @rdname line_intersection-method
#' 
#' @export
setGeneric("line_intersection", function(object, segments, ...) standardGeneric("line_intersection"))

#'@rdname line_intersection-method
setMethod("line_intersection", signature(object = "polygon"), function(object, 
                                                                       segments, 
                                                                       return_all = FALSE) {
    
    # Extract the points of the objects and create the edges to be 
    # evaluated
    edges <- cbind(object@points, object@points[c(2:nrow(object@points), 1), ])

    # Use the line_line_intersection function
    intersections <- line_line_intersection(edges, segments, return_all = return_all)

    # If you want to return all of the segments, then we need to rework the 
    # vector to a matrix with the edges in its columns. Then we can take the 
    # rowSums to indicate whether this edge has intersected with at least one of
    # the segments
    if(return_all) {
        intersections <- matrix(intersections, ncol = nrow(edges))
        return(rowSums(intersections) > 0L)
    } else {
        return(intersections)
    }
})

#'@rdname line_intersection-method
setMethod("line_intersection", signature(object = "circle"), function(object, 
                                                                      segments,
                                                                      return_all = FALSE) {

    if(is.data.frame(segments)) {
        segments <- as.matrix(segments)
    }
    
    intersecting_segments <- cbind(1:nrow(segments),
                                   rep(FALSE, each = nrow(segments)))

    # First check: Do any of the points that determine the segment fall 
    # within the circle? If so, there is an intersection. 
    #
    # Be careful, this is only true if only one of the points that make up the 
    # segment falls within the circle: If this is not the case, then the segment
    # might also just fall completely within the circle, which is fine for our
    # purposes.
    coords <- rbind(segments[,1:2], segments[nrow(segments), 3:4])
    idx <- in_object(object, coords)
    idy <- idx[1:(length(idx) - 1)] != idx[2:length(idx)] # Do the actual check: One point within, the other outside
    if(any(idy) & !return_all) {
        return(TRUE)
    }

    intersecting_segments[, 2] <- idx[1:(length(idx) - 1)]

    # Second check: Use the formula for an intersection of a line with a 
    # circle to determine whether the line itself intersects. For this to 
    # work, we need to offset the points of the edges with the center of 
    # the circle.
    #
    # Once we found that there is an intersection, we can compute the point 
    # at which the intersection happens and check whether it lies within the
    # two provided points that make up the edge. If not, then there is no 
    # actual intersection. 
    segments[,c(1, 3)] <- segments[,c(1, 3)] - center(object)[1]
    segments[,c(2, 4)] <- segments[,c(2, 4)] - center(object)[2]

    dx <- segments[,3] - segments[,1]
    dy <- segments[,4] - segments[,2]
    distance <- dx^2 + dy^2

    D <- segments[,1] * segments[,4] - segments[,3] * segments[,2]

    discriminant <- radius(object)^2 * distance - D^2

    # Check whether any of the edges are at risk of intersecting. If not, 
    # we can safely say that the circle does not intersect with the polygon
    idx <- discriminant >= 0
    if(!any(idx) & !return_all) {
        return(FALSE)
    }

    # If there are possible intersection, retain the points and other 
    # characteristics of the edges that might intersect with the circle
    segments <- segments[idx,]
    id_segments <- intersecting_segments[idx, 1]
    if(sum(idx) == 1) {
        # If we only retain one row, we have to transform the edges to a 
        # matrix again
        segments <- matrix(segments, nrow = 1)
    }

    dx <- dx[idx]
    dy <- dy[idx]
    distance <- distance[idx]
    D <- D[idx]
    discriminant <- discriminant[idx]
    
    # Compute the different intersection points with the circle and check 
    # whether these lie inbetween the segments of the polygon
    co <- cbind(D * dy + sign(dy) * dx * sqrt(discriminant),
                -D * dx + abs(dy) * sqrt(discriminant),
                D * dy - sign(dy) * dx * sqrt(discriminant),
                -D * dx - abs(dy) * sqrt(discriminant)) / distance

    if(nrow(segments) == 1) {
        ranges <- c(range(segments[, c(1, 3)]), range(segments[, c(2, 4)])) |>
            matrix(nrow = 1)
    } else {
        ranges <- cbind(matrixStats::rowRanges(segments[, c(1, 3)]),
                        matrixStats::rowRanges(segments[, c(2, 4)]))
    }

    x_check_1 <- (co[,1] <= ranges[,2]) & (co[,1] >= ranges[,1])
    y_check_1 <- (co[,2] <= ranges[,4]) & (co[,2] >= ranges[,3])
    x_check_2 <- (co[,3] <= ranges[,2]) & (co[,3] >= ranges[,1])
    y_check_2 <- (co[,4] <= ranges[,4]) & (co[,4] >= ranges[,3])

    idx <- (x_check_1 & y_check_1) | (x_check_2 & y_check_2)

    # Combine with all previous information and return whatever the research 
    # wants
    intersecting_segments[id_segments, 2] <- idx

    if(return_all) {
        return(intersecting_segments[,2])
    } else {
        return(any(intersecting_segments[,2]))
    }
})

#'@rdname line_intersection-method
setMethod("line_intersection", 
          signature(object = "segment"), 
          function(object, segments, return_all = FALSE) line_line_intersection(matrix(c(object@from, object@to), nrow = 1), 
                                                                                segments, 
                                                                                return_all = return_all))





################################################################################
# GETTERS AND SETTERS

#' @rdname center-method
setMethod("center", signature(object = "polygon"), function(object) {
    return(object@center)
})

#' @rdname center-method
setMethod("center<-", signature(object = "polygon"), function(object, value) {
    object@points <- cbind(object@points[,1] + value[1] - center(object)[1], 
                           object@points[,2] + value[2] - center(object)[2])
    object@center <- as(value, "coordinate")
    return(object)
})

#' @rdname center-method
setMethod("center", signature(object = "rectangle"), function(object) {
    return(object@center)
})

#' @rdname center-method
setMethod("center<-", signature(object = "rectangle"), function(object, value) {
    object@center <- as(value, "coordinate")
    object@points <- cbind(object@points[,1] + value[1],
                           object@points[,2] + value[2])
    return(object)
})

#' @rdname center-method
setMethod("center", signature(object = "circle"), function(object) {
    return(object@center)
})

#' @rdname center-method
setMethod("center<-", signature(object = "circle"), function(object, value) {
    object@center <- as(value, "coordinate")
    return(object)
})

#' @rdname center-method
setMethod("center", signature(object = "segment"), function(object) {
    return(object@center)
})

#' @rdname center-method
setMethod("center<-", signature(object = "segment"), function(object, value) {
    diff <- object@center - value

    object@from <- object@from - diff 
    object@to <- object@to - diff

    object@center <- value
    
    return(object)
})



#' @rdname forbidden-method
setMethod("forbidden", signature(object = "polygon"), function(object) {
    return(object@forbidden)
})

#' @rdname forbidden-method
setMethod("forbidden<-", signature(object = "polygon"), function(object, value) {
    object@forbidden <- value
    return(object)
})

#' @rdname forbidden-method
setMethod("forbidden", signature(object = "rectangle"), function(object) {
    return(object@forbidden)
})

#' @rdname forbidden-method
setMethod("forbidden<-", signature(object = "rectangle"), function(object, value) {
    object@forbidden <- value
    return(object)
})

#' @rdname forbidden-method
setMethod("forbidden", signature(object = "circle"), function(object) {
    return(object@forbidden)
})

#' @rdname forbidden-method
setMethod("forbidden<-", signature(object = "circle"), function(object, value) {
    object@forbidden <- value
    return(object)
})



#' @rdname from-method
setMethod("from", signature(object = "segment"), function(object, ...) {
    return(object@from)
})

#' @rdname from-method
setMethod("from<-", signature(object = "segment"), function(object, value) {
    object@from <- value
    object@size <- sqrt((object@from[1] - object@to[1])^2 + (object@from[2] - object@to[2])^2)
    object@center <- object@from + (object@to - object@from) / 2
    return(object)
})



#' @rdname id-method
setMethod("id", signature(object = "object"), function(object) {
    return(object@id)
})

#' @rdname id-method
setMethod("id<-", signature(object = "object"), function(object, value) {
    object@id <- value
    return(object)
})



#' @rdname orientation-method
setMethod("orientation", signature(object = "rectangle"), function(object) {
    return(object@orientation)
})

#' @rdname orientation-method
setMethod("orientation<-", signature(object = "rectangle"), function(object, value) {
    original_value <- object@orientation
    object@orientation <- value

    rect <- rotate(object, radians = value - original_value)
    object@points <- rect@points
    return(object)
})

#' @rdname orientation-method
setMethod("orientation", signature(object = "segment"), function(object) {
    return(object@orientation)
})

#' @rdname orientation-method
setMethod("orientation<-", signature(object = "segment"), function(object, value) {
    angle <- value - object@orientation
    object@orientation <- value   

    from_x0 <- object@from - object@center 
    to_x0 <- object@to - object@center 

    R <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), nrow = 2, ncol = 2)
    object@from <- object@center + as.numeric(R %*% from_x0)
    object@to <- object@center + as.numeric(R %*% to_x0)

    return(object)
})



#' @rdname points-method
setMethod("points", signature(object = "polygon"), function(object, ...) {
    return(object@points)
})

#' @rdname points-method
setMethod("points<-", signature(object = "polygon"), function(object, value) {
    object@points <- value 
    return(object)
})

#' @rdname points-method
setMethod("points", signature(object = "rectangle"), function(object, ...) {
    pts <- object@points 
    dimnames(pts) <- NULL
    return(pts)
})

#' @rdname points-method
setMethod("points", signature(object = "circle"), function(object, length.out = 100, ...) {
    # Create a vector of angles around the circle, allowing us to sample points 
    # at equidistant orientation on the circumference of the circle. Importantly, 
    # we sample length.out + 1 points and then delete the last one so that we 
    # don't end at the point 2 * pi twice: Gave a bug in the underlying code.
    angles <- seq(0, 2 * pi, length.out = length.out + 1)
    angles <- angles[-length(angles)]

    # Create the points themselves
    return(matrix(c(object@center[1] + object@radius * cos(angles),
                    object@center[2] + object@radius * sin(angles)), 
                  ncol = 2))
})

#' @rdname points-method
setMethod("points", signature(object = "segment"), function(object, ...) {
    return(rbind(object@from, object@to))
})

#' @rdname points-method
setMethod("points<-", signature(object = "segment"), function(object, value) {
    object@from <- value[1,]
    object@to <- value[2,]

    return(object)
})



#' @rdname position-method
setMethod("position", signature(object = "polygon"), function(object) {
    return(object@center)
})

#' @rdname position-method
setMethod("position<-", signature(object = "polygon"), function(object, value) {
    center(object) <- value
    return(object)
})

#' @rdname position-method
setMethod("position", signature(object = "rectangle"), function(object) {
    return(object@center)
})

#' @rdname position-method
setMethod("position<-", signature(object = "rectangle"), function(object, value) {
    center(object) <- value
    return(object)
})

#' @rdname position-method
setMethod("position", signature(object = "circle"), function(object) {
    return(object@center)
})

#' @rdname position-method
setMethod("position<-", signature(object = "circle"), function(object, value) {
    center(object) <- value
    return(object)
})

#' @rdname position-method
setMethod("position", signature(object = "segment"), function(object) {
    return(object@center)
})

#' @rdname position-method
setMethod("position<-", signature(object = "segment"), function(object, value) {
    center(object) <- value
    return(object)
})



#' @rdname radius-method
setMethod("radius", signature(object = "circle"), function(object) {
    return(object@radius)
})

#' @rdname radius-method
setMethod("radius<-", signature(object = "circle"), function(object, value) {
    object@radius <- value
    return(object)
})



#' @rdname size-method
setMethod("size", signature(object = "rectangle"), function(object) {
    return(object@size)
})

#' @rdname size-method
setMethod("size<-", signature(object = "rectangle"), function(object, value) {
    object@size <- value

    # Put the rectangle at the origin and rotate back to an orientation of 0
    pts <- object@points - rep(object@center, each = 4)

    alpha <- object@orientation
    R <- matrix(c(cos(-alpha), sin(-alpha), -sin(-alpha), cos(-alpha)), nrow = 2, ncol = 2)

    pts <- t(R %*% t(pts))

    # Create the new points matrix and use the signs of the previous rectangle. 
    # Should allow us to have the same order of the points in both cases.
    pts <- 0.5 * cbind(sign(pts[,1]) * value[1], 
                       sign(pts[,2]) * value[2])
    
    R <- matrix(c(cos(alpha), sin(alpha), -sin(alpha), cos(alpha)), nrow = 2, ncol = 2)

    pts <- t(R %*% t(pts))
    object@points <- cbind(pts[,1] + center(object)[1], 
                           pts[,2] + center(object)[2])

    return(object)
})

#' @rdname size-method
setMethod("size", signature(object = "circle"), function(object) {
    return(object@radius)
})

#' @rdname size-method
setMethod("size<-", signature(object = "circle"), function(object, value) {
    object@radius <- value
    return(object)
})

#' @rdname size-method
setMethod("size", signature(object = "segment"), function(object) {
    return(object@size)
})

#' @rdname size-method
setMethod("size<-", signature(object = "segment"), function(object, value) {
    # Assumption: The `from` coordinate remains the same. Then we can use an 
    # imaginary circle to get the new coordinate of `to`
    angle <- orientation(object)

    object@to <- object@from + value * c(cos(angle), sin(angle))
    object@size <- value
    object@center <- object@from + (object@to - object@from) / 2

    return(object)
})



#' @rdname to-method
setMethod("to", signature(object = "segment"), function(object, ...) {
    return(object@to)
})

#' @rdname to-method
setMethod("to<-", signature(object = "segment"), function(object, value) {
    object@to <- value
    object@size <- sqrt((object@from[1] - object@to[1])^2 + (object@from[2] - object@to[2])^2)
    object@center <- object@from + (object@to - object@from) / 2
    return(object)
})


