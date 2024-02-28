
setGeneric("rotate",
           function(object, degrees, radians, center) standardGeneric("rotate"),
           signature = "object"
)

#' An S4 Class to Represent Coordinate Vectors
#'
#' A named numeric vector that can only have two elements (x and y coordinates).
#'
#' @export
coordinate <- setClass("coordinate", contains = "numeric")

setMethod("initialize", "coordinate", function(.Object, ...) {
    .Object <- callNextMethod()

    if (length(.Object) != 2) stop("Coordinate vector must have length two (x and y)")

    names(.Object) <- c("x", "y")

    return(.Object)
})

#' Rotate Point Around Center
#'
#' @param object A numeric vector of length two with the point coordinates.
#' @param degrees A single numeric element indicating the degrees of rotation.
#' @param center A numerical vector of length two with the x and y coordinates
#' around which to rotate the point. Defaults to the origin (0, 0).
#'
#' @export
setMethod("rotate",
          signature(object = "numeric"),
          function(object, radians, center = c(0, 0)) {
              object <- as(object, "coordinate")
              center <- as(center, "coordinate")
              # Change coordinates to move around the origin
              x_origin <- object - center

              # Rotate the point to a new position
              M <- matrix(c(cos(radians), -sin(radians),
                            sin(radians), cos(radians)),
                          nrow = 2,
                          ncol = 2,
                          byrow = TRUE)

              y_origin <- as.numeric(M %*% x_origin)

              # Change coordinates back to the original center
              return(as(y_origin + center, "coordinate"))
          })

setAs("numeric", "coordinate", function(from) new("coordinate", from))

#' An S4 Abstract Base Class to Represent Objects
#'
#' @slot id A character giving the object a name.
#' @slot moveable A logical indicating whether the object can be moved from its
#' position.
#' @slot interactable A logical indicating whether the object can be interacted
#' with.
#' @slot busy A logical indicating whether an agent is interacting with it.
#' @slot interacted_with A logical indicating whether the object was interacted
#' with.
#'
#' @export
setClass("object", list(id = "character",
                        moveable = "logical",
                        interactable = "logical"),
         contains = "VIRTUAL")

#' Move an Object
#'
#' Move the center of the object to the target coordinates.
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#' @param target A numeric of length two with the target coordinates.
#'
#' @return The object moved to the target coordinates.
#' @export
#' @name move-method
setGeneric("move", function(object, target) standardGeneric("move"))

#' Calculate the Area of an Object
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#'
#' @return The area of the object.
#' @export
#' @name area-method
setGeneric("area", function(object) standardGeneric("area"))

#' Check Whether a Point Lies Within an Object
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#' @param x A numeric vector of length two with x and y coordinates of the point.
#' @param outside A logical indicating whether to return \code{TRUE} if the point
#' is outside the object.
#'
#' @return Logical whether the point is inside the object
#' (if \code{outside} is \code{FALSE}) or outside.
#' @export
#' @name in_object-method
setGeneric("in_object", function(object, x, outside = TRUE) standardGeneric("in_object"))

#' Add a Goal to an Object
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#'
#' @return The goal that was assigned to the object
#' @export
#' @name add_goal-method
setGeneric("add_goal", function(object) standardGeneric("add_goal"))

#' An S4 class to Represent Polygon Objects
#'
#' Polygons can be used to create flexible shapes and are defined through a set
#' of points. The last point is automatically connected with the first point.
#'
#' This class is suitable for creating custom object classes.
#'
#' @slot points A numeric matrix with two columns containing the
#' x- and y-coordinates of the points.
#' @slot clock_wise A single logical element indicating whether the points define
#' the polygon clockwise (default) or counter-clockwise.
#'
#' @export
#' @name polygon
polygon <- setClass("polygon", list(points = "matrix", clock_wise = "logical"), contains = "object")

setMethod("initialize", "polygon", function(.Object, clock_wise = TRUE, moveable = FALSE, ...) {
    .Object <- callNextMethod(.Object, ...)

    if (ncol(.Object@points) != 2) {
        stop("All points must have an x- and y-coordinate (two-column matrix)")
    }

    .Object@clock_wise <- clock_wise
    .Object@moveable <- moveable

    return(.Object)
})

#'@rdname in_object-method
#'
setMethod("in_object", signature(object = "polygon"), function(object, x, outside = TRUE) {
    # Use a Ray-casting algorithm to find out whether a point lies in a polygon.
    # This algorithm checks whether a point lies within an arbitrary polygon by
    # checking the even-odd-rule, which says that for any point (x,y) that lies
    # within a polygon, the number of times it cross the boundaries of this
    # polygon when x goes to infinity should be uneven/odd.
    #
    # This is not the most efficient algorithm, but it might be good to have
    # as the default, but to specify other algorithms when possible (e.g., for
    # rectangle, see below).
    #
    # TO DO: Benchmark this code and check if less efficient than the specific
    # method for rectangle

    counter <- 0

    # Extract the edges
    # A bit hacky now, but also put the first point of this matrix at the end
    # as well. This allows us to examine all edges
    edges <- object@points
    edges <- rbind(edges, edges[1,])
    for(i in 1:(nrow(edges) - 1)) {
        # Check whether the y-coordinate of the point is already above the
        # y-coordinates of both points that make up the edge. If this is the case,
        # then moving the x coordinate to infinity would just yield no
        # intersections. Return TRUE if this is not the case.
        check_1 <- (edges[i,2] > x[2]) != (edges[i + 1,2] > x[2])

        # Use a derived formula to find out for which value of the x coordinate
        # the imaginary horizontal line through the point `x` would intersect
        # with the edge. This derivation is based on deriving the equation
        # y = mx + b for the edge and then equation it to the equation y = x[2],
        # which represents the horizontal move from x[2] to infinity.
        slope <- (edges[i + 1,1] - edges[i,1]) / (edges[i + 1,2] - edges[i,2])
        x_intersection <- edges[i,1] + slope * (x[2] - edges[i,2])

        # Finally, check whether this intersection point lies further to the
        # right (towards infinity) than the initial value of the x coordinate.
        # If so, then the intersection indeed happens due to the move from the
        # x coordinate to infinity.
        check_2 <- x[1] < x_intersection

        # If both checks are TRUE, then there is an intersection and we should
        # increase the counter
        if(check_1 && check_2) {
            counter <- counter + 1
        }
    }

    # If outside == FALSE, return TRUE if you have an odd number of intersections.
    # If outside == TRUE, return TRUE if you have an even number of intersections.
    # otherwise
    #
    # In other words, return TRUE whenever the two booleans match (TRUE, TRUE ->
    # even number of intersections when outside of the polygon; FALSE, FALSE ->
    # odd number of intersections when inside of the polygon)
    return((counter %% 2 == 0) == outside)
})

#'@rdname in_object-method
#'
setMethod("add_goal", signature(object = "polygon"), function(object, 
                                                              id = character(0),
                                                              counter = 5
){
    # Get all the edges on which the goal can be attributed
    edges <- object@points 
    edges <- rbind(edges, edges[1,])

    # Select one of these edges at random and a random location on the line 
    # created by the points
    idx <- sample.int(nrow(objects@points), 1)
    co1 <- edges[idx,]
    co2 <- edges[idx + 1,]

    x <- runif(1, 0, 1)
    y <- co1[2] + ( (co2[2] - co1[2]) / (co2[1] - co1[1]) ) * (x - co1[1])

    # Create the goal itself
    return(goal(id = id,
                position = coordinate(c(x, y)), 
                counter = counter))    
})

#' An S4 Class to Represent Rectangle Objects
#'
#' Special case of the \code{\link[predped]{polygon-class}}.
#' Rectangles are defined by \code{center}, \code{size}, and \code{orientation} (optional).
#'
#' @slot center A numeric vector of length two with the coordinates of the center.
#' @slot size A numeric vector of length two with the object width and height.
#' @slot orientation A single numeric element indicating the orientation of the
#' rectangle in radians.
#'
#' @export
#' @name rectangle
rectangle <- setClass("rectangle", list(
        center = "numeric",
        size = "numeric",
        orientation = "numeric"
    ),
    contains = c("polygon")
)

setMethod("initialize", "rectangle", function(
        .Object,
        center,
        size,
        orientation = 0,
        moveable = FALSE, ...
) {
    if (length(size) != 2) stop("Size vector must have length two (x and y)")
    if (any(size <= 0)) stop("Size vector must be positive")
    if (length(orientation) != 1) stop("Orientation must be a single element")

    .Object@center <- as(center, "coordinate")

    size_half <- size/2

    lower <- center - size_half
    upper <- center + size_half

    points <- matrix(c(lower, c(lower[1], upper[2]), upper, c(upper[1], lower[2])), 4, 2, byrow = TRUE)

    if (orientation != 0) {
        points <- t(apply(points, 1, rotate, radians = orientation, center = center))
    }

    .Object <- callNextMethod(.Object, points = points, clock_wise = TRUE)

    .Object@size <- size
    .Object@moveable <- moveable
    .Object@orientation <- orientation

    return(.Object)
})

#' @rdname move-method
#' @export
setMethod("move", signature(object = "rectangle", target = "numeric"), function(object, target) {
    target <- as(target, "coordinate")

    if (!object@moveable) {
        return(object)
    }

    d <- target - object@center
    object@center <- target
    object@points <- object@points + d

    return(object)
})

#' Rotate a Rectangle
#'
#' @param object An object of a type \code{\link[predped]{rectangle-class}}.
#' @param degrees A single numeric element indicating the target rotation in degrees.
#' @param radians A single numeric element indicating the target rotation in radians.
#'
#' @return The object moved to the target coordinates.
#' @export
setMethod("rotate",
          signature(object = "rectangle"),
          function(object, degrees, radians) {
              # Transform degrees to radians
              if (missing(radians) && !missing(degrees)) {
                  radians <- degrees * pi / 180
              } else if (!(!missing(radians) && missing(degrees))) {
                  stop("Either 'degrees' or 'radians' must be provided")
              }

              object@orientation <- radians
              object@points <- t(apply(object@points, 1, rotate, radian = radians, center = object@center))

              return(object)
          }
)

#'@rdname area-method
#'
setMethod("area", signature(object = "rectangle"), function(object) prod(object@size))

#'@rdname in_object-method
#'
setMethod("in_object", signature(object = "rectangle"), function(object, x, outside = TRUE) {
    # Rotate the rectangle and point in the same direction and around the same
    # center (the center of the rectangle)
    rect <- rotate(object, radians = -object@orientation)
    x <- rotate(coordinate(x), center = object@center, radians = -object@orientation)

    # Determine the limits of the rectangle
    xlims <- range(rect@points[,1])
    ylims <- range(rect@points[,2])

    # Check whether the point lies inside of the rectangle
    check <- (x[1] > xlims[1]) & (x[1] < xlims[2]) &
             (x[2] > ylims[1]) & (x[2] < ylims[2])

    return(ifelse(outside, !check, check)) # Important: Benchmark shows that the other algorithm is faster
})

#' An S4 Class to Represent Circle Objects
#'
#' @slot center A numeric vector of length two indicating the center of the circle.
#' @slot radius A numeric indicating the radius of the circle.
#'
#' @export
circle <- setClass("circle", list(center = "numeric", radius = "numeric"), contains = c("object"))

setMethod("initialize", "circle", function(.Object, moveable = FALSE, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@center <- as(.Object@center, "coordinate")
    .Object@moveable <- moveable
    if (length(.Object@radius) != 1) stop("Slot 'radius' should be a single numeric value")
    return(.Object)
})

#' @rdname move-method
#' @export
setMethod("move", signature(object = "circle", target = "numeric"), function(object, target) {
    target <- as(target, "coordinate")

    if (!object@moveable) {
        return(object)
    }

    object@center <- target

    return(object)
})

#'@rdname area-method
#'
setMethod("area", signature(object = "circle"), function(object) pi*object@radius^2)

#' An S4 Class to Represent the Background
#'
#' @slot shape An object of a type that extends \code{\link[predped]{object-class}}
#' defining the shape of the background.
#' @slot objects A list of objects of a type that extends
#' \code{\link[predped]{object-class}} defining the objects in the background.
#'
#' @export
background <- setClass("background", list(shape = "object", objects = "list"))

setMethod("initialize", "background", function(.Object, ...) {
    .Object <- callNextMethod()
    if (!all(sapply(.Object@objects, is, class2 = "object"))) {
        stop("All elements in slot 'objects' must be of type 'object'")
    }
    return(.Object)
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
