
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

#' Sample a Random Point on the Circumference
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#' @param middle_edge Logical denoting whether the point should lie in the middle
#' of a random edge. Ignored for circles. Defaults to `TRUE`.
#' @param forbidden Numeric indicating forbidden values. For `polygon` and 
#' `rectangle`, this is the edge number on which the point should not be 
#' generated. For `circle`, this is either a vector or a matrix of angles between 
#' which the point should not be sampled (in radians). For the latter, it is 
#' important to note that the intervals created by the angles should not overlap.
#' Defaults to `NULL`, or the point can be sampled anywhere.
#'
#' @return Numerical coordinate of a point on the circumference of the object
#' @export
#' @name rng_point-method
setGeneric("rng_point", function(object, middle_edge = TRUE, forbidden = NULL) standardGeneric("rng_point"))

#' Convert a Cirlce with a Center and Radius to a Polygon
#'
#' @param object An object that contains circle paramters
#' @return  Matrix with points necessary to draw the circle
#' @export 
#' @name to_polygon-method
setGeneric("to_polygon", function(object, ...) standardGeneric("to_polygon"))

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

setMethod("initialize", "polygon", function(.Object, 
                                            id = NULL,
                                            clock_wise = TRUE, 
                                            moveable = FALSE, 
                                            interactable = FALSE,
                                            ...) {
    .Object <- callNextMethod(.Object, ...)

    if (ncol(.Object@points) != 2) {
        stop("All points must have an x- and y-coordinate (two-column matrix)")
    }

    .Object@id <- if(length(id) == 0) paste("object", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id
    .Object@clock_wise <- clock_wise
    .Object@moveable <- moveable
    .Object@interactable <- interactable

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

#'@rdname rng_point-method
#'
setMethod("rng_point", signature(object = "polygon"), function(object, 
                                                               middle_edge = TRUE,
                                                               forbidden = NULL) {
    
    # Sample the edge on which to draw a random point. To make our lives easier, 
    # we first transform `points` so that it contains the values of the 
    # first coordinate (x_1, y_1) in the first two columns and the values of the
    # second coordiante (x_2, y_2) in the last two columns. Each edges is then 
    # defined in each row.
    #
    # Importantly, the forbidden edges should be deleted from the options.
    edges <- cbind(object@points, object@points[c(2:nrow(object@points), 1),])
    if(!is.null(forbidden)) {
        edges <- edges[-forbidden,]

        # Extra check if you delete all except one edge
        if(!is.matrix(edges)) {
            edges <- matrix(edges, ncol = 4)
        }
    }

    idx <- sample(seq_len(nrow(edges)), 1)

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
#
# TO DO
#   - Currently, orientation is in radians. Might need to change to degrees, as 
#     everything else is in degrees
rectangle <- setClass("rectangle", list(
        center = "numeric",
        size = "numeric",
        orientation = "numeric"
    ),
    contains = c("polygon")
)

setMethod("initialize", "rectangle", function(.Object,
                                              center,
                                              size,
                                              id = NULL,
                                              clock_wise = TRUE,
                                              orientation = 0,
                                              moveable = FALSE,
                                              interactable = FALSE,
                                              ...
) {
    if (length(size) != 2) stop("Size vector must have length two (x and y)")
    if (any(size <= 0)) stop("Size vector must be positive")
    if (length(orientation) != 1) stop("Orientation must be a single element")

    .Object@id <- if(length(id) == 0) paste("object", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id
    .Object@center <- as(center, "coordinate")

    size_half <- size/2

    lower <- center - size_half
    upper <- center + size_half

    points <- matrix(c(lower, c(lower[1], upper[2]), upper, c(upper[1], lower[2])), 4, 2, byrow = TRUE)

    if (orientation != 0) {
        points <- t(apply(points, 1, rotate, radians = orientation, center = center))
    }

    if(!clock_wise) {
        points <- points[4:1,]
    }

    .Object <- callNextMethod(.Object, points = points, clock_wise = clock_wise)

    .Object@size <- size
    .Object@moveable <- moveable
    .Object@orientation <- orientation
    .Object@interactable <- interactable

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

setMethod("initialize", "circle", function(.Object, id = NULL, moveable = FALSE, interactable = FALSE, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@id <- if(length(id) == 0) paste("object", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id
    .Object@center <- as(.Object@center, "coordinate")
    .Object@moveable <- moveable
    .Object@interactable <- interactable
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

#' @rdname to_polygon-method
#' @export 
setMethod("to_polygon", signature(object = "circle"), function(object, ...) {
    t <- seq(0, 2 * pi, length.out = 100)
    cp <- as.matrix(data.frame(
        x = object@center[[1]] + object@radius * cos(t),
        y = object@center[[2]] + object@radius * sin(t)
    ))
    return(cp)
})

#'@rdname rng_point-method
#'
setMethod("rng_point", signature(object = "circle"), function(object, 
                                                              middle_edge = TRUE,
                                                              forbidden = NULL) {

    # First check which intervals are allowed to be sampled and add a column 
    # that contains the relative weight of the interval to be sampled in
    if(!is.null(forbidden)) {
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