
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
#' @param object A circle parameters
#' @return  Matrix with points necessary to draw the circle
#' @export 
#' @name to_polygon-method
setGeneric("to_polygon", function(object, ...) standardGeneric("to_polygon"))

#' Add Nodes along an Object
#'
#' @param object An object 
#' @param space_between Numeric denoting the amount of space that needs to be 
#' left between the edge of the object and the node. Defaults to `0.5`
#' 
#' @return  Matrix with points along the edges of the object
#' @export 
#' @name add_nodes-method
setGeneric("add_nodes", function(object, ...) standardGeneric("add_nodes"))

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
polygon <- setClass("polygon", list(points = "matrix", clock_wise = "logical", center = "numeric"), contains = "object")

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

    points <- .Object@points
    .Object@center <- coordinate(c(mean(points[,1]), mean(points[,2])))

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

#'@rdname add_nodes-method
#'
# Important; assumes that the intersection point between the two edges is on 
# the third and fourth column of `edge_1` or on the first and second column of 
# `edge_2`.
setMethod("add_nodes", signature(object = "polygon"), function(object, 
                                                               space_between = 0.5) {
    
    # Create a local function that will take in two coordinates and will return
    # the location of the new coordinate
    find_location <- function(edge_1, edge_2) {
        # Compute the slopes created by the two lines
        slope_1 <- (edge_1[2] - edge_1[4]) / (edge_1[1] - edge_1[3])
        slope_2 <- (edge_2[2] - edge_2[4]) / (edge_2[1] - edge_2[3])

        # Compute the angle between the edges. For this, we need to check 
        # whether the slopes are finite or note
        if(is.infinite(slope_1)) {
            angle <- atan(slope_2) + pi / 2
        } else if(is.infinite(slope_2)) {
            angle <- atan(slope_1) - pi / 2
        } else {
            angle <- atan((slope_1 - slope_2) / (1 + slope_1 * slope_2))
        }

        # To have a node that is perpendicular to this intersection between lines,
        # we will need to half this angle and compute a new slope
        angle <- angle / 2

        # Get the orienation of edge_1 based on its slope
        edge_angle <- atan(slope_1)

        # Add the half angle to this, and create two points on the perpendicular
        # line created by the point of interest and the angle
        angle <- edge_angle + angle
        point <- rbind(edge_2[1:2] + c(cos(angle), sin(angle)) * space_between,
                       edge_2[1:2] + c(cos(angle + pi), sin(angle + pi)) * space_between)

        return(point)
    }

    # Create the edges as needed by the `find_location` function. Important to 
    # note that we need to rbind the first edge to the matrix in order to find 
    # the node for the final point in the matrix.
    edges <- cbind(object@points, object@points[c(2:nrow(object@points), 1), ])
    edges <- rbind(edges, edges[1,])

    # Loop over the edges and do the necessary calculations
    nodes <- matrix(0, nrow = nrow(edges) * 2, 2)
    for(i in seq_len(nrow(edges) - 1)) {
        print(i)
        print(find_location(edges[i,], edges[i + 1,]))
        nodes[(i - 1) * 2 + 1:2,] <- find_location(edges[i,], edges[i + 1,])
    }

    return(nodes)
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

#'@rdname add_nodes-method
#'
setMethod("add_nodes", signature(object = "rectangle"), function(object, 
                                                                 space_between = 0.5) {

    # Approach will be to make a new rectangle that is greater than the original
    # one by a given factor and then taking its points as the new nodes. If we
    # want `space_between` space between the corners of the old rectangle and 
    # the corners of the new one, we will have to create an `extension` factor
    # based on the rule of Pythagoras, where we know that c^2 = `space_between`^2
    # and a^2 = b^2 = `extension`^2.
    extension = sqrt(space_between^2 / 2)
    
    # Make the new rectangle and extract its points. Importantly, we need to 
    # use 2 * `extension`, as we have two edges that need extending.
    rect <- rectangle(center = object@center, 
                      orientation = object@orientation,
                      size = 2 * extension + object@size)

    return(rect@points)
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
setMethod("to_polygon", signature(object = "circle"), function(object, length.out = 100, ...) {
    t <- seq(0, 2 * pi, length.out = length.out)
    cp <- as.matrix(data.frame(
        x = object@center[[1]] + object@radius * cos(t),
        y = object@center[[2]] + object@radius * sin(t)
    ))
    return(cp)
})

#'@rdname in_object-method
#'
setMethod("in_object", signature(object = "circle"), function(object, x, outside = TRUE) {
    # Compute the distance between the coordinate and the center of the circle.
    # If this distance is smaller than the radius, the point is within the 
    # circle.
    y <- center(object)
    dist <- sqrt((x[1] - y[1])^2 + (x[2] - y[2]))

    check <- dist < radius(object)
    return(ifelse(outside, !check, check))
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

#'@rdname add_nodes-method
#'
setMethod("add_nodes", signature(object = "circle"), function(object, 
                                                              space_between = 0.5) {
    
    # Create the angles at which to put the nodes around the circle
    angles <- seq(0, 2 * pi, pi / 4)

    # Create a matrix of locations based on the center of the object, the radius,
    # and the drawn angles and return. Importantly, radius is extended with a
    # number `space_between` so that some space is left between the object and 
    # the path point
    adjusted_radius <- radius(object) + space_between
    return(cbind(center(object)[1] + cos(angles) * adjusted_radius, 
                 center(object)[2] + sin(angles) * adjusted_radius))
})



# Getters and setters

#' @rdname radius-method
#' 
#' @export 
setGeneric("radius", function(object) standardGeneric("radius"))

#' @rdname radius-method
#' 
#' @export 
setGeneric("radius<-", function(object, value) standardGeneric("radius<-"))

setMethod("radius", signature(object = "circle"), function(object) {
    return(object@radius)
})

setMethod("radius<-", signature(object = "circle"), function(object, value) {
    object@radius <- value
    return(object)
})

#' @rdname size-method
#' 
#' @export 
setGeneric("size", function(object) standardGeneric("size"))

#' @rdname size-method
#' 
#' @export 
setGeneric("size<-", function(object, value) standardGeneric("size<-"))

setMethod("size", signature(object = "rectangle"), function(object) {
    return(object@size)
})

setMethod("size<-", signature(object = "rectangle"), function(object, value) {
    object@size <- value
    return(object)
})

setMethod("size", signature(object = "circle"), function(object) {
    return(object@radius)
})

setMethod("size<-", signature(object = "circle"), function(object, value) {
    object@radius <- value
    return(object)
})

#' @rdname center-method
#' 
#' @export 
setGeneric("center", function(object) standardGeneric("center"))

#' @rdname center-method
#' 
#' @export 
setGeneric("center<-", function(object, value) standardGeneric("center<-"))

setMethod("center", signature(object = "polygon"), function(object) {
    return(object@center)
})

setMethod("center<-", signature(object = "polygon"), function(object, value) {
    object@center <- value
    return(object)
})

setMethod("center", signature(object = "rectangle"), function(object) {
    return(object@center)
})

setMethod("center<-", signature(object = "rectangle"), function(object, value) {
    object@center <- value
    return(object)
})

setMethod("center", signature(object = "circle"), function(object) {
    return(object@center)
})

setMethod("center<-", signature(object = "circle"), function(object, value) {
    object@center <- value
    return(object)
})