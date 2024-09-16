
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

#' Rotate Multiple Points Around Center
#'
#' @param object A numeric vector of length two with the point coordinates.
#' @param degrees A single numeric element indicating the degrees of rotation.
#' @param center A numerical vector of length two with the x and y coordinates
#' around which to rotate the point. Defaults to the origin (0, 0).
#'
#' @export
setMethod("rotate",
          signature(object = "matrix"),
          function(object, radians, center = c(0, 0)) {
              # Check whether the matrix has the correct dimensionality
              if((nrow(object) != 2) & (ncol(object) != 2)) {
                  stop("Point matrix should have x- and y-coordinates: Neither rows nor columns have 2 values.")
              }

              # Transpose the matrix if necessary (2 x N)
              if(nrow(object) != 2) {
                  transpose <- TRUE 
                  object <- t(object)
              } else {
                  transpose <- FALSE
              }

              # Change coordinates to move around the origin
              rep_center <- matrix(rep(center, times = ncol(object)), 
                                   nrow = 2)
              x_origin <- object - rep_center

              # Rotate the point to a new position
              M <- matrix(c(cos(radians), -sin(radians),
                            sin(radians), cos(radians)),
                          nrow = 2,
                          ncol = 2,
                          byrow = TRUE)

              y_origin <- as.numeric(M %*% x_origin)

              # Change coordinates back to the original center
              y <- y_origin + rep_center

              # If the matrix of rotated points should be transposed, do so. 
              # Otherwise return as is.
              if(transpose) {
                  y <- t(y)
              }
              return(y)
          })

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
#' @rdname object-class
#' @family objects
#' @seealso \code{\link[predped]{rectangle-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{circle-class}}
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
#' @param x A numeric vector or matrix with x and y coordinates of the point.
#' @param outside A logical indicating whether to return \code{TRUE} if the point
#' is outside the object.
#'
#' @return Logical whether the point is inside the object
#' (if \code{outside} is \code{FALSE}) or outside.
#' @export
#' @name in_object-method
setGeneric("in_object", function(object, x, outside = TRUE) standardGeneric("in_object"))

#' Make an Object Larger
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#' @param extension Numeric denoting the length with which to extend the object 
#' in all directions.
#'
#' @return Enlarged object
#' 
#' @export
#' @name enlarge-method
setGeneric("enlarge", function(object, extension) standardGeneric("enlarge"))

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

#' Add Nodes on the Circumference of an Object
#'
#' @param object An object 
#' @param space_between Numeric denoting the amount of space that needs to be 
#' left between the edge of the object and the node. Defaults to `5e-2`
#' 
#' @return  Matrix with points along the edges of the object
#' @export 
#' @name nodes_on_circumference-method
setGeneric("nodes_on_circumference", function(object, ...) standardGeneric("nodes_on_circumference"))

#' Check whether an Object intersects with Other Object
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#' @param other_object Another object of type that extends \code{\link[predped]{object-class}}
#' with which `object` is to be tested with.
#'
#' @return Logical denoting whether the objects intersect
#' @export
#' @name intersects-method
setGeneric("intersects", function(object, other_object) standardGeneric("intersects"))

#' Check whether an Object intersects with Line Segments
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#' @param segments Matrix of size N x 4 containing the coordinates of the line 
#' segments in order x_1, y_1, x_2, y_2.
#'
#' @return Logical denoting whether the segments intersect with the object
#' @export
#' @name line_intersection-method
setGeneric("line_intersection", function(object, segments, ...) standardGeneric("line_intersection"))

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
#' @rdname polygon-class
#' @family objects
#' @seealso \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{circle-class}}
#'
#' @export
polygon <- setClass("polygon", list(points = "matrix", clock_wise = "logical", center = "numeric"), contains = "object")

setMethod("initialize", "polygon", function(.Object, 
                                            id = character(0),
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

    range_x <- range(points[,1])
    range_y <- range(points[,2])
    .Object@center <- coordinate(c(min(range_x) + 0.5 * diff(range_x), 
                                   min(range_y) + 0.5 * diff(range_y)))

    return(.Object)
})

#'@rdname in_object-method
#'
setMethod("in_object", signature(object = "polygon"), function(object, x, outside = TRUE) {
    # If x is not a matrix, make it one. This will allow us to use `in_object`
    # in a vectorized manner (taking in a matrix of coordinates)
    if(is.data.frame(x)) {
        x <- as.matrix(x)
    }

    if(!is.matrix(x)) {
        x <- matrix(x, ncol = 2)
    }

    # Use the raycasting algorithm to determine whether the points in x are 
    # contained in the polygon.
    return(raycasting(object@points, x, outside = outside))
})

#'@rdname enlarge-method
#'
setMethod("enlarge", signature(object = "polygon"), function(object, extension) {
    # Find the nodes of the polygon and use these new nodes as the points of 
    # the enlarged polygon.
    points(object) <- add_nodes(object, space_between = extension, only_corners = TRUE)
    return(object)
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
    edges <- cbind(object@points, object@points[c(2:nrow(object@points), 1), ])
    edges <- rbind(edges, edges[1,])

    # Loop over the edges and do the necessary calculations. Then bind together
    # all nodes into a matrix and check which ones fall inside (or outside) of 
    # the object (to be deleted)
    nodes <- lapply(seq_len(nrow(edges) - 1), 
                    \(x) find_location(edges[x,], edges[x + 1,]))
    nodes <- do.call("rbind", nodes)

    idx <- in_object(object, nodes, outside = outside)
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

#'@rdname nodes_on_circumference-method
#'
setMethod("nodes_on_circumference", signature(object = "polygon"), function(object, 
                                                                            space_between = 5e-2) {

    corners <- object@points 
    n <- nrow(corners)

    x_changes <- cbind(corners[,1], corners[c(2:n, 1), 1])
    y_changes <- cbind(corners[,2], corners[c(2:n, 1), 2])

    len_x <- ceiling(abs((x_changes[,2] - x_changes[,1]) / space_between))
    len_y <- ceiling(abs((y_changes[,2] - y_changes[,1]) / space_between))

    len <- matrixStats::rowMaxs(cbind(len_x, len_y))

    nodes <- cbind(as.numeric(unlist(multi_seq(x_changes[,1], 
                                               x_changes[,2],
                                               length.out = len))),
                   as.numeric(unlist(multi_seq(y_changes[,1], 
                                               y_changes[,2],
                                               length.out = len))))

    return(nodes)
})

#'@rdname intersects-method
#'
setMethod("intersects", signature(object = "polygon"), function(object, other_object) {
    
    # Dispath based on the type of the other object
    if(inherits(other_object, "circle")) {
        return(intersects(other_object, object))
    } else if(inherits(other_object, "segment")) {
        return(intersects(other_object, object))        
    } else {
        # Extract the points of the objects and create the edges to be 
        # evaluated
        edges_1 <- cbind(object@points, object@points[c(2:nrow(object@points), 1), ])
        edges_2 <- cbind(other_object@points, other_object@points[c(2:nrow(other_object@points), 1), ])

        # Use the line_line_intersection function
        return(line_line_intersection(edges_1, edges_2))
    }
})

#'@rdname line_intersection-method
#'
setMethod("line_intersection", signature(object = "polygon"), function(object, segments, return_all = FALSE) {
    
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
#' @rdname rectangle-class
#' @family objects
#'
#' @export
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
                                              id = character(0),
                                              clock_wise = TRUE,
                                              orientation = 0,
                                              moveable = FALSE,
                                              interactable = FALSE,
                                              degrees = FALSE,
                                              ...) {

    if (length(size) != 2) stop("Size vector must have length two (x and y)")
    if (any(size <= 0)) stop("Size vector must be positive")
    if (length(orientation) != 1) stop("Orientation must be a single element")

    .Object@center <- as(center, "coordinate")

    size_half <- size/2

    lower <- center - size_half
    upper <- center + size_half

    points <- matrix(c(lower, c(lower[1], upper[2]), upper, c(upper[1], lower[2])), 4, 2, byrow = TRUE)

    if(degrees) {
        orientation <- orientation * pi / 180
    }

    if (orientation != 0) {
        points <- t(apply(points, 1, rotate, radians = orientation, center = center))
    }

    if(!clock_wise) {
        points <- points[4:1,]
    }

    .Object <- callNextMethod(.Object, points = points, clock_wise = clock_wise)

    .Object@id <- if(length(id) == 0) paste("object", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id
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
#' @rdname rotate-method
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
    # If x is not a matrix, make it one. This will allow us to use `in_object`
    # in a vectorized manner (taking in a matrix of coordinates)
    if(is.data.frame(x)) {
        x <- as.matrix(x)
    }
    
    if(!is.matrix(x)) {
        x <- matrix(x, ncol = 2)
    }

    # Use the raycasting algorithm to determine whether the points in x are 
    # contained in the rectangle. Has been shown to be more efficient than doing the 
    # proper calculations for rectangle. For legacy reasons, still kept commented
    # in.
    return(raycasting(object@points, x, outside = outside))

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

#'@rdname enlarge-method
#'
setMethod("enlarge", signature(object = "rectangle"), function(object, extension) {
    # Extend the size of the rectangle with the factor sqrt{space_between^2 / 2}, 
    # as we do in the `add_nodes` function.
    size(object) <- size(object) + 2 * sqrt(extension^2 / 2)
    return(object)
})

#'@rdname add_nodes-method
#'
setMethod("add_nodes", signature(object = "rectangle"), function(object, 
                                                                 space_between = 0.5,
                                                                 only_corners = FALSE,
                                                                 outside = TRUE) {

    # Approach will be to make a new rectangle that is greater than the original
    # one by a given factor and then taking its points as the new nodes. If we
    # want `space_between` space between the corners of the old rectangle and 
    # the corners of the new one, we will have to create an `extension` factor
    # based on the rule of Pythagoras, where we know that c^2 = `space_between`^2
    # and a^2 = b^2 = `extension`^2.
    extension <- sqrt(space_between^2 / 2)

    # Use the setter for the size of a rectangle to change its size. This setter
    # will automatically change the corner points inside of the rectangle.
    #
    # Importantly, the size of the rectangle changes with 2 times the extension
    size(object) <- size(object) + ifelse(outside, 2, -2) * extension 
    
    # Return the nodes as is when you only want nodes to be created at the 
    # corners of the polygon
    if(only_corners) {
        return(object@points)
    } 

    # We need to add some additional spaces so that there is `space_between` 
    # amount of space between each of the now created nodes. For this, we will 
    # use logic that is the same as for polygons.
    nodes <- object@points
    edges <- cbind(nodes, nodes[c(2:nrow(nodes), 1),])

    # Define the number of times each of the lines defined by the edges should 
    # be divided by
    sizes <- sqrt((edges[,1] - edges[,3])^2 + (edges[,2] - edges[,4])^2)

    # Delete those sizes that are smaller than space_between
    idx <- sizes > space_between 
    sizes <- sizes[idx]
    edges <- edges[idx,]

    # Get the spaces that should be left inbetween the next points on the grid
    number_points <- ceiling(sizes / space_between)
    distances <- sizes / number_points

    # Find the points on the line that correspond to each of the distances 
    # and bind them to the `nodes` matrix. In order for this to work, we 
    # should add the distances from a starting point to the end point by using
    # the angle of the slope with a radius equal to the distance from the 
    # starting point to the end point. The starting point is always defined 
    # as that node for which x is minimal, except when the line drawn is 
    # vertical, in which case the y-coordinate matters
    # slopes <- (edges[,2] - edges[,4]) / (edges[,1] - edges[,3])
    slopes <- (edges[,4] - edges[,2]) / (edges[,3] - edges[,1])
    angles <- atan(slopes)

    # In the last step, we loop over these values to create the new nodes
    new_nodes <- list()
    for(i in seq_len(nrow(nodes))) {
        if(slopes[i] == 0) {
            idx <- c(1, 3)[which.min(edges[i, c(1, 3)])]
        } else {
            idx <- 1
        }

        new_nodes[[i]] <- cbind(1:(number_points[i] - 1) * cos(angles[i]) * distances[i] + edges[i, idx],
                                1:(number_points[i] - 1) * sin(angles[i]) * distances[i] + edges[i, idx + 1])
    }
    nodes <- rbind(nodes, do.call("rbind", new_nodes))

    return(nodes)
})

#'@rdname intersects-method
#'
setMethod("intersects", signature(object = "rectangle"), function(object, other_object) {
    # Dispath based on the type of the other object. If circle or polygon, then 
    # we switch the two objects and dispatch to the `intersects` method of these
    # two classes
    if(inherits(other_object, "circle")) {
        return(intersects(other_object, object))
    } else if(inherits(other_object, "segment")) {
        return(intersects(other_object, object))        
    }else {        
        # Extract the points of the objects and create the edges to be 
        # evaluated
        edges_1 <- cbind(object@points, object@points[c(2:nrow(object@points), 1), ])
        edges_2 <- cbind(other_object@points, other_object@points[c(2:nrow(other_object@points), 1), ])

        # Use the line_line_intersection function
        return(line_line_intersection(edges_1, edges_2))
    }    
})

#' An S4 Class to Represent Circle Objects
#'
#' @slot center A numeric vector of length two indicating the center of the circle.
#' @slot radius A numeric indicating the radius of the circle.
#'
#' @rdname circle-class
#' @family objects
#' @seealso \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{rectangle-class}}, 
#' \code{\link[predped]{polygon-class}}
#' 
#' @export
circle <- setClass("circle", list(center = "numeric", radius = "numeric"), contains = c("object"))

setMethod("initialize", "circle", function(.Object, 
                                           id = character(0), 
                                           moveable = FALSE, 
                                           interactable = FALSE, 
                                           ...) {

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

#'@rdname in_object-method
#'
setMethod("in_object", signature(object = "circle"), function(object, x, outside = TRUE) {
    # If x is not a matrix, make it one. This will allow us to use `in_object`
    # in a vectorized manner (taking in a matrix of coordinates)
    if(is.data.frame(x)) {
        x <- as.matrix(x)
    }
    
    if(!is.matrix(x)) {
        x <- matrix(x, ncol = 2)
    }

    # Compute the distance between the coordinate and the center of the circle.
    # If this distance is smaller than the radius, the point is within the 
    # circle.
    y <- center(object)
    check <- (x[,1] - y[1])^2 + (x[,2] - y[2])^2 < radius(object)^2
    
    if(outside) {
        check <- !check
    }
    return(check)
})

#'@rdname enlarge-method
#'
setMethod("enlarge", signature(object = "circle"), function(object, extension) {
    # Extend the radius with extension.
    radius(object) <- radius(object) + extension
    return(object)
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

#'@rdname nodes_on_circumference-method
#'
setMethod("nodes_on_circumference", signature(object = "circle"), function(object, 
                                                                           space_between = 5e-2) {

    return(nodes <- points(object, length.out = ceiling(2 * pi * radius(object) / space_between)))
})

#'@rdname intersects-method
#'
setMethod("intersects", signature(object = "circle"), function(object, other_object) {
    # Dispath based on the type of the other object. If circle or polygon, then 
    # we switch the two objects and dispatch to the `intersects` method of these
    # two classes
    if(inherits(other_object, "circle")) {
        # This case is rather easy, as we just need to determine whether the 
        # distance between the centers of the circles is smaller or bigger than 
        # the sum of their radii. 
        #
        # However, to ensure that a circle can also be contained within the 
        # other circle, we also have to ensure that the distance between the 
        # centers is bigger than the difference between the radii
        distance <- m4ma::dist1(center(object), 
                                matrix(center(other_object), 
                                       ncol = 2))

        return((distance <= radius(object) + radius(other_object)) & 
               (distance >= abs(radius(object) - radius(other_object))))

    } else if(inherits(other_object, "segment")) {
        return(intersects(other_object, object))

    } else {
        # Add nodes to the other object
        other <- add_nodes(other_object, space_between = 1e-2)
        return(any(in_object(object, other, outside = FALSE)))

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
    }  
})

#'@rdname line_intersection-method
#'
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
    idx <- in_object(object, coords, outside = FALSE)
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

#' An S4 class to Represent Lines
#'
#' Lines are used to determine intersections and to prevent pedestrians from 
#' walking certain ways.
#'
#' @slot id Character denoting the id of the segment
#' @slot from Numeric vector denoting where the segment starts
#' @slot to Numeric vector denoting where the segment stops
#' @slot center Numeric vector denoting the center of the segment
#' @slot orientation Numeric denoting the orientation of the segment in radians
#' @slot interactable Logical denoting whether the segment can be interacted 
#' with. Defaults to `FALSE`.
#'
#' @export
#' @name segment
segment <- setClass("segment", list(id = "character", 
                                    from = "numeric", 
                                    to = "numeric", 
                                    center = "numeric",
                                    orientation = "numeric",
                                    interactable = "logical"), contains = "object")

setMethod("initialize", "segment", function(.Object, 
                                            from, 
                                            to, 
                                            id = character(0), 
                                            interactable = FALSE,
                                            ...) {

    .Object@id <- if(length(id) == 0) paste("segment", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id
    .Object@from <- from
    .Object@to <- to
    .Object@interactable <- interactable

    .Object@center <- 0.5 * c(to[1] - from[1], to[2] - from[2])
    .Object@orientation <- atan2(to[2] - from[2], to[1] - from[1])

    return(.Object)
})

#'@rdname in_object-method
#'
setMethod("in_object", signature(object = "segment"), function(object, x, outside = TRUE) {
    # For a line, it does not matter whether a point is contained within the line.
    # Therefore always return FALSE
    return(FALSE)
})

#'@rdname rng_point-method
#'
setMethod("rng_point", signature(object = "segment"), function(object,
                                                               middle_edge = TRUE) {

    if(middle_edge) {
        return(as.numeric(center(object)))
    } else {
        a <- runif(1, 0, 1)
        return(a * c(object@to[1] - object@from[1], object@to[2] - object@from[2]))
    }
})   

#'@rdname add_nodes-method
#'
setMethod("add_nodes", signature(object = "segment"), function(object, 
                                                               ...) {
    
    # Should not add nodes to a line
    return(NULL)
})

#'@rdname intersects-method
#'
setMethod("intersects", signature(object = "segment"), function(object, other_object) {
    # Dispath based on the type of the other object. If circle or polygon, then 
    # we switch the two objects and dispatch to the `intersects` method of these
    # two classes
    if(inherits(other_object, "circle")) {
        # Similar steps to the polygon one, but now for a single line

        # Adjust the from and to so that the circle is centered at the origin
        from <- object@from - center(circle)
        to <- object@to - center(circle)

        # First check: Do any of the points that determine the segment fall 
        # within the circle? If so, there is an intersection.
        if((from[1]^2 + from[2]^2 <= radius(circle)^2) | 
           (to[1]^2 + to[2]^2 <= radius(circle)^2)) {
            return(TRUE)
        }

        # Second check: Use the formula for an intersection of a line with a 
        # circle to determine whether the line itself intersects. For this to 
        # work, we need to offset the points of the edges with the center of 
        # the circle.
        #
        # Once we found that there is an intersection, we can compute the point 
        # at which the intersection happens and check whether it lies within the
        # two provided points that make up the edge. If not, then there is no 
        # actual intersection. 
        dx <- to[1] - from[1]
        dy <- to[2] - from[2]
        distance <- dx^2 + dy^2

        D <- from[1] * to[2] - from[2] * to[1]

        discriminant <- radius(object)^2 * distance - D^2

        # Check whether the line is at risk of intersecting. If not, 
        # we can safely say that the circle does not intersect with the polygon
        if(discriminant >= 0) {
            return(FALSE)
        }

        # If not, compute the different intersection points with the circle and 
        # check whether these lie inbetween the segments of the polygon
        co <- c(D * dy + sign(dy) * dx * sqrt(discriminant),
                -D * dx + abs(dy) * sqrt(discriminant),
                D * dy - sign(dy) * dx * sqrt(discriminant),
                -D * dx - abs(dy) * sqrt(discriminant)) / distance

        x_range <- range(c(from[1], to[1]))
        y_range <- range(c(from[2], to[2]))

        x_check_1 <- (co[,1] <= x_range[2]) & (co[,1] >= x_range[1])
        y_check_1 <- (co[,2] <= y_range[2]) & (co[,2] >= y_range[1])
        x_check_2 <- (co[,3] <= x_range[2]) & (co[,3] >= x_range[1])
        y_check_2 <- (co[,4] <= y_range[2]) & (co[,4] >= y_range[1])

        return(any((x_check_1 & y_check_1) | (x_check_2 & y_check_2)))

    } else if(inherits(other_object, "segment")) {
        # This case can be handed to m4ma
        return(m4ma::line.line.intersection(object@from,
                                            object@to, 
                                            other_object@from,
                                            other_object@to,
                                            interior.only = TRUE))

    } else {
        # Here, we will loop over all points that make up the edges of the polygon
        # or rectangle and check whether they intersect with the 
        # line.line.intersection function
        edges <- cbind(other_object@points, 
                       other_object@points[c(2:nrow(points), 1),])
        return(line_line_intersection(matrix(c(object@from, object@to), nrow = 1),
                                      edges))

        return(any(idx)) 
    }  
})





################################################################################
# Getters and setters

#' Getter/Setter for the id-slot
#' 
#' @rdname id-method
#' 
#' @export 
setGeneric("id", function(object) standardGeneric("id"))

#' @rdname id-method
#' 
#' @export 
setGeneric("id<-", function(object, value) standardGeneric("id<-"))

setMethod("id", signature(object = "object"), function(object) {
    return(object@id)
})

setMethod("id<-", signature(object = "object"), function(object, value) {
    object@id <- value
    return(object)
})

#' Getter/Setter for the radius-slot
#' 
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

#' Getter/Setter for the size-slot
#' 
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
    points <- 0.5 * rbind(c(value[1], value[2]),
                          c(value[1], -value[2]),
                          c(-value[1], -value[2]),
                          c(-value[1], value[2]))
    
    alpha <- object@orientation
    R <- matrix(c(cos(alpha), sin(alpha), -sin(alpha), cos(alpha)), nrow = 2, ncol = 2)

    points <- R %*% t(points)
    object@points <- cbind(points[1,] + center(object)[1], 
                           points[2,] + center(object)[2])
    return(object)
})

setMethod("size", signature(object = "circle"), function(object) {
    return(object@radius)
})

setMethod("size<-", signature(object = "circle"), function(object, value) {
    object@radius <- value
    return(object)
})

#' Getter/Setter for the center-slot
#' 
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
    object@points <- cbind(object@points[,1] + value[1] - center(object)[1], 
                           object@points[,2] + value[2] - center(object)[2])
    object@center <- as(value, "coordinate")
    return(object)
})

setMethod("center", signature(object = "rectangle"), function(object) {
    return(object@center)
})

setMethod("center<-", signature(object = "rectangle"), function(object, value) {
    object@center <- as(value, "coordinate")
    object@points <- cbind(object@points[,1] + value[1],
                           object@points[,2] + value[2])
    return(object)
})

setMethod("center", signature(object = "circle"), function(object) {
    return(object@center)
})

setMethod("center<-", signature(object = "circle"), function(object, value) {
    object@center <- as(value, "coordinate")
    return(object)
})

setMethod("center", signature(object = "segment"), function(object) {
    return(object@center)
})

setMethod("center<-", signature(object = "segment"), function(object, value) {
    object@center <- value
    object@from <- object@from + value 
    object@to <- object@to + value
    return(object)
})

#' Getter/Setter for the orientation-slot
#' 
#' @rdname orientation-method
#' 
#' @export 
setGeneric("orientation", function(object) standardGeneric("orientation"))

#' @rdname orientation-method
#' 
#' @export 
setGeneric("orientation<-", function(object, value) standardGeneric("orientation<-"))

setMethod("orientation", signature(object = "rectangle"), function(object) {
    return(object@orientation)
})

setMethod("orientation<-", signature(object = "rectangle"), function(object, value) {
    original_value <- object@orientation
    object@orientation <- value

    rect <- rotate(object, radians = value - original_value)
    object@points <- rect@points
    return(object)
})

setMethod("orientation", signature(object = "segment"), function(object) {
    return(object@orientation)
})

setMethod("orientation<-", signature(object = "segment"), function(object, value) {
    angle <- value - object@orientation
    object@orientation <- value   

    R <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), nrow = 2, ncol = 2)
    object@from <- R %*% object@from
    object@to <- R %*% object@to

    return(object)
})

#' Getter/Setter for the points-slot
#' 
#' @rdname points-method
#' 
#' @export 
setGeneric("points", function(object, ...) standardGeneric("points"))

#' @rdname points-method
#' 
#' @export 
setGeneric("points<-", function(object, value) standardGeneric("points<-"))

setMethod("points", signature(object = "rectangle"), function(object, ...) {
    pts <- object@points 
    dimnames(pts) <- NULL
    return(pts)
})

setMethod("points", signature(object = "polygon"), function(object, ...) {
    return(object@points)
})

setMethod("points<-", signature(object = "polygon"), function(object, value) {
    object@points <- value 
    return(object)
})

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

setMethod("points", signature(object = "segment"), function(object, ...) {
    return(matrix(c(object@from, object@to), nrow = 2, ncol = 2))
})

setMethod("points<-", signature(object = "segment"), function(object, value) {
    object@from <- value[1,]
    object@to <- value[2,]

    return(object)
})

#' Getter/Setter for the from-slot
#' 
#' @rdname from-method
#' 
#' @export 
setGeneric("from", function(object, ...) standardGeneric("from"))

#' @rdname from-method
#' 
#' @export 
setGeneric("from<-", function(object, value) standardGeneric("from<-"))

setMethod("from", signature(object = "segment"), function(object, ...) {
    return(object@from)
})

setMethod("from<-", signature(object = "segment"), function(object, value) {
    object@from <- value
    return(object)
})

#' Getter/Setter for the to-slot
#' 
#' @rdname to-method
#' 
#' @export 
setGeneric("to", function(object, ...) standardGeneric("to"))

#' @rdname to-method
#' 
#' @export 
setGeneric("to<-", function(object, value) standardGeneric("to<-"))

setMethod("to", signature(object = "segment"), function(object, ...) {
    return(object@to)
})

setMethod("to<-", signature(object = "segment"), function(object, value) {
    object@to <- value
    return(object)
})






################################################################################
# Additional utility functions

#' Raycasting algorithm
#' 
#' This algorithm checks whether a point lies within an arbitrary polygon by
#' checking the even-odd-rule, which says that for any point (x,y) that lies
#' within a polygon, the number of times it cross the boundaries of this
#' polygon when x goes to infinity should be uneven/odd.
#'
#' This is not the most efficient algorithm, but it might be good to have
#' as the default, but to specify other algorithms when possible (e.g., for
#' rectangle, see below).
#' 
#' @param coords Matrix of size n x 2 containing the coordinates of the corners
#' of the object within which the points may or may not lie
#' @param x Matrix of size m x 2 containing the coordinates of the points that 
#' should be checked
#' @param outside A logical indicating whether to return \code{TRUE} if the point
#' is outside the object.
#' 
#' @return Logical vector denoting whether each point in `x` lies within or 
#' outside of the object
#' 
#' @export
raycasting <- function(coords, x, outside = TRUE) {
    # Create the edges of the object based on its corner coordinates
    edges <- cbind(coords, coords[c(2:nrow(coords), 1),])

    # Enlongen the two matrices of segments so that the intersection of each 
    # segment within the two matrices can be compared to each other. For this, 
    # take the Kronecker product with a vector of ones
    n_1 <- nrow(edges)
    n_2 <- nrow(x)

    edges <- edges %x% rep(1, each = n_2)
    x <- rep(1, each = n_1) %x% x

    # Check whether the y-coordinate of the points are above the y-coordinates 
    # of the segments that make up the edges 
    check_1 <- (edges[,2] > x[,2]) != (edges[,4] > x[,2])

    # Use a derived formula to find out for which value of the x coordinate
    # the imaginary horizontal line through the point `x` would intersect
    # with the edge. This derivation is based on deriving the equation
    # y = mx + b for the edge and then equation it to the equation y = x[2],
    # which represents the horizontal move from x[2] to infinity.
    slope <- (edges[,3] - edges[,1]) / (edges[,4] - edges[,2])
    x_intersection <- edges[,1] + slope * (x[,2] - edges[,2])

    # Finally, check whether this intersection point lies further to the
    # right (towards infinity) than the initial value of the x coordinate.
    # If so, then the intersection indeed happens due to the move from the
    # x coordinate to infinity.
    check_2 <- x[,1] < x_intersection

    # If both checks are TRUE, then there is an intersection. Determine how often
    # this occurs in the data
    counter <- matrix(check_1 & check_2, 
                      nrow = n_2, 
                      ncol = n_1) |>
        rowSums()

    # If outside == FALSE, return TRUE if you have an odd number of intersections.
    # If outside == TRUE, return TRUE if you have an even number of intersections.
    # otherwise
    #
    # In other words, return TRUE whenever the two booleans match (TRUE, TRUE ->
    # even number of intersections when outside of the polygon; FALSE, FALSE ->
    # odd number of intersections when inside of the polygon)
    return((counter %% 2 == 0) == outside)
}