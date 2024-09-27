#' Determine iterated values from various arguments
#' 
#' @details
#' Takes in a numeric, numeric vector, or function and transforms this input to 
#' a numeric vector that contains values based on the input. When a numeric is 
#' provided, this function will output this same numeric repeated an
#' \code{iterations} number of times. Similarly, when a numeric vector is 
#' provided, this numeric vector will also be repeated an \code{iterations}
#' number of times. When a function is provided, \code{iterations} will be 
#' provided as an argument of this function.
#' 
#' This function is used to enhance the generalizability of how certain values 
#' are determined, and is used for determining:
#' \itemize{
#'    \item{}{the number of goals to simulate in each goal stack in the 
#'            \code{\link[predped]{multiple_goal_stacks}} function}
#'    \item{}{the counter for each goal in a goal stack in the 
#'            \code{\link[predped]{goal_stack}} function}
#'    \item{}{the number of agents that can maximally be in the simulation at 
#'            each time point in the \code{\link[predped]{simulate-predped-method}}}
#'    \item{}{the iteration number at which an agent can be added to the
#'            simulation in the \code{\link[predped]{simulate-predped-method}}}
#' }
#' 
#' Note that for this function to work, one should correctly define their 
#' own function when provided to the argument \code{x}. Specifically, this 
#' function should take in a single argument \code{n} which defines the number 
#' of values to generate. For example, a typical default for this function is 
#' \code{\(n) rnorm(n, 10, 2)}.
#' 
#' @param x Numeric, numerical vector, or function that should be used to 
#' generate the needed values.
#' @param iterations Integer denoting the number of values that should be 
#' generated.
#' @param positive_integer Logical denoting whether to make all drawn values 
#' positive integers. Defaults to \code{TRUE}, given the use-cases that it is 
#' currently used for.
#' 
#' @return Numeric vector containing values based on the input.
#' 
#' @examples 
#' # Generate several variables to be tested with determine_values
#' number <- 1
#' numeric_vector <- 1:2
#' generating_function <- \(n) runif(n, 0, 10)
#' 
#' # Test them out with 5 iterations
#' determine_values(number, 5)
#' determine_values(numeric_vector, 5)
#' determine_values(generating_function, 5)
#' 
#' @seealso
#' \code{\link[predped]{simulate-predped}}
#' \code{\link[predped]{goal_stack}}
#' \code{\link[predped]{multiple_goal_stacks}}
#' 
#' @rdname determine_values
#' 
#' @export
determine_values <- function(x, 
                             iterations,
                             positive_integer = TRUE) {

    # First check whether the input is a function or a numeric
    if(inherits(x, "function")) {
        values <- x(iterations)

    } else if(is.numeric(x)) {
        values <- rep(x, times = iterations)

    } else {
        stop("Cannot determine the values in `determine_values`: input is not a function or numeric.")
    }

    # If positive values are needed, replace each value lower than 1 with a 1.
    if(positive_integer) {
        values <- ceiling(values)
        values[values <= 1] <- 1
    }

    return(as.integer(values))
}

#' Find all objects of a given class
#' 
#' This function is a simple utility function to extract all objects from a given 
#' class from a list containing several of those.
#' 
#' @param class_name A string with the name of the class
#' @param lst The list in which to search for these objects
#' 
#' @return A list containing all instances of the class.
#' 
#' @export find_class
find_class <- function(class_name, lst) {
    return(Filter(function(x) class_name %in% class(x), lst))
}

#' Find the perpendicular orientation
#' 
#' Finds the perpendicular or tangential orientation of a line starting from a 
#' point on an edge of the background's shape and going inwards. The point on the 
#' edge is chosen to be the entrance to the space. Is used to find in what 
#' orientation the agents should be heading when entering the space.
#' 
#' @param object An object of class object
#' @param co A vector of size 2 containing x and y coordinates for a location 
#' from which to deduce the perpendicular orientation
#' 
#' @return Numeric denoting the perpendicular orientation to the entrance of the 
#' space in degrees
#' 
#' @export
perpendicular_orientation <- function(object, co) {
    # Dispatch based on the shape of the background
    if(inherits(object, "circle")) {
        # If the entrance lies on the circumference of the circle, we can easily
        # derive the angle at which the entrance finds itself relative to the 
        # center of the circle. The perpendicular orientation to this angle is 
        # then the angle + 180 (or angle + pi)
        co <- co - center(object)
        angle <- atan2(co[2], co[1]) + pi

    } else if(inherits(object, "polygon")) {
        # Find out where the entrance lies in the background
        # For this, we compute the sum of the distances between each of 
        # the points that make up an edge and the entrance point. The edge that has 
        # the distance closest to the actual size of the edge will be taken as the 
        # entrance wall.
        points <- object@points
        edges <- cbind(points, points[c(2:nrow(points), 1),]) # Make a 4-columned matrix with (x1, y1) and (x2, y2)

        distances <- cbind(sqrt((edges[,1] - edges[,3])^2 + (edges[,2] - edges[,4])^2),
                           sqrt((edges[,1] - co[1])^2 + (edges[,2] - co[2])^2),
                           sqrt((edges[,3] - co[1])^2 + (edges[,4] - co[2])^2))
        distances <- distances[,2] + distances[,3] - distances[,1]

        # Now that we know on which edge the entrance lies, we can compute the 
        # perpendicular orientation to this edge. Approach uses the first point
        # of the edge as the center of an imaginary circle, which has the second
        # point of the edge on its circumference. We can then use atan2 again to
        # find the angle of the slope in the same way as before. The perpendicular
        # orientation is then defined as the found angle - pi / 2 (when moving
        # clockwise) or + pi / 2 (when moving counterclockwise)
        edge <- as.numeric(edges[which.min(distances),])

        co <- edge[3:4] - edge[1:2]
        angle <- atan2(co[2], co[1]) 

        angle <- ifelse(object@clock_wise, 
                        angle - pi / 2, 
                        angle + pi / 2)
    }
    # Convert to degrees
    angle <- angle * 180 / pi
    names(angle) <- NULL

    # If you have a negative angle, convert this to a positive one
    if(angle < 0) {
        angle <- angle + 360
    }

    # If the angle is equal to 360, change to 0
    if(angle == 360) {
        angle <- 0
    }

    return(angle)
}

#' Compute the line-line intersection between several segments. Is a vectorized 
#' function with minimal loss of time when the number of segments to test 
#' increases.
#'
#' @param segments_1 Matrix with four columns denoting the x- and y-coordinates
#' that make up the line segment. Should be in order x_1, y_1, x_2, y_2.
#' @param segments_2 Matrix of line segments that `segments_1` should be tested
#' with. Should have the same structure as `segments_1`
#' @param return_all Logical denoting whether it should return the intersection 
#' of all segments to each other. If true, will include indicators of which segments
#' were compared. Defaults to `FALSE`.
#'
#' @return Returns a logical denoting whether any of the segments in 
#' 
#' @export
line_line_intersection <- function(segments_1, 
                                   segments_2,
                                   return_all = FALSE) {

    # Check which of the two sequences is shortest. Whichever is shortest will 
    # go into the loop, the other will serve as the vectorized set of segments
    n_1 <- nrow(segments_1)
    n_2 <- nrow(segments_2)
    
    if(n_2 < n_1) {
        looped <- segments_2 
        vectored <- segments_1
    } else {
        looped <- segments_1 
        vectored <- segments_2
    }
    n <- nrow(looped)

    intersection <- matrix(FALSE, nrow = nrow(vectored), ncol = n)
    for(i in seq_len(n)) {
        # Compute the values of t, u and the values they can maximally take t_max, 
        # u_max of the Bézier parametrization of the line segments. If 0 <= t <= t_max
        # and 0 <= u <= u_max, then the intersection between two lines lies within the
        # boundaries that make up that line. 
        t <- (looped[i,1] - vectored[,1]) * (vectored[,2] - vectored[,4]) -
            (looped[i,2] - vectored[,2]) * (vectored[,1] - vectored[,3])
        u <- (looped[i,1] - looped[i,3]) * (looped[i,2] - vectored[,2]) -
            (looped[i,2] - looped[i,4]) * (looped[i,1] - vectored[,1])
        
        t_max <- (looped[i,1] - looped[i,3]) * (vectored[,2] - vectored[,4]) -
            (looped[i,2] - looped[i,4]) * (vectored[,1] - vectored[,3])
        u_max <- -t_max

        # Do the test itself:
        #
        # Important limitation (and TO DO): End points are not regarded as intersecting.
        # This is done because of some weird bugs with parallel lines that are 
        # regarded as intersecting while not intersecting at all.
        t <- sign(t_max) * t
        u <- sign(u_max) * u

        intersection[, i] <- Reduce("&", 
                                    list(0 < t, 
                                         t <= abs(t_max), 
                                         0 < u, 
                                         u <= abs(u_max)))
    }

    if(return_all) {
        # Here, we need to be careful: We want to make sure that the logicals
        # are ordered such that each element in segment_1 is repeated n_2 times
        # and is linked to each element in segment_2. Per example, if segment_1
        # consists of c(1, 2, 3) and segment_2 of c(4, 5), then we want the 
        # resulting matrix to look like cbind(c(1, 1, 2, 2, 3, 3), c(4, 5, 4, 
        # 5, 4, 5).
        #
        # This is automatically the case if the `looped` segments are equal to 
        # `segments_1` and the `vectored` one is equal to `segments_2`. We can 
        # therefore reduce this to a vector and return.
        #
        # If this is not the case, then we first need to transpose and then 
        # transform it to a vector anyway.
        if(n_2 < n_1) {
            intersection <- t(intersection)
        }

        return(as.logical(intersection))
    } else {
        return(any(intersection))
    }
}

#' Raycasting algorithm
#' 
#' This algorithm checks whether a point lies within an arbitrary polygon by
#' checking the even-odd-rule, which says that for any point (x,y) that lies
#' within a polygon, the number of times it cross the boundaries of this
#' polygon when x goes to infinity should be uneven/odd.
#'
#' @details
#' While this may not seem like the most efficient algorithm, it is quite fast 
#' for the typical objects used in predped.
#' 
#' @param coords Numerical matrix of size N x 2 containing the coordinates of 
#' the corners of the object.
#' @param x Numerical matrix of size M x 2 containing the coordinates of the 
#' coordinates of which should be checked whether they are inside of the object.
#' 
#' @return Logical vector denoting whether each point in \code{x} lies within
#' (\code{TRUE}) or outside (\code{FALSE}) of the object.
#' 
#' @examples
#' # Create a set of points that come from a rectangle
#' my_rectangle <- rectangle(center = c(0, 0), size = c(2, 2))
#' points <- my_rectangle@points 
#' 
#' # Create a set of points that fall inside of or outside of the rectangle
#' coords <- rbind(c(0, 0), 
#'                 c(2, 0))
#' 
#' # Check where they lie with the raycasting algorithm
#' raycasting(points, coords)
#' 
#' @seealso 
#' \code{\link[predped]{in_object}},
#' \code{\link[predped]{out_object}}
#' 
#' @rdname raycasting
#' 
#' @export
raycasting <- function(coords, x) {
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

    # Return TRUE whenever the counter is uneven, as this indicates that the 
    # point is inside of the object
    return(!(counter %% 2 == 0))
}

# Vectorized version of seq, used in `overlap_with_objects`
multi_seq <- Vectorize(seq.default, vectorize.args = c("from", "to", "by", "length.out"))
