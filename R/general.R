params_archetypes <- read.csv(file.path("archetypes.csv"))

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

#' Separate agent and object
#' 
#' This function extracts agents and objects from a single `state` list.
#' 
#' @param state A list containing the current state of the simulation
#' 
#' @return Two lists containing the objects or the agents
#' 
#' @export separate_agent_object
separate_agent_object <- function(state) {
    # Identify the agents from the `state` list
    agents <- find_class("agent", state)

    # Identify the objects and delete the agents from this list
    objects <- find_class("object", state) 
    objects[[objects %in% agents]] <- NULL 

    return(list("agents" = agents, 
                "objects" = objects))
}

# Create a singular function for the unpacking for a given part of the list
unpack_list <- function(index){
    # Extract the wanted variable from the list across all agents. Then, 
    # transpose the result and give the result row names
    transposed <- apply(state, 2, function(x) x[[index]]) |>
        t()
    row.names(transposed) <- names
    return(transposed)        
}

# Undocumented function because this is in no way a particularly beautiful 
# function, nor is it meant to be the final way in which we do this.
#
# It takes in the background as an object and computes the orientation the 
# agent needs to walk in in order to walk perpendicular to the entrance they 
# just came from.
#
# @param background Object of the background class.
perpendicular_orientation <- function(background) {
    # Dispatch based on the shape of the background
    if(class(shape(background)) == "circle") {
        # If the entrance lies on the circumference of the circle, we can easily
        # derive the angle at which the entrance finds itself relative to the 
        # center of the circle. The perpendicular orientation to this angle is 
        # then the angle + 180 (or angle + pi)
        co <- entrance(background)
        angle <- atan2(co[1], co[2]) + pi
        angle <- angle * 180 / pi
    } else {
        # Find out where the entrance lies in the background
        # For this, we compute the sum of the distances between each of 
        # the points that make up an edge and the entrance point. The edge that has 
        # the distance closest to the actual size of the edge will be taken as the 
        # entrance wall.
        co <- entrance(background)@.Data
        points <- shape(background)@points
        points <- cbind(points, points[c(2:nrow(points), 1),]) # Make a 4-columned matrix with (x1, y1) and (x2, y2)

        distances <- cbind(sqrt((points[,1] - points[,3])^2 + (points[,2] - points[,4])^2),
                        sqrt((points[,1] - co[1])^2 + (points[,2] - co[2])^2),
                        sqrt((points[,3] - co[1])^2 + (points[,4] - co[2])^2))
        distances <- distances[,1] - distances[,2] - distances[,3]

        # Now that we know on which edge the entrance lies, we can compute the 
        # perpendicular orientation to this edge. Approach computes the orientation 
        # of the line defined by the entrance and the edge of the wall that contains 
        # it and then either subtracts (clockwise == TRUE) or adds (clockwise == FALSE)
        # 90 degrees from it. The formula to do this is simply tan^{-1} (slope), 
        # where slope is the slope of the edge
        edge <- as.numeric(points[which.min(distances),])
        slope <- (edge[2] - edge[4]) / (edge[1] - edge[3])

        angle <- atan(slope) * 180 / pi     # Conversion from radians to degrees
        if(background@shape@clock_wise) {
            angle <- angle - 90
        } else {
            angle <- angle + 90
        }
    }

    # If you have a negative angle, convert this to a positive one
    if(angle < 0) {
        angle <- angle + 360
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

    # Enlongen the two matrices of segments so that the intersection of each 
    # segment within the two matrices can be compared to each other. For this, 
    # take the Kronecker product with a vector of ones
    n_1 <- nrow(segments_1)
    n_2 <- nrow(segments_2)

    segments_1 <- segments_1 %x% rep(1, each = n_2)
    segments_2 <- rep(1, each = n_1) %x% segments_2

    # Compute the values of t, u and the values they can maximally take t_max, 
    # u_max of the Bézier parametrization of the line segments. If 0 <= t <= t_max
    # and 0 <= u <= u_max, then the intersection between two lines lies within the
    # boundaries that make up that line. 
    t <- (segments_1[,1] - segments_2[,1]) * (segments_2[,2] - segments_2[,4]) -
        (segments_1[,2] - segments_2[,2]) * (segments_2[,1] - segments_2[,3])
    u <- (segments_1[,1] - segments_1[,3]) * (segments_1[,2] - segments_2[,2]) -
        (segments_1[,2] - segments_1[,4]) * (segments_1[,1] - segments_2[,1])
    
    t_max <- (segments_1[,1] - segments_1[,3]) * (segments_2[,2] - segments_2[,4]) -
        (segments_1[,2] - segments_1[,4]) * (segments_2[,1] - segments_2[,3])
    u_max <- -t_max

    # Do the test itself:
    #
    # Important limitation (and TO DO): End points are not regarded as intersecting.
    # This is done because of some weird bugs with parallel lines that are 
    # regarded as intersecting while not intersecting at all.
    t <- sign(t_max) * t
    u <- sign(u_max) * u

    intersection <- (0 < t) & (t <= abs(t_max)) & (0 < u) & (u <= abs(u_max))

    if(return_all) {
        return(intersection)
    } else {
        return(any(intersection))
    }
}
