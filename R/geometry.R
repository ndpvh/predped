################################################################################
# GENERAL GEOMETRY

# Turns a rectangular object into a specification of its constituent lines
# Output a 2 (x/y) x 4 (lines) x 2 (begin/end point) array
object2lines <- function(o) {
    array(c(o$x[1], o$y[1], o$x[1], o$y[2], o$x[1], o$y[1], o$x[2], o$y[1],
            o$x[2], o$y[1], o$x[2], o$y[2], o$x[1], o$y[2], o$x[2], o$y[2]),  
            dim = c(2, 4, 2), dimnames = list(c("x", "y"), 
                                            c("L1", "L2", "L3", "L4"), 
                                            c("P1", "P2")))
}

# Distance from p1 to p2, same sized xy column matrices
dist <- function(p1, p2) {
    sqrt(apply((p1 - p2)^2, 1, sum))
}

# distance from p1 to p2, p1 is a single point, p2 is a matrix
dist1 <- function(p1, p2) {
    sqrt(apply((t(p2) - as.vector(p1))^2, 2, sum))
}

# Shortest absolute angle between a1 and a2
minAngle <- function(a1,a2) {
    pmin(abs(a1 - a2), abs(pmin(a1, a2) + (360 - pmax(a1, a2))))
}

# Shortest angle anti-clockwise from p1 as orign to p2 (> -180 to 180)
angle2s <- function(p1, p2) {
    round((atan2(p2[, 2] - p1[, 2], p2[, 1] - p1[, 1]) * 180 / pi), 10)
}

# Anti-clockwise angle from p1 as origin to p2 (x,y pairs matrices) 0 to <360
angle2 <- function(p1, p2) {
    round((atan2(p2[, 2] - p1[, 2], p2[, 1] - p1[, 1]) * 180 / pi), 10) %% 360
}

################################################################################
# INTERSECTIONS

##' intersections inside L1 (from P1 to P2) and L2 (from P3 to P4).
##' @return Vector containing x,y coordinates of intersection of L1
##' and L2.  If L1 and L2 are parallel, this is infinite-valued.  If
##' \code{interior.only} is \code{TRUE}, then when the intersection
##' does not occur between P1 and P2 and P3 and P4, a vector
##' containing \code{NA}s is returned.
##' @source Weisstein, Eric W. "Line-Line Intersection."
##' From MathWorld--A Wolfram Web Resource.
##' \url{http://mathworld.wolfram.com/Line-LineIntersection.html}
##' @author David Sterratt
##' @export
##' @examples
##' ## Intersection of two lines
##' line.line.intersection(c(0, 0), c(1, 1), c(0, 1), c(1, 0))
##'
##' ## Two lines that don't intersect
##' line.line.intersection(c(0, 0), c(0, 1), c(1, 0), c(1, 1))
#
# TO DO 
#   - Change this to either C++ code, or to a more general function that holds
#     for more kinds of objects
line.line.intersection <- function(P1, P2, P3, P4, interior.only = FALSE) {
    P1 <- as.vector(P1)
    P2 <- as.vector(P2)
    P3 <- as.vector(P3)
    P4 <- as.vector(P4)
    
    dx1 <- P1[1] - P2[1]
    dx2 <- P3[1] - P4[1]
    dy1 <- P1[2] - P2[2]
    dy2 <- P3[2] - P4[2]
    
    D <- det(rbind(c(dx1, dy1),
                    c(dx2, dy2)))
    if (D==0) {
        return(c(Inf, Inf))
    }
    D1 <- det(rbind(P1, P2))
    D2 <- det(rbind(P3, P4))
    
    X <- det(rbind(c(D1, dx1),
                    c(D2, dx2)))/D
    Y <- det(rbind(c(D1, dy1),
                    c(D2, dy2)))/D
    
    if (interior.only) {
        ## Compute the fractions of L1 and L2 at which the intersection
        ## occurs
        lambda1 <- -((X-P1[1])*dx1 + (Y-P1[2])*dy1)/(dx1^2 + dy1^2)
        lambda2 <- -((X-P3[1])*dx2 + (Y-P3[2])*dy2)/(dx2^2 + dy2^2)
        if (!((lambda1>0) & (lambda1<1) &
            (lambda2>0) & (lambda2<1))) {
        return(c(NA, NA))
        }
    }
    return(c(X, Y))
}

################################################################################
# PREDPED SPECIFIC

#' Compute the centers of the cells
#' 
#' Compute the positions of the centers of the cells the agent might move to 
#' based on the velocity and angle. What happens is that you supply the real-
#' valued position, velocity, and angle for a specific object, and then create
#' discretized cell positions based on either changes in velocity (deceleraration, 
#' continuation, or acceleration) and/or direction.
#' 
#' @param cell The index of the cells to create the cell centers for
#' @param position The current position of the object
#' @param velocity The current velocity of the object
#' @param angle The current direction of the object
#' @param vels All discretized options to change the velocity
#' @param angles All discretized options to change the direction
#' 
#' @return Matrix of positions for the different cells
#' 
#' TO DO
#'  - Make the function simpler: At this moment to many arguments for its own 
#'    good
#'  - Create a test for this function
#' 
#' Replacement of function `c_vd`
compute_cell_centers <- function(cell, 
                                 position, 
                                 velocity, 
                                 angle,
                                 vels = matrix(rep(c(1.5, 1, .5), each = 11), ncol = 3),
                                 angles = matrix(rep(c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 
                                       327.5, 310, 287.5), times = 3), ncol = 3)) {

    # Change in angle and velocity
    changed_angle <- (angle + angles[cell]) %% 360
    changed_velocity <- scale_velocity(velocity) * vels[cell]

    # Compute the anticipated position of the cells
    cell_centers <- position + t(changed_velocity * angle_to_L2(changed_angle))
    return(t(cell_centers))
}

# Scale velocity by the time step. Original function was `scaleVel`
scale_velocity <- function(x, time_step = 0.5) {
    return(x * time_step)  
}

# Angle to L2 normalized carteisn direction, so sqrt(d(x)^2+d(y)^2) = 1. Original
# was  `aTOd`
angle_to_L2 <- function(x) {
    cbind(x = cos(x * pi / 180), y = sin(x * pi / 180))
}

# Which angle cone (1..11, NA means outside of view) is p2 in relative to 
# p1 heading at angel a1 
#
# This function kept original for now, as its scope is not yet clear
Iangle <- function(p1, a1, p2, border = c(-85, -60, -40, -25, -15, -5, 5, 15, 
                                          25, 40, 60, 85)) {
  
    tomp <- function(x) { # -180 ... 180
        neg <- x < -180
        x[neg] <- 360 + x[neg]
        
        pos <- x > 180
        x[pos] <- x[pos] - 360
        
        x
    }
    
    a <- (angle2(p1, p2) - a1)
    
    out <- 12 - .bincode(tomp(a), border)
    names(out) <- row.names(p2)
    out
}

# Compute the absolute angular difference between discretized angles with the 
# cone of comparison being created by `angle_1` and `angle_2`.
#
# Original function: `headingAngle`
angular_difference <- function(angle_1,
                               angle_2, 
                               discretized_angles = c(72.5, 50, 32.5, 20, 10, 0, 350, 
                                                      340, 327.5, 310, 287.5)) {

    # I just copied the places of angle_1 and angle_2 in this function, but 
    # don't think it matters
    return(sapply((discretized_angles + angle_2) %% 360, 
                  \(x) minAngle(x, angle_1)))
}