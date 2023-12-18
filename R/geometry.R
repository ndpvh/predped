# Turns an rectangular object into a specification of its constituent lines
# Output a 2 (x/y) x 4 (lines) x 2 (begin/end point) array
object2lines <- function(o) {
  array(c(o$x[1], o$y[1], o$x[1], o$y[2], o$x[1], o$y[1], o$x[2], o$y[1],
          o$x[2], o$y[1], o$x[2], o$y[2], o$x[1], o$y[2], o$x[2], o$y[2]),  
        dim = c(2, 4, 2), dimnames = list(c("x", "y"), 
                                          c("L1", "L2", "L3", "L4"), 
                                          c("P1", "P2")))
}

# Calcualte cell centres for set of cells (index 1..33) for p1 heading at 
# velocity v1 at angle a1 given time step tStep seconds.

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
