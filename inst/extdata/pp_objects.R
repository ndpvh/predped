#######################################################
# PURPOSE: Defines several geometric objects that     #
#          can be used to create an environment       #
#######################################################

# TO DO
#   1) Implement ortientation and movability of objects
#   2) Create environment for our experiment

#' Rotate points
#' 
#' @description \code{rotate} rotates a point around a given 
#' center.
#' 
#' @param x numerical vector with the x and y coordinates to 
#' rotate
#' @param degrees the degrees
#' @param center numerical vector with the x and y coordinates
#' around which to rotate the point. Defaults to the origin.
#' 
#' @export 
rotate <- function(x, degrees, center = c(0, 0)) {
    # Change coordinates to move around the origin
    x_origin <- x - center

    # Transform the degrees to radians
    radians <- degrees * pi / 180

    # Rotate the point to a new position
    M <- matrix(c(cos(radians), - sin(radians), 
                  sin(radians), cos(radians)),
                nrow = 2, 
                ncol = 2)
    y_origin <- (M %*% x_origin) |>
        as.numeric()

    # Change coordinates back to the original center
    return( y_origin + center )
}

# Degrees to radians
# Radians to degrees


#' Create a rectangle
#' 
#' @description \code{rectangle} creates a rectangle that can be used or
#' interacted with in the environment. This code makes use 
#' of a reference class which allows the user to use several
#' methods for rectangle, all of which will be specified at 
#' a later point.
#' 
#' @param size numerical vector with the width and height of 
#' the rectangle
#' @param coordinate numerical vector with the x and y 
#' coordinates of the center of the rectangle
#' @param orientation the orientation of the rectangle in 
#' degrees. Here, 0 degrees indicate that the is completely
#' aligned with the x-axis, while 90 degrees woudl indicate
#' alignment with the y-axis.
#' 
#' @method area
#' @method corner_coordinates 
#' 
#' @export
rectangle <- setRefClass("rectangle", 
                         fields = list(size = "numeric",
                                       coordinate = "numeric",
                                       orientation = "numeric"),
                         methods = list(corner_coordinates = function() {
                                            # x and y coordinates
                                            x <- coordinate[1]
                                            y <- coordinate[2]
                                            
                                            # Get half the widths and heights
                                            w <- size[1] / 2
                                            h <- size[2] / 2

                                            # Rotate the points around the center


                                            return( rbind( c(x - width, y - height), 
                                                           c(x - width, y + height),
                                                           c(x + width, y - height),
                                                           c(x + width, y + height) ) )
                                        }))
