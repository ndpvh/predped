#' An S4 Class to Represent Table Objects
#'
#' Tables are represented as circles in this class.
#'
#' @slot circles A list of circle objects representing the tables.
#'
#' @export
#' @name Table
Table <- setClass("Table",
                  slots = list(circles = "list"))

#' Initialize a Table object with random properties
#' 
#' This method initializes a Table object with random properties, such as evenly spread out center coordinates and a single randomly selected radius for all circles.
#' 
#' @param .Object The Table object to initialize.
#' @param num_tables The number of tables to create.
#' @param x_range The range of x-coordinates for the center of the tables.
#' @param y_range The range of y-coordinates for the center of the tables.
#' @param ... Additional arguments.
#' 
#' @return An initialized Table object.
#' 
#' @export
#' @name initialize.Table
setMethod("initialize", "Table", function(.Object, num_tables = 1, x_range = c(0, 100), y_range = c(0, 100), ...) {
    circles <- vector("list", length = num_tables)
    radius <- sample(10:20, 1)  # Select a random radius from 10 to 20
    
    # Calculate step size for evenly spaced circles
    # TO DO: Should be changed later on
    step_x <- diff(x_range) / (sqrt(num_tables) + 1)
    step_y <- diff(y_range) / (sqrt(num_tables) + 1)
    
    for (i in 1:num_tables) {
        # Calculate center coordinates for each circle
        center_x <- x_range[1] + (i %% sqrt(num_tables)) * step_x
        center_y <- y_range[1] + (i %/% sqrt(num_tables) + 1) * step_y
        
        circle <- new("circle", center = c(center_x, center_y), radius = radius)
        circles[[i]] <- circle
    }
    .Object@circles <- circles
    return(.Object)
})

#' Plot method for Table objects
#' 
#' This function plots the tables represented by the circles.
#' 
#' @param object A Table object.
#' @param ... Additional arguments to pass to the plot function.
#' 
#' @return A ggplot2 object representing the plot.
#' 
#' @export
#' @name plotTable
setGeneric("plotTable", function(object, ...) {
  standardGeneric("plotTable")
})

setMethod("plotTable", "Table", function(object, ...) {
  require(ggplot2)
  ggplot() +
    geom_point(data = data.frame(x = sapply(object@circles, function(circ) circ@center[1]),
                                 y = sapply(object@circles, function(circ) circ@center[2])),
               aes(x, y), size = object@circles[[1]]@radius * 2) +  # Use the radius of the first circle
    theme_minimal() +
    theme(aspect.ratio = 1)
})


# Example use
tables <- new("Table", num_tables = 20, x_range = c(0, 5), y_range = c(0, 5), radius = sample(1:10, 1))
print(tables)
plotTable(tables)

