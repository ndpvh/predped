#' An S4 class representing the distribution and coordinates of shelves in a supermarket.
#' @slot num_columns Number of columns in the supermarket.
#' @slot total_shelves Total number of shelves in the supermarket.
#' @slot shelf_distribution Distribution of shelves among columns.
#' @slot rectangles Coordinates of the shelves.
#' @name Shelves
#' @export
#' 
Shelves <- setClass("Shelves", slots = list(num_columns = "numeric",
                                            total_shelves = "numeric",
                                            shelf_distribution = "numeric",
                                            rectangles = "list"))

setMethod("initialize", "Shelves", function(.Object, num_columns = NULL, total_shelves = NULL, shelf_distribution = NULL, ...) {
    # Check if shelf_distribution is provided
    if (!is.null(shelf_distribution)) {
        num_columns <- length(shelf_distribution)
        total_shelves <- sum(shelf_distribution)
        warning("You have provided a distribution for the shelves. The number of columns and the number of shelves are adjusted based on the distribution.")
    } else {
        # Generate random values if shelf_distribution is not provided
        if (is.null(num_columns)) {
            num_columns <- sample(1:10, 1)  # Random number of columns between 1-10
        }
        if (is.null(total_shelves)) {
            total_shelves <- max(sample(1:20, 1), num_columns)  # Random number of shelves between 1-20
        }
    }

    # Make sure num_columns does not exceed total_shelves
    if (!is.null(num_columns) && num_columns > total_shelves) {
        warning("Number of columns cannot exceed the number of shelves. Adjusting the column number.")
        num_columns <- total_shelves
    }

    if (is.null(shelf_distribution)) {
        # Initialize the distribution
        shelf_distribution <- rep(0, num_columns)

        # Make sure each column has at least one shelf
        for (i in 1:min(total_shelves, num_columns)) {
            shelf_distribution[i] <- 1 
        }

        # Update & distribute the remaining shelves
        remaining_shelves <- total_shelves - sum(shelf_distribution)
        while (remaining_shelves > 0) {
            col_index <- sample(1:num_columns, 1) 
            shelf_distribution[col_index] <- shelf_distribution[col_index] + 1
            remaining_shelves <- remaining_shelves - 1
        }
    }

    # Initialize the list for rectangles
    rectangles <- list()

    # Assign values to object slots
    .Object@num_columns <- num_columns
    .Object@total_shelves <- sum(shelf_distribution)
    .Object@shelf_distribution <- shelf_distribution
    .Object@rectangles <- rectangles 
    
    return(.Object)
})

#' Retrieve coordinates of rectangles.
#'
#' This function generates coordinates of rectangles representing shelves within the Shelves object.
#'
#' @param object A Shelves object containing information about shelf distribution.
#' @param shelf_length Length of each shelf.
#' @param shelf_width Width of each shelf.
#' @param aisle_width Width of the aisle between shelves.
#' @param ... Additional arguments.
#' 
#' @return The Shelves object with updated rectangles slot containing coordinates of shelves.
#' 
#' @export
#' @name getCoordinates
#' 
setGeneric("getCoordinates",
           function(object, ...) {
               standardGeneric("getCoordinates")
           })

#'@rdname getCoordinates-method
#' 
#' #' Example usage
#' shelves <- new("Shelves", num_columns = 5, total_shelves = 18)
#' shelves_coordinates <- getCoordinates(shelves, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5)
#' 
setMethod("getCoordinates", "Shelves", function(object, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5, ...) {
    shelf_rectangles <- vector("list", length = sum(object@shelf_distribution))
    index <- 1
    for (col in 1:object@num_columns) {
        for (shelf in 1:object@shelf_distribution[col]) {
            shelf_center <- c((col - 1) * (shelf_width + aisle_width) + shelf_width / 2, shelf)
            shelf_rectangles[[index]] <- new("rectangle", center = shelf_center, size = c(shelf_width, shelf_length))
            index <- index + 1
        }
    }
    object@rectangles <- shelf_rectangles  # Update rectangles slot with generated coordinates
    return(object)
})

#' Plot method for Shelves objects
#' This method is only to visualize the changes in the code.
#' TO-DO: Add a method to visualize the rows.
#' 
#' @param x A Shelves object.
#' @param shelf_length Length of each shelf.
#' @param shelf_width Width of each shelf.
#' @param aisle_width Width of the aisle between shelves.
#' @param aisle_color Color of the aisle.
#' @param shelf_color Color of the shelves.
#' @param ... Additional arguments passed to geom_rect.
#' 
#' Example usage
#'plotShelves(shelves_coordinates)
#'
plotShelves <- function(x, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5, 
                        aisle_color = "white", shelf_color = "grey", ...) {
  
  # Load required libraries
  require(ggplot2)
  
  # Create data frame for shelves
  shelves_df <- data.frame(
    x = unlist(lapply(x@rectangles, function(rect) rect@center[1] - shelf_width / 2)),
    y = unlist(lapply(x@rectangles, function(rect) rect@center[2] - shelf_length / 2)),
    width = shelf_width,
    height = shelf_length
  )
  
  # Create data frame for aisles
  aisles_df <- data.frame(
    x = seq(0, max(shelves_df$x) + aisle_width, by = aisle_width),
    y = 0,
    width = aisle_width,
    height = max(shelves_df$y) + shelf_length
  )
  
  # Plot
  ggplot() +
    geom_rect(data = aisles_df, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), 
              fill = aisle_color, color = NA) +
    geom_rect(data = shelves_df, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), 
              fill = shelf_color, color = "black", ...) +
    coord_fixed() +
    theme_void()
}
