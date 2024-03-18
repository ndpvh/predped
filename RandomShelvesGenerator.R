#' An S4 class representing the distribution and coordinates of shelves in a supermarket.
#' @slot num_columns Number of columns in the supermarket.
#' @slot num_rows Number of rows in the supermarket.
#' @slot total_shelves Total number of shelves in the supermarket.
#' @slot shelf_distribution Distribution of shelves among columns and rows.
#' @slot rectangles Coordinates of the shelves.
#' @name Shelves
#' @export
#' 
Shelves <- setClass("Shelves", slots = list(num_columns = "numeric",
                                            num_rows = "numeric",
                                            total_shelves = "numeric",
                                            shelf_distribution = "matrix",
                                            rectangles = "list"))

setMethod("initialize", "Shelves", function(.Object, num_columns = NULL, num_rows = NULL, total_shelves = NULL, shelf_distribution = NULL, ...) {
    # Check whether shelf_distribution is already provided
    if (!is.null(shelf_distribution)) {
        num_columns <- ncol(shelf_distribution)
        num_rows <- nrow(shelf_distribution)
        total_shelves <- sum(shelf_distribution)
        warning("You have provided a distribution for the shelves. The number of columns, number of rows, and the number of shelves are adjusted based on the distribution.")
    } else {
        # Generate random num_columns & num_rows if not provided
        if (is.null(num_columns)) {
            num_columns <- sample(1:10, 1) # Random columns between 1-10
        }
        if (is.null(num_rows)) {
            num_rows <- sample(1:10, 1) # Random rows between 1-10
        }
        # Generate random shelves between 1 and num_columns * num_rows
        # TO-DO: Check if it is robust enough
        if (is.null(total_shelves)) {
            total_shelves <- sample(1:(num_columns * num_rows), 1) 
        }
    }

    if (is.null(shelf_distribution)) {
        # Initialize the distribution
        shelf_distribution <- matrix(0, nrow = num_rows, ncol = num_columns)

        # Randomly fill the shelves to each slot
        shelves_remaining <- total_shelves
        while (shelves_remaining > 0) {
            row_index <- sample(1:num_rows, 1)
            col_index <- sample(1:num_columns, 1)
            shelf_distribution[row_index, col_index] <- shelf_distribution[row_index, col_index] + 1
            shelves_remaining <- shelves_remaining - 1
        }
        
        # Adjust the number of columns and rows if there are empty columns or rows
        # TO-DO: Warnings are still triggered even the arguments are not provided by user
        # This should not be triggered otherwise
        empty_cols <- which(colSums(shelf_distribution) == 0)
        empty_rows <- which(rowSums(shelf_distribution) == 0)
        
        if (length(empty_cols) > 0) {
            num_columns <- num_columns - length(empty_cols)
            shelf_distribution <- shelf_distribution[, colSums(shelf_distribution) != 0]
            warning(paste(length(empty_cols), "empty column(s) detected and omitted."))
        }
        if (length(empty_rows) > 0) {
            num_rows <- num_rows - length(empty_rows)
            shelf_distribution <- shelf_distribution[rowSums(shelf_distribution) != 0, ]
            warning(paste(length(empty_rows), "empty row(s) detected and omitted."))
        }
    }

    # Assign values to object slots
    .Object@num_columns <- num_columns
    .Object@num_rows <- num_rows
    .Object@total_shelves <- total_shelves
    .Object@shelf_distribution <- shelf_distribution
    .Object@rectangles <- list()
    
    return(.Object)
})

#' Retrieve coordinates of rectangles.
#'
#' This function generates coordinates of rectangles representing shelves within the Shelves object.
#'
#' TO-DO: Now it thinks the 2 shelves in the same slot has the same coordinates
#' TO-DO: Fix this by adjusting the length and of the rectangle?
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
setGeneric("getCoordinates", function(object, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5, ...) {
    standardGeneric("getCoordinates")
})

setMethod("getCoordinates", "Shelves", function(object, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5, ...) {
    shelf_rectangles <- vector("list", length = sum(object@shelf_distribution))
    index <- 1
    for (row in 1:object@num_rows) {
        for (col in 1:object@num_columns) {
            if (object@shelf_distribution[row, col] > 0) {
                for (shelf in 1:object@shelf_distribution[row, col]) {
                    shelf_center <- c((col - 1) * (shelf_width + aisle_width) + shelf_width / 2,
                                      (row - 1) * (shelf_length + aisle_width) + (shelf - 0.5) * shelf_length)
                    shelf_rectangles[[index]] <- new("rectangle", center = shelf_center, size = c(shelf_width, shelf_length))
                    index <- index + 1
                }
            }
        }
    }
    object@rectangles <- shelf_rectangles  # Update rectangles slot with generated coordinates
    return(object)
})


#' Plot method for Shelves objects
#' This method is only to visualize the changes in the code.
#' TO-DO: 
#' 
#' @param x A Shelves object.
#' @param shelf_length Length of each shelf.
#' @param shelf_width Width of each shelf.
#' @param aisle_width Width of the aisle between shelves.
#' @param aisle_color Color of the aisle.
#' @param shelf_color Color of the shelves.
#' @param ... Additional arguments passed to geom_rect.
#' 
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

# Example use
shelves <- new("Shelves")
print(shelves)
shelves_coordinates <- getCoordinates(shelves, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5)
plotShelves(shelves_coordinates)

shelves_2 <- new("Shelves", num_columns = 3, num_rows = 3, total_shelves = 2)
print(shelves_2)


# Example use
shelf_distribution <- matrix(c(2, 2, 1,
                               2, 1, 1,
                               3, 3, 4), nrow = 3, byrow = TRUE)

shelves <- new("Shelves", shelf_distribution = shelf_distribution)
shelves_coordinates <- getCoordinates(shelves, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5)
plotShelves(shelves_coordinates)
