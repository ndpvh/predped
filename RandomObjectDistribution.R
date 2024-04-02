#' An S4 class representing the distribution and coordinates of random objects in a supermarket.
#' @slot num_columns Number of columns in the supermarket.
#' @slot num_rows Number of rows in the supermarket.
#' @slot total_objects Total number of objects in the supermarket.
#' @slot object_distribution Distribution of objects among columns and rows.
#' @slot objects List containing coordinates of the objects.
#' @slot fill_slots Allowing empty slots in object distribution.
#' @slot object_shape Shape of the objects ('rectangles', 'circles', or 'polygons').
#' @name RandomObjectDistribution
#' @export
#' 
#' Example use of RandomObjectDistribution class for rectangles.
#'
#' rect_distribution <- new("RandomObjectDistribution",
#'                          num_columns = 5,
#'                          num_rows = 4,
#'                          total_objects = 15,
#'                          fill_slots = TRUE,
#'                          object_shape = "rectangle")
#' 
RandomObjectDistribution <- setClass("RandomObjectDistribution", slots = list(
  num_columns = "numeric",
  num_rows = "numeric",
  total_objects = "numeric",
  object_distribution = "matrix",
  objects = "list",
  fill_slots = "logical",
  object_shape = "character"
))

setMethod("initialize", "RandomObjectDistribution", function(.Object, num_columns = NULL, num_rows = NULL, total_objects = NULL, object_distribution = NULL, fill_slots = FALSE, object_shape = NULL, ...) {
  if (is.null(object_shape) || !(object_shape %in% c("rectangle", "circle", "polygon"))) {
    stop("object_shape must be specified and should be one of 'rectangle', 'circle', or 'polygon'.")
  }

  if (is.null(object_distribution)) {
    # Generate random distribution if not provided
    if (is.null(num_columns)) {
      num_columns <- sample(1:10, 1) # Random columns between 1-10
    }
    if (is.null(num_rows)) {
      num_rows <- sample(1:10, 1) # Random rows between 1-10
    }
    # Generate random number of objects between 1 and num_columns * num_rows
    if (is.null(total_objects)) {
      total_objects <- sample(1:(num_columns * num_rows), 1) 
    }
    # Initialize object_distribution matrix
    object_distribution <- matrix(0, nrow = num_rows, ncol = num_columns)
    
    # Fill the distribution randomly
    objects_remaining <- total_objects
    while (objects_remaining > 0) {
      row_index <- sample(1:num_rows, 1)
      col_index <- sample(1:num_columns, 1)
      object_distribution[row_index, col_index] <- object_distribution[row_index, col_index] + 1
      objects_remaining <- objects_remaining - 1
    }
    
    # Adjust number of columns and rows if there are empty columns or rows
    empty_cols <- which(colSums(object_distribution) == 0)
    empty_rows <- which(rowSums(object_distribution) == 0) 
    
    if (!is.null(empty_rows) && length(empty_rows) > 0) {
      num_rows <- num_rows - length(empty_rows)
      object_distribution <- object_distribution[rowSums(object_distribution) != 0, ]  
      warning(paste(length(empty_rows), "empty row(s) detected and omitted."))
    }
    if (!is.null(empty_cols) && !anyNA(empty_cols) && length(empty_cols) > 0 && nrow(object_distribution) > 0) {
      num_columns <- num_columns - length(empty_cols)
      object_distribution <- object_distribution[, colSums(object_distribution) != 0]
      warning(paste(length(empty_cols), "empty column(s) detected and omitted."))
    }
    
    # Fill the slots if specified
    if (fill_slots) {
      empty_slots_count <- sum(object_distribution == 0)
      if (empty_slots_count > 0) {
        object_distribution[object_distribution == 0] <- 1
        total_objects <- total_objects + empty_slots_count
        warning(paste("Empty slots are filled with one object each. There are", empty_slots_count, "additional objects."))
      }
    }
  }

  # Ensure object_distribution is a matrix
  object_distribution <- as.matrix(object_distribution)

  # Assign values to object slots
  .Object@num_columns <- num_columns
  .Object@num_rows <- num_rows
  .Object@total_objects <- total_objects
  .Object@object_distribution <- object_distribution
  .Object@objects <- list()
  .Object@fill_slots <- fill_slots
  .Object@object_shape <- object_shape

  return(.Object)
})

#' Retrieve coordinates of objects based on the object shape.
#'
#' This function generates coordinates of objects (rectangles or circles) based on the specified object shape
#' within the RandomObjectDistribution object.
#' 
#' @param object A RandomObjectDistribution object containing information about object distribution.
#' @param shelf_length Length of each shelf (for rectangles).
#' @param shelf_width Width of each shelf (for rectangles).
#' @param radius Radius of each circle (for circles).
#' @param aisle_width Width of the aisle between objects.
#' @param ... Additional arguments.
#' 
#' @return The RandomObjectDistribution object with updated objects slot containing coordinates of objects.
#' 
#' @export
#' @name getCoordinates
#' 
#' Example Use of getCoordinates method for rectangles and circles.
#' 
#' rect_coords <- getCoordinates(rect_distribution, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5)
#' circle_coords <- getCoordinates(circle_distribution, radius = 1, aisle_width = 1.5)
#' 
setGeneric("getCoordinates", function(object, shelf_length = 1, shelf_width = 0.5, radius = 1, aisle_width = 1.5, ...) {
    standardGeneric("getCoordinates")
})

setMethod("getCoordinates", "RandomObjectDistribution", function(object, shelf_length = 1, shelf_width = 0.5, radius = 1, aisle_width = 1.5, ...) {
    if (object@object_shape == "rectangle") {
        rectangles <- vector("list", length = sum(object@object_distribution))
        index <- 1
        row_y_start <- rep(0, object@num_rows)  # Initialize starting y-coordinate for each row

        for (row in 1:object@num_rows) {
            max_nr_per_row <- max(object@object_distribution[row, ])
            if (max_nr_per_row > 0) {
                if (row > 1) {
                    # Calculate the starting y-coordinate for each subsequent row
                    row_y_start[row] <- row_y_start[row - 1] + (max(object@object_distribution[row - 1, ]) * shelf_length) + aisle_width
                }
                for (col in 1:object@num_columns) {
                    num_objects <- object@object_distribution[row, col]
                    if (num_objects > 0) {
                        col_x <- (col - 1) * (shelf_width + aisle_width) + shelf_width / 2
                        for (obj in 1:num_objects) {
                            obj_center <- c(col_x, row_y_start[row] + (obj - 0.5) * shelf_length)
                            rectangles[[index]] <- new("rectangle", center = obj_center, size = c(shelf_width, shelf_length))
                            index <- index + 1
                        }
                    }
                }
            }
        }
        object@objects <- rectangles
    } else if (object@object_shape == "circle") {
        circles <- vector("list", length = sum(object@object_distribution))
        index <- 1
        row_y_start <- rep(0, object@num_rows)  # Initialize starting y-coordinate for each row

        for (row in 1:object@num_rows) {
            max_nr_per_row <- max(object@object_distribution[row, ])
            if (max_nr_per_row > 0) {
                if (row > 1) {
                    # Calculate the starting y-coordinate for each subsequent row
                    row_y_start[row] <- row_y_start[row - 1] + (max(object@object_distribution[row - 1, ]) * (2 * radius)) + aisle_width
                }
                for (col in 1:object@num_columns) {
                    num_objects <- object@object_distribution[row, col]
                    if (num_objects > 0) {
                        col_x <- (col - 1) * (2 * radius + aisle_width) + radius
                        for (obj in 1:num_objects) {
                            obj_center <- c(col_x, row_y_start[row] + (obj - 0.5) * (2 * radius))
                            circles[[index]] <- new("circle", center = obj_center, radius = radius)
                            index <- index + 1
                        }
                    }
                }
            }
        }
        object@objects <- circles
    } else {
        stop("Unsupported object shape. Supported shapes are 'rectangle' and 'circle', and 'polygon'.")
    }

    return(object)
})

#' Plot method for RandomObjectDistribution objects
#' 
#' @param x A RandomObjectDistribution object.
#' @param shelf_length Length of each shelf (for rectangles).
#' @param shelf_width Width of each shelf (for rectangles).
#' @param radius Radius of each circle (for circles).
#' @param aisle_width Width of the aisle between objects.
#' @param aisle_color Color of the aisle.
#' @param object_color Color of the objects (rectangles or circles).
#' @param ... Additional arguments passed to geom_polygon.
#' 
#' @export
#' 
#' Example Use of plotDistribution function for rectangles and circles.
#' 
#' plotDistribution(rect_coords, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5)
#' plotDistribution(circle_coords, radius = 1, aisle_width = 1.5)
#' 
setGeneric("plotDistribution", function(x, ...) {
  standardGeneric("plotDistribution")
})

plotDistribution <- function(x, shelf_length = 1, shelf_width = 0.5, radius = 1, aisle_width = 1.5, 
                             aisle_color = "white", object_color = "grey", ...) {
  
  # Load required libraries
  require(ggplot2)
  
  if (x@object_shape == "rectangle") {
    # Create data frame for shelves
    shelves_df <- data.frame(
      x = unlist(lapply(x@objects, function(rect) rect@center[1] - shelf_width / 2)),
      y = unlist(lapply(x@objects, function(rect) rect@center[2] - shelf_length / 2)),
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
    
    # Plot rectangles using the plot method for rectangles
    rectangle_plots <- lapply(x@objects, function(rect) plot(rect, ...))
    p <- ggplot() + 
      geom_blank() +
      geom_rect(data = aisles_df, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), 
                fill = aisle_color, color = NA) +
      rectangle_plots +
      coord_fixed() +
      theme_void()
    
  } else if (x@object_shape == "circle") {
    # Create data frame for circles
    circles_df <- data.frame(
      x = unlist(lapply(x@objects, function(circle) circle@center[1])),
      y = unlist(lapply(x@objects, function(circle) circle@center[2])),
      radius = rep(radius, length(x@objects))
    )
    
    # Create data frame for aisles
    aisles_df <- data.frame(
      x = seq(0, max(circles_df$x) + 2 * radius + aisle_width, by = aisle_width),
      y = 0,
      width = aisle_width,
      height = max(circles_df$y) + 2 * radius
    )
    
    # Plot circles using the plot method for circles
    circle_plots <- lapply(x@objects, function(circle) plot(circle, ...))
    p <- ggplot() + 
      geom_blank() +
      circle_plots +
      coord_fixed() +
      theme_void()
  } else {
    stop("Unsupported object shape. Supported shapes are 'rectangle' and 'circle'.")
  }
  
  return(p)
}

# Example use for rectangles
rect_distribution <- new("RandomObjectDistribution",
                         num_columns = 5,
                         num_rows = 4,
                         total_objects = 15,
                         fill_slots = TRUE,
                         object_shape = "rectangle")

rect_coords <- getCoordinates(rect_distribution, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5)
plotDistribution(rect_coords, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5)

# Example use for circles
circle_distribution <- new("RandomObjectDistribution",
                           num_columns = 6,
                           num_rows = 4,
                           total_objects = 20,
                           fill_slots = TRUE,
                           object_shape = "circle")

circle_coords <- getCoordinates(circle_distribution, radius = 1, aisle_width = 1.5)
plotDistribution(circle_coords, radius = 1, aisle_width = 1.5)

