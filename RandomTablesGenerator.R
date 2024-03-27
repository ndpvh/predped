#' An S4 class representing the distribution and coordinates of tables in a supermarket.
#' @slot num_columns Number of columns in the supermarket.
#' @slot num_rows Number of rows in the supermarket.
#' @slot total_tables Total number of tables in the supermarket.
#' @slot table_distribution Distribution of tables among columns and rows.
#' @slot circles Coordinates of the circular objects (tables).
#' @slot fill_slots Allowing empty slots in table distribution.
#' @name Tables
#' @export
#' 
Tables <- setClass("Tables", slots = list(num_columns = "numeric",
                                          num_rows = "numeric",
                                          total_tables = "numeric",
                                          table_distribution = "matrix",
                                          circles = "list",
                                          fill_slots = "logical"))

setMethod("initialize", "Tables", function(.Object, num_columns = NULL, num_rows = NULL, total_tables = NULL, table_distribution = NULL, fill_slots = FALSE, ...) {
    # Check whether table_distribution is already provided
    if (!is.null(table_distribution)) {
        num_columns <- ncol(table_distribution)
        num_rows <- nrow(table_distribution)
        total_tables <- sum(table_distribution)
        warning("You have provided a distribution for the tables. The number of columns, number of rows, and the number of tables are adjusted based on the distribution.")
    } else {
        # Generate random num_columns & num_rows if not provided
        if (is.null(num_columns)) {
            num_columns <- sample(1:10, 1) # Random columns between 1-10
        }
        if (is.null(num_rows)) {
            num_rows <- sample(1:10, 1) # Random rows between 1-10
        }
        # Generate random tables between 1 and num_columns * num_rows
        if (is.null(total_tables)) {
            total_tables <- sample(1:(num_columns * num_rows), 1) 
        }
    }

    if (is.null(table_distribution)) {
        # Initialize the distribution
        table_distribution <- matrix(0, nrow = num_rows, ncol = num_columns)

        # Randomly fill the tables to each slot
        tables_remaining <- total_tables
        while (tables_remaining > 0) {
            row_index <- sample(1:num_rows, 1)
            col_index <- sample(1:num_columns, 1)
            table_distribution[row_index, col_index] <- table_distribution[row_index, col_index] + 1
            tables_remaining <- tables_remaining - 1
        }

        # Adjust the number of columns and rows if there are empty columns or rows
        empty_cols <- which(colSums(table_distribution) == 0)
        empty_rows <- which(rowSums(table_distribution) == 0) 

        if (!is.null(empty_rows) && length(empty_rows) > 0) { # to prevent rowSums error
            num_rows <- num_rows - length(empty_rows)
            table_distribution <- table_distribution[rowSums(table_distribution) != 0, ]  
            warning(paste(length(empty_rows), "empty row(s) detected and omitted."))
        }
        if (!is.null(empty_cols) && !anyNA(empty_cols) && length(empty_cols) > 0 && nrow(table_distribution) > 0) { # to prevent colSums error
            num_columns <- num_columns - length(empty_cols)
            table_distribution <- table_distribution[, colSums(table_distribution) != 0]
            warning(paste(length(empty_cols), "empty column(s) detected and omitted."))
        }
        
        # fill the slots with 1 table if there are empty slots & update total_tables
        if (fill_slots) {
            empty_slots_count <- sum(table_distribution == 0)
            if (empty_slots_count > 0) {
                additional_tables <- empty_slots_count
                table_distribution[table_distribution == 0] <- 1
                total_tables <- total_tables + additional_tables
                warning(paste("Empty slots are filled with one table each. There are", empty_slots_count, "additional tables."))
            }
        }
    }

    # Ensure table_distribution is a matrix
    table_distribution <- as.matrix(table_distribution)

    # Assign values to object slots
    .Object@num_columns <- num_columns
    .Object@num_rows <- num_rows
    .Object@total_tables <- total_tables
    .Object@table_distribution <- table_distribution
    .Object@circles <- list()
    .Object@fill_slots <- fill_slots

    return(.Object)
})

#' Retrieve coordinates of circles.
#'
#' This function generates coordinates of circles representing tables within the Tables object.
#'
#' @param object A Tables object containing information about table distribution.
#' @param radius Radius of each circle (table).
#' @param aisle_width Width of the aisle between tables.
#' @param ... Additional arguments.
#'
#' @return The Tables object with updated circles slot containing coordinates of tables.
#'
#' @export
#' @name getCoordinates
#' 
setGeneric("getCoordinates", function(object, radius = 1, aisle_width = 1.5, ...) {
    standardGeneric("getCoordinates")
})

setMethod("getCoordinates", "Tables", function(object, radius = 1, aisle_width = 1.5, ...) {
    table_circles <- vector("list", length = sum(object@table_distribution))
    index <- 1
    row_y_start <- rep(0, object@num_rows)  # Initialize starting y-coordinate for each row
    
    for (row in 1:object@num_rows) {
        max_table_nr_per_row <- max(object@table_distribution[row, ])
        if (max_table_nr_per_row > 0) {
            if (row > 1) {
                # Calculate the starting y-coordinate for each subsequent row
                row_y_start[row] <- row_y_start[row - 1] + (max(object@table_distribution[row - 1, ]) * (2 * radius)) + aisle_width
            }
            for (col in 1:object@num_columns) {
                num_tables <- object@table_distribution[row, col]
                if (num_tables > 0) {
                    col_x <- (col - 1) * (2 * radius + aisle_width) + radius
                    for (table in 1:num_tables) {
                        table_center <- c(col_x, row_y_start[row] + (table - 0.5) * (2 * radius))
                        table_circles[[index]] <- new("circle", center = table_center, radius = radius)
                        index <- index + 1
                    }
                }
            }
        }
    }
    
    object@circles <- table_circles
    return(object)
})

#' Plot method for Tables objects
#' This method is only to visualize the changes in the code.
#' 
#' @param x A Tables object.
#' @param radius Radius of each circle (table).
#' @param aisle_width Width of the aisle between tables.
#' @param aisle_color Color of the aisle.
#' @param table_color Color of the tables.
#' @param ... Additional arguments passed to geom_polygon.
#' 
#' 
plotTables <- function(x, radius = 1, aisle_width = 1.5, 
                        aisle_color = "white", table_color = "grey", ...) {
  
  # Load required libraries
  require(ggplot2)
  
  # Create data frame for tables
  tables_df <- data.frame(
    x = unlist(lapply(x@circles, function(circle) circle@center[1])),
    y = unlist(lapply(x@circles, function(circle) circle@center[2])),
    radius = rep(radius, length(x@circles))
  )
  
  # Create data frame for aisles
  aisles_df <- data.frame(
    x = seq(0, max(tables_df$x) + 2 * radius + aisle_width, by = aisle_width),
    y = 0,
    width = aisle_width,
    height = max(tables_df$y) + 2 * radius
  )
  
  # Plot
  ggplot() +
    geom_rect(data = aisles_df, aes(xmin = x, xmax = x + width, ymin = y, ymax = y + height), 
              fill = aisle_color, color = NA) +
    lapply(x@circles, function(circle) plot(circle, ...)) +
    coord_fixed() +
    theme_void()
}

# Example use
table_distribution <- matrix(c(7, 2, 1,
                               8, 10, 2,
                               3, 5, 6), nrow = 3, byrow = TRUE)

tables <- new("Tables", table_distribution = table_distribution)
tables_coordinates <- getCoordinates(tables, radius = 1, aisle_width = 1.5)
plotTables(tables_coordinates)

tables_2 <- new("Tables", num_columns = 3, num_rows = 3, total_tables = 12)
print(tables_2)
tables_coordinates <- getCoordinates(tables_2, radius = 1, aisle_width = 0)
plotTables(tables_coordinates)

tables_3 <- new("Tables")
tables_coordinates <- getCoordinates(tables_3, radius = 1, aisle_width = 0)
plotTables(tables_coordinates)
