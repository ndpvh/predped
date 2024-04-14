generate_random_polygon <- function(num_edges = NULL, edge_lengths = NULL, equal_edges = FALSE, ...) {
  if (is.null(num_edges)) {
    num_edges <- sample(3:10, 1)  # Random number of edges between 3 and 10
  } else {
    if (num_edges < 3) {
      stop("Number of edges must be at least 3")
    }
  }
  
  if (equal_edges) {
    if (!is.null(edge_lengths) && length(edge_lengths) > 1) {
      warning("All edges are set to be equal. Only the first edge length is being used.")
    }
    edge_length <- ifelse(!is.null(edge_lengths), edge_lengths[1], runif(1, min = 5, max = 15))
    edge_lengths <- rep(edge_length, num_edges)
    message("All edges are ", edge_length, " units long.")
  } else {
    if (!is.null(edge_lengths)) {
      if (length(edge_lengths) < num_edges) {
        warning("You provided only ", length(edge_lengths), " edge lengths. The remaining edge lengths are generated randomly.")
        edge_lengths <- c(edge_lengths, runif(num_edges - length(edge_lengths), min = 5, max = 15))
      } else if (length(edge_lengths) > num_edges) {
        warning("Number of edges is ", num_edges, ". The first ", num_edges, " edge lengths are used.")
        edge_lengths <- edge_lengths[1:num_edges]
      }
    } else {
      edge_lengths <- runif(num_edges, min = 1, max = 15)
    }
  }
  
  max_edge_length <- max(edge_lengths)
  
  angles <- seq(0, 2 * pi, length.out = num_edges + 1)
  edge_angles <- sample(angles[-length(angles)], num_edges, replace = FALSE)
  edge_angles <- sort(edge_angles)
  
  vertices <- matrix(nrow = num_edges, ncol = 2)
  
  for (i in 1:num_edges) {
    angle <- edge_angles[i]
    vertices[i, 1] <- edge_lengths[i] * cos(angle) 
    vertices[i, 2] <- edge_lengths[i] * sin(angle)  
  }
  
  # Close the polygon by duplicating the first vertex at the end
  vertices <- rbind(vertices, vertices[1, ])
  
  # Convert matrix to data frame
  vertices_df <- data.frame(x = vertices[, 1], y = vertices[, 2])

  # Create a polygon object
  polygon_obj <- new("polygon", points = as.matrix(vertices_df), ...)
  
  return(polygon_obj)
}

# Example usage without specifying the number of edges or edge lengths
random_polygon <- generate_random_polygon()

# Plot the random polygon
ggplot2::ggplot() +
  ggplot2::geom_polygon(data = random_polygon, ggplot2::aes(x = x, y = y), fill = "grey", color = "black") +
  ggplot2::theme_minimal()

# Example usage with specifying the number of edges and setting all edge lengths to be equal
equal_edges_polygon <- generate_random_polygon(num_edges = 5, edge_lengths = c(9, 3, 5, 8), equal_edges = TRUE)

# Plot the polygon with equal edges
ggplot2::ggplot() +
  ggplot2::geom_polygon(data = equal_edges_polygon, ggplot2::aes(x = x, y = y), fill = "grey", color = "black") +
  ggplot2::theme_minimal()

# Example usage with specifying the number of edges and edge lengths
specified_polygon <- generate_random_polygon(num_edges = 6, equal_edges = TRUE)

# Plot the specified polygon
ggplot2::ggplot() +
  ggplot2::geom_polygon(data = specified_polygon, ggplot2::aes(x = x, y = y), fill = "grey", color = "black") +
  ggplot2::theme_minimal()
  

#' Retrieve coordinates of a randomly generated polygon as a polygon object.
#' 
#' This function generates a random polygon based on the specified parameters and returns it as a polygon object.
#' 
#' @param num_edges Number of edges of the polygon.
#' @param edge_lengths Lengths of edges of the polygon.
#' @param equal_edges Logical indicating if all edges should be equal in length.
#' 
#' @return An instance of the polygon class containing the coordinates of the vertices of the polygon.
#' 
#' @export
#' 
getCoordinates <- function(num_edges = NULL, edge_lengths = NULL, equal_edges = FALSE) {
  polygon_coords <- generate_random_polygon(num_edges = num_edges, edge_lengths = edge_lengths, equal_edges = equal_edges)
  polygon_object <- new("polygon", points = as.matrix(polygon_coords), clock_wise = TRUE)
  return(polygon_object)
}

#' Retrieve coordinates of a rectangle surrounding the polygon.
#' 
#' This function calculates the minimum and maximum coordinates of the polygon's vertices, 
#' and constructs a rectangle around the polygon based on these coordinates.
#' 
#' @param object A polygon object.
#' 
#' @return A list containing the coordinates of the rectangle: 'x', 'y', 'width', and 'height'.
#' 
#' @export
#' 
getSurroundingRectangle <- function(object) {
  min_x <- min(object@points[, 1])
  max_x <- max(object@points[, 1])
  min_y <- min(object@points[, 2])
  max_y <- max(object@points[, 2])
  
  center <- c((min_x + max_x) / 2, (min_y + max_y) / 2)
  size <- c(max_x - min_x, max_y - min_y)
  
  return(new("rectangle", center = center, size = size))
}

#' Plot method for polygon objects
#'
#' This method plots the polygon represented by the polygon object.
#'
#' @param object A polygon object.
#' @param ... Additional arguments to be passed to geom_polygon.
#'
#' @export
#'
setMethod("plot", "polygon", function(x, ...) {
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = as.data.frame(x@points),  # Convert polygon points to data frame
      aes(x = x, y = y),  # Assuming columns are named X1 and X2
      fill = "grey", color = "black",  # Set fill and border colors for polygon
    ) +
    ggplot2::theme_minimal()
})

equal_edges_polygon <- getCoordinates(num_edges = 10)
plot(equal_edges_polygon)

setMethod("plot", "rectangle", function(x, ...) {
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = as.data.frame(x@points),  # Convert points to data frame
      aes(x = V1, y = V2),  # Assuming columns are named V1 and V2
      fill = NA, color = "red",  # Set border color for rectangle
      ...  # Pass additional arguments to geom_polygon
    ) +
    ggplot2::theme_minimal()
})


# Get the surrounding rectangle
surrounding_rect <- getSurroundingRectangle(equal_edges_polygon)

# Plot the rectangle
plot(surrounding_rect)

print(as.data.frame(t(surrounding_rect@points)))

getCoordinates <- function(num_edges = NULL, edge_lengths = NULL, equal_edges = FALSE, aisle_width = 1.5, ...) {
  # Generate polygon coordinates
  polygon_coords <- generate_random_polygon(num_edges = num_edges, edge_lengths = edge_lengths, equal_edges = equal_edges)
  
  # Calculate surrounding rectangle coordinates
  min_x <- min(polygon_coords$x)
  max_x <- max(polygon_coords$x)
  min_y <- min(polygon_coords$y)
  max_y <- max(polygon_coords$y)
  
  # Expand the rectangle to include aisle width
  min_x <- min_x - aisle_width
  max_x <- max_x + aisle_width
  min_y <- min_y - aisle_width
  max_y <- max_y + aisle_width
  
  # Construct the rectangle coordinates
  rectangle_coords <- list(x = c(min_x, max_x, max_x, min_x, min_x), 
                           y = c(min_y, min_y, max_y, max_y, min_y))
  
  # Return both sets of coordinates
  return(list(polygon_coords = polygon_coords, rectangle_coords = rectangle_coords))
}


#####################
Polygons <- setClass("Polygons", 
                     slots = list(num_columns = "numeric",
                                  num_rows = "numeric",
                                  total_polygons = "numeric",
                                  polygon_distribution = "matrix",
                                  polygons = "list",
                                  total_rectangles = "numeric",
                                  surrounding_rectangles = "list",
                                  fill_slots = "logical"))

setMethod("initialize", "Polygons", function(.Object, num_columns = NULL, num_rows = NULL, total_polygons = NULL, polygon_distribution = NULL, fill_slots = FALSE, ...) {
  # Check whether polygon_distribution is already provided
  if (!is.null(polygon_distribution)) {
    num_columns <- ncol(polygon_distribution)
    num_rows <- nrow(polygon_distribution)
    total_polygons <- sum(polygon_distribution)
    warning("You have provided a distribution for the polygons. The number of columns, number of rows, and the number of polygons are adjusted based on the distribution.")
  } else {
    # Generate random num_columns & num_rows if not provided
    if (is.null(num_columns)) {
      num_columns <- sample(1:10, 1) # Random columns between 1-10
    }
    if (is.null(num_rows)) {
      num_rows <- sample(1:10, 1) # Random rows between 1-10
    }
    # Generate random polygons between 1 and num_columns * num_rows
    if (is.null(total_polygons)) {
      total_polygons <- sample(1:(num_columns * num_rows), 1) 
    }
  }
  
  if (is.null(polygon_distribution)) {
    # Initialize the distribution
    polygon_distribution <- matrix(0, nrow = num_rows, ncol = num_columns)
    
    # Randomly fill the polygons to each slot
    polygons_remaining <- total_polygons
    while (polygons_remaining > 0) {
      row_index <- sample(1:num_rows, 1)
      col_index <- sample(1:num_columns, 1)
      polygon_distribution[row_index, col_index] <- polygon_distribution[row_index, col_index] + 1
      polygons_remaining <- polygons_remaining - 1
    }

    # Adjust number of columns and rows if there are empty columns or rows
    empty_cols <- which(colSums(polygon_distribution) == 0)
    empty_rows <- which(rowSums(polygon_distribution) == 0) 
    
    if (!is.null(empty_rows) && length(empty_rows) > 0) {
      num_rows <- num_rows - length(empty_rows)
      polygon_distribution <- polygon_distribution[rowSums(polygon_distribution) != 0, ]  
      warning(paste(length(empty_rows), "empty row(s) detected and omitted."))
    }
    if (!is.null(empty_cols) && !anyNA(empty_cols) && length(empty_cols) > 0 && nrow(polygon_distribution) > 0) {
      num_columns <- num_columns - length(empty_cols)
      polygon_distribution <- polygon_distribution[, colSums(polygon_distribution) != 0]
      warning(paste(length(empty_cols), "empty column(s) detected and omitted."))
    }
    
    # Fill the slots if specified
    if (fill_slots) {
      empty_slots_count <- sum(polygon_distribution == 0)
      if (empty_slots_count > 0) {
        # generate random number of objects 1-5
        random_objects <- sample(1:5, empty_slots_count, replace = TRUE)
        polygon_distribution[polygon_distribution == 0] <- random_objects
        total_polygons <- total_polygons + empty_slots_count
        warning(paste("Empty slots are filled with one object each. There are", empty_slots_count, "additional objects."))
      }
    }
  }
  
  # Ensure polygon_distribution is a matrix
  polygon_distribution <- as.matrix(polygon_distribution)
  
  # Initialize empty surrounding rectangles
  surrounding_rectangles <- vector("list", length = total_polygons)
  
  # Assign values to object slots
  .Object@num_columns <- num_columns
  .Object@num_rows <- num_rows
  .Object@total_polygons <- total_polygons
  .Object@polygon_distribution <- polygon_distribution
  .Object@polygons <- list()
  .Object@total_rectangles <- total_polygons
  .Object@surrounding_rectangles <- surrounding_rectangles
  .Object@fill_slots <- fill_slots
  
  return(.Object)
})

# get Coordinates
setGeneric("getCoordinates", function(object, num_edges = NULL, edge_lengths = NULL, equal_edges = FALSE, aisle_width = 1.5, ...) {
  standardGeneric("getCoordinates")
})

setMethod("getCoordinates", "Polygons", function(object, num_edges = NULL, edge_lengths = NULL, equal_edges = FALSE, aisle_width = 1.5, ...) {
  # Generate one random polygon
  polygon_obj <- generate_random_polygon(num_edges = num_edges, edge_lengths = edge_lengths, equal_edges = equal_edges, ...)
  polygon_points <- polygon_obj@points
  
  # Find the minimum x and y coordinates
  min_x <- min(polygon_points[, "x"])
  min_y <- min(polygon_points[, "y"])
  max_x <- max(polygon_points[, "x"])
  max_y <- max(polygon_points[, "y"])
  
  # Adjust polygon coordinates to ensure no negative values
  polygon_points[, "x"] <- polygon_points[, "x"] - min_x
  polygon_points[, "y"] <- polygon_points[, "y"] - min_y
  
  # Extract number of columns and rows from the distribution matrix
  num_columns <- ncol(object@polygon_distribution)
  num_rows <- nrow(object@polygon_distribution)
  
  # Fill the polygon distribution with the adjusted polygon
  polygon_coords_list <- vector("list", length = object@total_polygons)
  index <- 1
  for (i in 1:num_rows) {
    for (j in 1:num_columns) {
      num_polygons <- object@polygon_distribution[i, j]
      if (num_polygons > 0) {
        # Initialize displacement
        displacement_x <- (j - 1) * (max_x - min_x)
        first_polygon_y <- (i - 1) * (max_y - min_y)
        
        # Generate polygons for the slot
        for (k in 1:num_polygons) {
          # Update polygon coordinates based on displacement
          polygon_coords <- polygon_points
          polygon_coords[, "x"] <- polygon_coords[, "x"] + displacement_x
          polygon_coords[, "y"] <- polygon_coords[, "y"] + first_polygon_y
          
          # Store polygon coordinates
          polygon_coords_list[[index]] <- polygon_coords
          index <- index + 1
          
          # Increment y-coordinate for the next polygon in the slot
          first_polygon_y <- first_polygon_y + (max_y - min_y)
        }
      }
    }
  }
  
  # Assign coordinates to object slot
  object@polygons <- polygon_coords_list
  
  # Return the object
  return(object)
})


# Plot function
plot_polygons <- function(poly_coords) {
  # Extract polygon coordinates
  polygons <- poly_coords@polygons
  
  # Initialize empty dataframe to store all coordinates
  all_coords <- data.frame(x = numeric(0), y = numeric(0), type = character(0), group = integer(0))
  
  # Add polygon coordinates to dataframe
  for (i in 1:length(polygons)) {
    poly_coords <- polygons[[i]]
    num_points <- nrow(poly_coords)
    group <- rep(i, num_points)  # Create a group identifier for each polygon
    all_coords <- rbind(all_coords, data.frame(x = poly_coords[, "x"], y = poly_coords[, "y"], type = "Polygon", group = group))
  }
  
  ggplot(all_coords, aes(x, y, group = group)) +
    geom_polygon(fill = "gray", color = "black") + 
    theme_minimal() +
    coord_equal() +
    labs(title = "Polygons")
}


polygon_distribution <- matrix(c(3, 2, 4, 2), nrow = 1, byrow = TRUE)

# Create a Polygons object
polygons <- new("Polygons", polygon_distribution = polygon_distribution)
poly_coords <- getCoordinates(polygons)
poly_coords
plot_polygons(poly_coords)

polygons <- new("Polygons", fill_slots = TRUE, num_rows = 3)
poly_coords <- getCoordinates(polygons, equal_edges = TRUE, num_edges = 10)
plot_polygons(poly_coords)

str(poly_coords)
class(poly_coords)
