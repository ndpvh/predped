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

#####################
Polygons <- setClass("Polygons", 
                     slots = list(num_columns = "numeric",
                                  num_rows = "numeric",
                                  total_polygons = "numeric",
                                  polygon_distribution = "matrix",
                                  polygons = "list",
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
  
  # Assign values to object slots
  .Object@num_columns <- num_columns
  .Object@num_rows <- num_rows
  .Object@total_polygons <- total_polygons
  .Object@polygon_distribution <- polygon_distribution
  .Object@polygons <- list()
  .Object@fill_slots <- fill_slots
  
  return(.Object)
})

# get Coordinates
setGeneric("getCoordinates", function(object, num_edges = NULL, edge_lengths = NULL, equal_edges = FALSE, aisle_width = 1.5, ...) {
  standardGeneric("getCoordinates")
})


setMethod("getCoordinates", "Polygons", function(object, num_edges = NULL, edge_lengths = NULL, equal_edges = FALSE, aisle_width = 1.5, ...) {
  # Generate one random polygon (assuming this function works correctly)
  polygon_obj <- generate_random_polygon(num_edges = num_edges, edge_lengths = edge_lengths, equal_edges = equal_edges, ...)
  polygon_points <- polygon_obj@points

  # Calculate the height and width of the polygon
  polygon_height <- max(polygon_points[, "y"]) - min(polygon_points[, "y"])
  polygon_width <- max(polygon_points[, "x"]) - min(polygon_points[, "x"])

  # Extract number of columns and rows from the distribution matrix
  num_columns <- ncol(object@polygon_distribution)
  num_rows <- nrow(object@polygon_distribution)
  
  # Initialize the list of polygon coordinates
  polygon_coords_list <- vector("list", length = object@total_polygons)
  index <- 1

  # Initialize the starting y-coordinate for each column
  column_start_y <- rep(0, num_columns)

  for (i in 1:num_rows) {
    for (j in 1:num_columns) {
      num_polygons_in_slot <- object@polygon_distribution[i, j]
      
      # Calculate the displacement for each column
      displacement_x <- (j - 1) * (polygon_width + aisle_width)
      
      # Get the current y-coordinate start for this column
      displacement_y <- column_start_y[j]
      
      for (k in 1:num_polygons_in_slot) {
        # Copy the coordinates of the polygon template
        polygon_coords <- polygon_points
        
        # Update polygon coordinates based on displacement
        polygon_coords[, "x"] <- polygon_coords[, "x"] + displacement_x
        polygon_coords[, "y"] <- polygon_coords[, "y"] + displacement_y
        
        # Store polygon coordinates
        polygon_coords_list[[index]] <- polygon_coords
        index <- index + 1

        # Increment y-coordinate for the next polygon in the same slot
        displacement_y <- displacement_y + polygon_height
      }
      
      # Update the starting y-coordinate for the next row in this column
      column_start_y[j] <- displacement_y + aisle_width
    }
  }
  
  # Assign coordinates to object slot
  object@polygons <- polygon_coords_list
  
  # Return the object
  return(object)
})



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
  
  # Plot polygons
  ggplot(all_coords, aes(x, y, group = group)) +
    geom_polygon(fill = "gray", color = "black") +
    theme_minimal() +
    coord_equal() +
    labs(title = "Polygons with Aisles")
}


polygon_distribution <- matrix(c(3, 2, 4, 2), nrow = 4, byrow = TRUE)

# Create a Polygons object
polygons <- new("Polygons", polygon_distribution = polygon_distribution)
poly_coords <- getCoordinates(polygons, edge_lengths = 1:2)
poly_coords
plot_polygons(poly_coords)

polygons <- new("Polygons", fill_slots = TRUE)
poly_coords <- getCoordinates(polygons)
plot_polygons(poly_coords)

str(poly_coords)
class(poly_coords)

