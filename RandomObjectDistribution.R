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
#' @examples
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

  # Check whether object_distribution is already provided
  if (!is.null(object_distribution)) {
    num_columns <- ncol(object_distribution)
    num_rows <- nrow(object_distribution)
    total_objects <- sum(object_distribution)
    warning("You have provided a distribution for the objects. The number of columns, number of rows, and the total number of objects are adjusted based on the distribution.")
  } else {
    # Generate random distribution if not provided
    if (is.null(num_columns)) {
      num_columns <- sample(1:10, 1) # Random columns between 1-10
    }
    if (is.null(num_rows)) {
      num_rows <- sample(1:10, 1) # Random rows between 1-10
    }
    if (is.null(total_objects)) {
      total_objects <- sample(1:(num_columns * num_rows), 1) 
    }
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
        # generate random number of objects 1-5
        random_objects <- sample(1:5, empty_slots_count, replace = TRUE)
        object_distribution[object_distribution == 0] <- random_objects
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
#' This function generates coordinates of objects (rectangles, circles, or polygons)
#' based on the specified object shape within the RandomObjectDistribution object.
#' 
#' @param object A RandomObjectDistribution object containing information about object distribution.
#' @param rectangle_length Length of each rectangle.
#' @param rectangle_width Width of each rectangle.
#' @param radius Radius of each circle.
#' @param num_edges Number of edges for each polygon.
#' @param edge_lengths Lengths of the edges (for polygons).
#' @param equal_edges A logical flag indicating whether all edges should be of equal length (for polygons).
#' @param aisle_width Width of the aisles between objects.
#' @param ... Additional arguments.
#' 
#' @return The RandomObjectDistribution object with updated objects slot containing coordinates of objects.
#' 
#' @export
#' @name getCoordinates
#' 
#' @examples
#' rect_coords <- getCoordinates(rect_distribution, rectangle_length = 1, rectangle_width = 0.5, aisle_width = 1.5)
#' circle_coords <- getCoordinates(circle_distribution, radius = 1, aisle_width = 1.5)
#' poly_coords <- getCoordinates(poly_distribution, num_edges = 6, edge_lengths = 1, equal_edges = TRUE, aisle_width = 1.5)
#' 
setGeneric("getCoordinates", function(object, rectangle_length = 1, rectangle_width = 0.5, radius = 1, num_edges = NULL, edge_lengths = NULL, equal_edges = FALSE, aisle_width = 1.5, ...) {
    standardGeneric("getCoordinates")
})

setMethod("getCoordinates", "RandomObjectDistribution", function(object, rectangle_length = 1, rectangle_width = 0.5, radius = 1, num_edges = NULL, edge_lengths = NULL, equal_edges = FALSE, aisle_width = 1.5, ...) {

    # method for the coordinates of the rectangles
    if (object@object_shape == "rectangle") {
        rectangles <- vector("list", length = sum(object@object_distribution))
        index <- 1
        row_y_start <- rep(0, object@num_rows)  # Initialize starting y-coordinate for each row

        for (row in 1:object@num_rows) {
            max_nr_per_row <- max(object@object_distribution[row, ])
            if (max_nr_per_row > 0) {
                if (row > 1) {
                    # Calculate the starting y-coordinate for each subsequent row
                    row_y_start[row] <- row_y_start[row - 1] + (max(object@object_distribution[row - 1, ]) * rectangle_length) + aisle_width
                }
                for (col in 1:object@num_columns) {
                    num_objects <- object@object_distribution[row, col]
                    if (num_objects > 0) {
                        col_x <- (col - 1) * (rectangle_width + aisle_width) + rectangle_width / 2
                        for (obj in 1:num_objects) {
                            obj_center <- c(col_x, row_y_start[row] + (obj - 0.5) * rectangle_length)
                            rectangles[[index]] <- new("rectangle", center = obj_center, size = c(rectangle_width, rectangle_length))
                            index <- index + 1
                        }
                    }
                }
            }
        }
        object@objects <- rectangles

    # method for the coordinates of the circles
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

      # method for the coordinates of the polygons
      } else if (object@object_shape == "polygon") {
          # Generate one random polygon
          polygon <- generate_random_polygon(num_edges = num_edges, edge_lengths = edge_lengths, equal_edges = equal_edges, ...)
          polygon_points <- polygon@points

          # The height and width of generated polygon
          polygon_height <- max(polygon_points[, "y"]) - min(polygon_points[, "y"])
          polygon_width <- max(polygon_points[, "x"]) - min(polygon_points[, "x"])

          # Get the number of columns and rows
          num_columns <- ncol(object@object_distribution)
          num_rows <- nrow(object@object_distribution)
          
          # Initialize a list for coordinates
          polygons <- vector("list", length = object@total_objects)
          index <- 1

          # Initialize the starting y-coordinate for each column
          column_start_y <- rep(0, num_columns)

          # Maximum y-coordinate reached in a previous row
          previous_row_max_y <- 0

          for (i in 1:num_rows) {
            # For each new row, start with the maximum y-coordinate from the previous row
            # plus the aisle width
            if (i > 1) {
              column_start_y <- rep(previous_row_max_y + aisle_width, num_columns)
            }

            # Maximum y-coordinate reached in the current row
            current_row_max_y <- 0

            for (j in 1:num_columns) {
              num_polygons_in_slot <- object@object_distribution[i, j]

              # Displacement for each column on the x-axis
              displacement_x <- (j - 1) * (polygon_width + aisle_width)

              # Starting y-coordinate for this column
              displacement_y <- column_start_y[j]

              for (k in 1:num_polygons_in_slot) {
                # Update polygon coordinates for the current position
                polygon_coords <- polygon_points
                polygon_coords[, "x"] <- polygon_coords[, "x"] + displacement_x
                polygon_coords[, "y"] <- polygon_coords[, "y"] + displacement_y

                # Update polygons list
                polygons[[index]] <- polygon_coords
                index <- index + 1

                # Update the y-coordinate for the next polygon in the same slot
                displacement_y <- displacement_y + polygon_height

                # Update the current row's maximum y-coordinate
                if (displacement_y > current_row_max_y) {
                  current_row_max_y <- displacement_y
                }
              }

              # Update the starting y-coordinate for the next row in this column
              column_start_y[j] <- displacement_y
            }

            # Update the previous row's maximum y-coordinate for the next iteration
            previous_row_max_y <- current_row_max_y
          }

          object@objects <- polygons


    } else {
        stop("Unsupported object shape. Supported shapes are 'rectangle' and 'circle', and 'polygon'.")
    }
    return(object)
})

#' A helper function to generate a Random Polygon
#'
#' This function generates a random polygon, allowing customization
#' of the number of edges and the lengths of those edges.
#' Polygons with equal edge lengths can be created using the `equal_edges` argument.
#' This function is used within the `RandomObjectDistribution` class.
#'
#' @param num_edges An optional integer specifying the number of edges the polygon should have.
#' If not provided, a random number between 3 and 10 will be selected.
#' @param edge_lengths A numeric vector specifying the length of each edge.
#' If `equal_edges` is TRUE, only the first value of this vector is used to set all edge lengths.
#' If not provided, random lengths between 1 and 5 units will be generated.
#' @param equal_edges A logical flag indicating whether all edges should be of equal length.
#' If set to TRUE and `edge_lengths` is provided, only the first edge length is used
#' If `edge_lengths` is not provided, a random length between 1 and 5 units is generated and used for all edges.
#' @param ... Additional arguments to pass to the constructor of the polygon, if needed.
#'
#' @return A polygon object with specified or randomly assigned edges and lengths.
#'
#' @export
#' @name generate_random_polygon
#' 
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
    edge_length <- ifelse(!is.null(edge_lengths), edge_lengths[1], runif(1, min = 1, max = 5))
    edge_lengths <- rep(edge_length, num_edges)
    message("All edges are ", edge_length, " units long.")
  } else {
    if (!is.null(edge_lengths)) {
      if (length(edge_lengths) < num_edges) {
        warning("You provided only ", length(edge_lengths), " edge lengths. The remaining edge lengths are generated randomly.")
        edge_lengths <- c(edge_lengths, runif(num_edges - length(edge_lengths), min = 1, max = 5))
      } else if (length(edge_lengths) > num_edges) {
        warning("Number of edges is ", num_edges, ". The first ", num_edges, " edge lengths are used.")
        edge_lengths <- edge_lengths[1:num_edges]
      }
    } else {
      edge_lengths <- runif(num_edges, min = 1, max = 5)
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
  polygon <- new("polygon", points = as.matrix(vertices_df), ...)
  
  return(polygon)
}

#' A helper function to extract outline coordinates
#' According to their shape specific requirements
#'
#' This function generates coordinates from which a convex hull
#' can be drawn around the outline of all the objects from
#' the random distribution.
#' This function is used within the `plotDistribution` plotting function.
#'
#' @param object A data structure containing coordinates of the objects,
#'          obtained via the `getCoordinates` function applied to a RandomObjectDistribution object.
#'
#' @return A maxtrix with four columns, used to plot the outline in `plotDistribution`.
#'
#' @export
#' @name generate_random_polygon
#' 
out_coords <- function(object) {

    if (object@object_shape == "rectangle") {
        
        obj <- unlist(object@objects)
        ob <- lapply(obj, \(x) add_nodes(x, only_corners = TRUE))
        ob <- do.call(rbind, ob)
        ob <- unique(ob)
        tri.obj <- interp::tri.mesh(x = ob[,1], y = ob[,2], duplicate = "remove")
        hull <- tri.obj$chull
        ob <- ob[hull,]
        ob <- cbind(ob, ob[c(2:nrow(ob), 1),])
    }
    
    else if (object@object_shape == "polygon") {
        
        obj <- object@objects
        ob <- do.call(rbind, obj)
        ob <- unique(ob)
        tri.obj <- interp::tri.mesh(x = ob[,1], y = ob[,2], duplicate = "remove")
        hull <- tri.obj$chull
        ob <- ob[hull,]
        ob <- cbind(ob, ob[c(2:nrow(ob), 1),])
    }

    else if (object@object_shape == "circle") {
        
        obj <- lapply(circle_coords@objects, \(x) unlist(x))
        ob <- lapply(obj, \(x) add_nodes(x, only_corners = TRUE, space_between = 0.2))
        ob <- do.call(rbind, ob)
        ob <- unique(ob)
        tri.obj <- interp::tri.mesh(x = ob[,1], y = ob[,2], duplicate = "remove")
        hull <- tri.obj$chull
        ob <- ob[hull,]
        ob <- cbind(ob, ob[c(2:nrow(ob), 1),])
    }

    return(ob)
}

#' Plotting function for visualizing random object distribution using the coordinates of objects.
#'
#' This function plots objects based on coordinates obtained from a RandomObjectDistribution object.
#' It handles different shapes such as rectangles, circles, and polygons.
#' The coordinates of these objects should be provided by the getCoordinates() function.
#'
#' @param x A data structure containing coordinates of the objects,
#'          obtained via the getCoordinates function applied to a RandomObjectDistribution object.
#' @param object_fill_color Color used to fill the shapes.
#' @param object_border_color Color used for the border of the shapes.
#' @param ... Additional arguments passed to the geoms for customization.
#'
#' @return A ggplot object that visually represents the distribution of objects as defined and positioned
#'         in the RandomObjectDistribution instance.
#'
#' @export
#' @name plotDistribution
#'
#' @examples
#' plotDistribution(rect_coords)
#' plotDistribution(circle_coords)
#' plotDistribution(poly_coords)
#' 
plotDistribution <- function(x, object_fill_color = "grey", object_border_color = "black", ...) {
  
  require(ggplot2)
  
  # Start a ggplot object with blank aesthetics
  p <- ggplot() + geom_blank() + coord_fixed() + theme_void()

  # Add objects to the plot based on their shape
  if (x@object_shape == "rectangle" || x@object_shape == "circle" || x@object_shape == "polygon") {
    object_plots <- lapply(x@objects, function(object) {

      # Check the type of object and plot accordingly
      if (class(object)[1] == "rectangle") {
        # Plot rectangles with given dimensions
        plot(object, fill = object_fill_color, color = object_border_color, ...)

      } else if (class(object)[1] == "circle") {
        # Plot circles with given radius
        plot(object, fill = object_fill_color, color = object_border_color, ...)

      } else if (is.matrix(object)) {
        # Object is a matrix when representing polygon points
        # Convert matrix to dataframe for plotting
        df <- data.frame(x = object[, 1], y = object[, 2])
        ggplot2::geom_polygon(data = df, ggplot2::aes(x = x, y = y),
                              fill = object_fill_color, color = object_border_color, ...)
      } else {
        stop("Unsupported object shape.")
      }
    })

    # Combine all geoms into one ggplot object & plot the distribution
    for (geom in object_plots) {
      p <- p + geom
    }

    # Draw the outline around all of the plotted geoms
    pts <- out_coords(x)
    p <- p + ggplot2::geom_segment(ggplot2::aes(
                    x = as.numeric(pts[,1]), y = as.numeric(pts[,2]),
                    xend = as.numeric(pts[,3]), yend = as.numeric(pts[,4])))
    
  } else {
    stop("Unsupported object shape. Supported shapes are 'rectangle', 'circle', and 'polygon'.")
  }

  return(p)
}

#' Additional Examples with rectangles, circles, and polygons
#' Example use for rectangles
#' rect_distribution <- new("RandomObjectDistribution",
#'                          num_columns = 5,
#'                          num_rows = 4,
#'                          total_objects = 15,
#'                          fill_slots = TRUE,
#'                          object_shape = "rectangle")
#' rect_coords <- getCoordinates(rect_distribution,rectangle_length = 4, rectangle_width = 2, aisle_width = 1.5)
#' plotDistribution(rect_coords)
#' 
#' Example use for circles
#' circle_distribution <- new("RandomObjectDistribution",  
#'                            num_columns = 6,
#'                            num_rows = 4,
#'                            total_objects = 10,
#'                            fill_slots = TRUE,
#'                            object_shape = "circle")

#' circle_coords <- getCoordinates(circle_distribution, radius = 0.7, aisle_width = 4)
#' plotDistribution(circle_coords)
#' 
#' Example uses for polygons
#' poly_distribution <- new("RandomObjectDistribution",  
#'                          num_columns = 2,
#'                          num_rows = 2,
#'                          total_objects = 4,
#'                          fill_slots = TRUE,
#'                          object_shape = "polygon")
#' 
#' poly_coords <- getCoordinates(poly_distribution)
#' plotDistribution(poly_coords)
#'
#' polygon_matrix <- matrix(c(3, 2, 4, 2), nrow = 2, byrow = TRUE)
#' poly_distribution <- new("RandomObjectDistribution", object_distribution = polygon_matrix, object_shape = "polygon")
#' poly_coords <- getCoordinates(poly_distribution, num_edges = 6, edge_lengths = 1, equal_edges = TRUE, aisle_width = 1.5)
#' plotDistribution(poly_coords)
