# create an S4 class for the environment

# change these
# setwd("/Users/eceyatikci/predped/predped")
# gotsource("/Users/eceyatikci/predped/predped/RCore/pp_plot.R")

# Create an environment class
# environment_class <- setClass("EnvironmentClass", slots = list(
  
  # main environment characteristics
  length = "numeric", # length of environment
  width = "numeric",  # width of environment
  main_door_width = "numeric", # width of main door
  entrance_length = "numeric", # entrance area length
  entrance_width = "numeric", # entrance area width
  entrance_door_width = "numeric", # width of the entrance door
  entrance_exit_length = "numeric", # length of the border
  num_aisles = "numeric", # number of aisles 
  aisle_width = "numeric", # width of the aisles
  
  # capacity of the environment
  # should be defined as capacity
  # will change these later 
  # implement to the simulation
  max_num_agents = "numeric", # maximum number of agents at the same time
  min_num_agents = "numeric", # always some agents present(workers)
  
  # supermarket objects
  # shelves
  wall_shelf_length = "numeric", # length of wall shelves
  aisle_shelf_length = "numeric", # length of aisle shelves
  wall_shelf_width = "numeric", # width of the shelves
  aisle_shelf_width = "numeric", # width of aisle shelves
  
  # stands - for fruits, bakery, deepfreeze etc.
  # height is not important
  # generally wider than shelves and agents can move around all edges
  num_stands = "numeric", # number of counters
  stand_length = "numeric", # length of counters
  stand_width = "numeric", # width of the counters 
  
  # checkout
  # one longer edge should be blocked
  num_checkouts = "numeric", # number of checkouts
  checkout_length = "numeric", # length of checkouts
  checkout_width = "numeric" # width of checkouts
))

# update plotSpace
plotSpace <- function(objects, plotGrid = TRUE) { 
  par(mfrow = c(1,1))
  
  # needs to be fixed, not sure how to correct yet, fastest solution for now
  # range for the x and y axes
  x_range <- c(0, 50)
  y_range <- x_range
  
  # empty plot with equal aspect ratio
  plot(NA, xlim = x_range, ylim = y_range, xlab = "x", ylab = "y", bty = "n", asp = 1)
  
  # add grid
  if (plotGrid) {
    myGrid(list(x = x_range, y = y_range))
  }
  
  # add objects if there are any
  if (length(objects) > 0) {
    for (i in seq_along(objects)) {
      obj <- objects[[i]]
      if (length(obj) > 0) {  # check if not empty
        if (!is.null(obj$col) && !is.na(obj$col)) {
          rect(obj$x[1], obj$y[1], obj$x[2], obj$y[2], col = obj$col, border = obj$col, lwd = obj$lwd)
        } else {
          rect(obj$x[1], obj$y[1], obj$x[2], obj$y[2], border = obj$col, lwd = obj$lwd, lty = obj$lty)
        }
      }
    }
  }
  
  # plot overall space with drawSquare
  if (!is.null(objects[[1]])) {
    drawSquare(objects[[1]], col = NA, border = "#636363", lwd = 2)
  }
}


# initializiton method for baseline store
setMethod("initialize",
          signature = "EnvironmentClass",
          function(.Object,
                   length = 40,
                   width = 25,
                   main_door_width = 2.5,
                   entrance_length = 8,
                   entrance_width = width,
                   entrance_door_width = 1.5,
                   entrance_exit_length = entrance_length,
                   num_aisles = 6,
                   aisle_width = 2,
                   max_num_agents = 100,
                   min_num_agents = 10,
                   wall_shelf_length = 20,
                   aisle_shelf_length = 10,
                   wall_shelf_width = 1.2,
                   aisle_shelf_width = 1.2,
                   num_stands = 3,
                   stand_length = 2,
                   stand_width = 2,
                   num_checkouts = 4,
                   checkout_length = 3.5,
                   checkout_width = 1.5) {
            
            # assign values to slots
            .Object@length <- length
            .Object@width <- width
            .Object@main_door_width <- main_door_width
            .Object@entrance_length <- entrance_length
            .Object@entrance_width <- entrance_width
            .Object@entrance_door_width <- entrance_door_width
            .Object@entrance_exit_length <- entrance_exit_length
            .Object@num_aisles <- num_aisles
            .Object@aisle_width <- aisle_width
            .Object@max_num_agents <- max_num_agents
            .Object@min_num_agents <- min_num_agents
            .Object@wall_shelf_length <- wall_shelf_length
            .Object@aisle_shelf_length <- aisle_shelf_length
            .Object@wall_shelf_width <- wall_shelf_width
            .Object@aisle_shelf_width <- aisle_shelf_width
            .Object@num_stands <- num_stands
            .Object@stand_length <- stand_length
            .Object@stand_width <- stand_width
            .Object@num_checkouts <- num_checkouts
            .Object@checkout_length <- checkout_length
            .Object@checkout_width <- checkout_width
            
            return(.Object)
          })


# Plot the environment
setGeneric("PlotEnvironment", function(object) { standardGeneric("PlotEnvironment")})

setMethod("PlotEnvironment", signature = "EnvironmentClass", function(object) {
  
  # space
  space <- list(x = c(0, object@length),
                y = c(0, object@width))
  
  # entrance & exit area
  entrance <- list(x = c(0, object@entrance_length),
                   y = c(0, object@entrance_width),
                   lty = "dashed")
  exit_border <- list(x = c(0, object@entrance_length),
                      y = c(0, object@entrance_exit_length),
                      lty = "dashed")
  
  # wall shelves
  bottom_shelf <- list(x = c(object@entrance_length, object@length),
                       y = c(object@wall_shelf_width, 0),
                       col = "grey")
  top_shelf <- list(x = c(object@entrance_length * 2, object@length),
                    y = c(object@width, object@width - object@wall_shelf_width),
                    col = "grey")
  right_shelf <- list(x = c(object@length - object@wall_shelf_width, object@length),
                      y = c(0, object@width),
                      col = "grey")
  
  # stands
  all_stands <- list()
  # give this as an argument later! I don't like it here
  rows <- 2 # can be adjusted based on preferences
  for (row in 1:rows) {
    for (i in 1:object@num_stands) {
      stand_x_start <- object@entrance_length + 2 * object@aisle_width + (i - 1) * (object@stand_length + object@aisle_width)
      stand_x_end <- stand_x_start + object@stand_length
      stand_y_start <- row * (object@wall_shelf_width + object@aisle_width)
      stand_y_end <- stand_y_start + object@stand_width
      
      stand <- list(
        x = c(stand_x_start, stand_x_end),
        y = c(stand_y_start, stand_y_end),
        col = "darkgreen"
      )
      
      all_stands <- c(all_stands, list(stand))
    }
  }
  
  # chekoutts
  all_checkouts <- list()
  for (i in 1:object@num_checkouts) {
    checkout_x_end <- object@entrance_length
    checkout_y_start <- object@width - object@aisle_width - (i - 1) * (object@checkout_width + object@aisle_width)
    checkout_y_end <- checkout_y_start - object@checkout_width
    
    # calculate the left side of the checkout
    checkout_x_start <- checkout_x_end - object@checkout_length
    
    checkout <- list(
      x = c(checkout_x_start, checkout_x_end),
      y = c(checkout_y_start, checkout_y_end),
      col = "red"
    )
    
    all_checkouts <- c(all_checkouts, list(checkout))
  }
  
  # aisle shelves
  # I don't like these here, later change to object attributes
  num_aisle_shelves <- 2  # aisle shelves per row, 
  num_rows <- 4  # Number of rows of aisle shelves
  
  all_aisle_shelves <- list()
  
  for (row in 1:num_rows) {
    for (i in 1:num_aisle_shelves) {
      aisle_shelf_x_start <- object@entrance_length + object@aisle_width*2 + (i - 1) * (object@aisle_shelf_length + 2 * object@aisle_width)
      aisle_shelf_x_end <- aisle_shelf_x_start + object@aisle_shelf_length
      aisle_shelf_y_start <- (object@stand_width + object@aisle_width) * rows + object@aisle_width + (row - 1) * (object@aisle_shelf_width + object@aisle_width)
      aisle_shelf_y_end <- aisle_shelf_y_start + object@aisle_shelf_width
      
      aisle_shelf <- list(
        x = c(aisle_shelf_x_start, aisle_shelf_x_end),
        y = c(aisle_shelf_y_start, aisle_shelf_y_end),
        col = "grey"
      )
      
      all_aisle_shelves <- c(all_aisle_shelves, list(aisle_shelf))
    }
  }
  
  # vertical stands for bread cheese etc.
  # don't have these as objects yet, just an initial idea to fill space
  # still working on, at least position is correct now
  # num_aisle_shelves_vertical <- 3  # Number of vertical aisle shelves
  
  #all_aisle_shelves_vertical <- list()
  
  #for (i in 1:num_aisle_shelves_vertical) {
  #  aisle_shelf_x_start <- object@entrance_length + (i - 1) * (object@aisle_shelf_width + object@aisle_width)
  #  aisle_shelf_x_end <- aisle_shelf_x_start + object@aisle_shelf_width
  #  aisle_shelf_y_start <- object@width + object@wall_shelf_width + object@aisle_width
  #  aisle_shelf_y_end <- aisle_shelf_y_start - object@aisle_shelf_length
    
  #  aisle_shelf_vertical <- list(
  #    x = c(aisle_shelf_x_start, aisle_shelf_x_end),
  #    y = c(aisle_shelf_y_start, aisle_shelf_y_end),
  #    col = "grey"
  #  )
    
  #  all_aisle_shelves_vertical <- c(all_aisle_shelves_vertical, list(aisle_shelf_vertical))
#  }
  
  

  # make all objects to a list
  all_objects <- list(space, entrance, exit_border, bottom_shelf, top_shelf, right_shelf)
  all_objects <- c(all_objects, all_stands, all_checkouts, all_aisle_shelves)
  
  # Plot the environment
  plotSpace(all_objects)
})

# row_stands, row_shelves, col_shelves etc.can be given as arguments as well
# make it more flexible and adjustable 
new_store <- new("EnvironmentClass", num_stands = 3) # give num_stands as argument
PlotEnvironment(new_store)

# try to make it a plot
