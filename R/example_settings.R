# create an S4 class for the environment

# change these
setwd("/Users/eceyatikci/predped/predped")
source("/Users/eceyatikci/predped/predped/RCore/pp_plot.R")

# Create an environment class
environment_class <-   setClass("EnvironmentClass", slots = list(
  
  # main environment characteristics
  length = "numeric", # length of environment
  width = "numeric",  # width of environment
  main_door_width = "numeric", # width of main door
  entrance_length = "numeric", # entrance area length
  entrance_width = "numeric", # entrance area width
  entrance_door_width = "numeric", # width of the entrance door
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
  
  # Determine the range for the x and y axes
  x_range <- c(0, 50)
  y_range <- x_range
  
  # Empty plot with equal aspect ratio
  plot(NA, xlim = x_range, ylim = y_range, xlab = "x", ylab = "y", bty = "n", asp = 1)
  
  # add grid
  if (plotGrid) {
    myGrid(list(x = x_range, y = y_range))
  }
  
  # add objects, if there are objects
  if (length(objects) > 1) {
    for (i in 2:length(objects)) {
      if (length(objects[[i]]) > 0) {  # check if the object is not empty
        if (is.null(objects[[i]]$col) || is.na(objects[[i]]$col)) {
          rect(objects[[i]]$x[1], objects[[i]]$y[1], objects[[i]]$x[2], objects[[i]]$y[2], border = objects[[i]]$border, lwd = objects[[i]]$lwd, lty = objects[[i]]$lty)
        } else {
          rect(objects[[i]]$x[1], objects[[i]]$y[1], objects[[i]]$x[2], objects[[i]]$y[2], col = objects[[i]]$col, border = objects[[i]]$border, lwd = objects[[i]]$lwd)
        }
      }
    }
  }
  
  # plot overall space, after objects so lines are on top
  drawSquare(objects[[1]], col = NA, border = "#636363", lwd = 2)
}


# initializiton method for baseline store
setMethod("initialize",
          signature = "EnvironmentClass",
          function(.Object,
                   length = 40,
                   width = 25,
                   main_door_width = 2.5,
                   entrance_length = 8,
                   entrance_width = 10,
                   entrance_door_width = 1.5,
                   num_aisles = 6,
                   aisle_width = 3,
                   max_num_agents = 100,
                   min_num_agents = 10,
                   wall_shelf_length = 20,
                   aisle_shelf_length = 15,
                   wall_shelf_width = 1.2,
                   aisle_shelf_width = 2,
                   num_stands = 4,
                   stand_length = 2,
                   stand_width = 1.5,
                   num_checkouts = 4,
                   checkout_length = 3,
                   checkout_width = 1.5) {
            
            # assign values to slots
            .Object@length <- length
            .Object@width <- width
            .Object@main_door_width <- main_door_width
            .Object@entrance_length <- entrance_length
            .Object@entrance_width <- entrance_width
            .Object@entrance_door_width <- entrance_door_width
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
setGeneric("PlotEnvironment", function(object) { standardGeneric("PlotEnvironment")}) # nolint: line_length_linter.

setMethod("PlotEnvironment", signature = "EnvironmentClass", function(object) {
  # the boundaries of space
  space <- list(x = c(0, object@length),
                y = c(0, object@width))
  # the boundaries of entrance & exit area
  entrance <- list(x = c(0, object@entrance_length),
                   y = c(0, object@entrance_width),
                   lty = "dashed")
  # wall shelves
  bottom_shelf <-
    list(x = c(object@entrance_length, object@length - object@entrance_length),
         y = c(object@wall_shelf_width, 0),
         col = "grey")
  top_shelf <-
    list(x = c(object@entrance_length * 2, object@length - object@entrance_length),
         y = c(object@width, object@width - object@wall_shelf_width),
         col = "grey")
  
  # plot the environment
  plotSpace(list(space, entrance, bottom_shelf, top_shelf))
})

new_store <- new("EnvironmentClass")
PlotEnvironment(new_store)
