# An S4 class to Simulate an Environment

# Load in the Predictive Pedestrian Package
# And all of it's dependencies
source("RCore/PredictivePedestrian.R")
rm(list = ls())

# Build a class that contains several features of a supermarket
# These are just some intial ideas for paramter input
# That I took from the minimal working example 

envir <-   setClass("environment_class", list(
  length = "numeric", # desired length of the environment
  width = "numeric",  # desired width of the environment 
  door_width = "numeric", # desired width of the door
  shelf_width = "numeric", # desired width of the shelves
  aisle_width = "numeric", # desired width of the aisles
  aisle_count = "numeric", # desired number of aisles
  cashregisters = "data.frame", # data frame with coordinates of the cash registers
  rectangles = "data.frame")) # data frame with coordinates of the shelves aisle + wall

# Specify the basline values for a supermarket
# Values here are taken from the minimal working example
# These values can be changed depending on
# What we want our baseline supermarket template to look like

setMethod("initialize",
          signature = "environment_class",
          function(.Object,
                   length = 25,
                   width = 40,
                   door_width = 2.5,
                   shelf_width = 1.2,
                   aisle_width = 3,
                   aisle_count = 6,
                   cashregisters = data.frame(0),
                   rectangles = data.frame(0)) {

            .Object@length <- length
            .Object@width <- width
            .Object@door_width <- door_width
            .Object@shelf_width <- shelf_width
            .Object@aisle_width <- aisle_width
            .Object@aisle_count <- aisle_count
            .Object@cashregisters <- cashregisters
            .Object@rectangles <- rectangles
            return(.Object)
          })

# A nicer overview of the elements that are in the
# environment_class object

setMethod("show",
          signature = "environment_class",
          function(object) {
            cat(crayon::bold("Environment Objects:"), "\n", "\n")
            cat("Environment Length:", object@length, "m", "\n")
            cat("Environment Width:", object@width, "m", "\n")
            cat("Environment Door Width:", object@door_width, "m", "\n")
            cat("Environment Shelf Width:", object@shelf_width, "m", "\n")
            cat("Environment Aisle Width:", object@aisle_width, "m", "\n")
            cat("Environment Aisle Count:", object@aisle_count, "\n")
            cat("Environment Cash Register Count:", object@cashregisters, "\n")
          })

# To check whether it works
test <- new("environment_class")

# Below follows a hard-coded ggplot of a potential supermarket
# The aim of this is to investigate what ggplot functions can be used
# To draw objects in the environment
# There are some preliminary learnings
# First, annotate is not handy to use when your plot has no real data
# otherwise the annotate function changes the x and y axis dimensions
# Second, geom_tile although taking in the xy coordinates, is quite complicated
# to use geom_tile you would have to take in a data.frame with x and y coordinates
# to draw the elements as desired. Cool route, but complicated & not necessarily user friendly

# Now the example of what I was working on
setGeneric("build_environment", function(object) { standardGeneric("build_environment")})
setMethod("build_environment", signature = "environment_class", function(object) {
     data <- data.frame(x = c(0, object@width),
                              y = c(0, object@length))
    
    p <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
     ggplot2::xlab("x") +
     ggplot2::ylab("y") +
     ggplot2::coord_equal() +
     ggplot2::geom_rect(xmin = 10, xmax = 20, ymin = 0, ymax = 2) + # wall shelf
     ggplot2::geom_rect(xmin = 25, xmax = 38, ymin = 0, ymax = 2) + # wall shelf
     ggplot2::geom_rect(xmin = 38, xmax = 40, ymin = 0, ymax = 25) + # wall shelf
     ggplot2::geom_rect(xmin = 26, xmax = 38, ymin = 23, ymax = 25) + # wall shelf
     ggplot2::geom_rect(xmin = 4, xmax = 10, ymin = 19, ymax = 21, fill = "red") + # cash register
     ggplot2::geom_rect(xmin = 4, xmax = 10, ymin = 15, ymax = 17, fill = "red") + # cash register
     ggplot2::geom_rect(xmin = 4, xmax = 10, ymin = 11, ymax = 13, fill = "red") + # cash register
     ggplot2::geom_rect(xmin = 4, xmax = 10, ymin = 7, ymax = 9, fill = "red") + # cash register
     ggplot2::scale_x_continuous(expand = c(0,0)) + # makes the origin at 0,0
     ggplot2::scale_y_continuous(expand =  c(0,0))
   p + ggplot2::geom_segment(x = 4, xend = 4, y = 0, yend = 25, linetype = "dashed") + # draw a shared entry/exit area
       ggplot2::geom_segment(x = 0, xend = 4, y = 6, yend = 6, linetype = "dashed", colour = "red") # barrier so that people entering cannot go here
})

empty_background <- build_environment(test)
empty_background

setGeneric("Generate", function(object) standardGeneric("Generate"))
setMethod("Generate", "environment_class", function(object) {
  
  # Create an empty background first
  p <- ggplot2::ggplot() +
  ggplot2::xlim(0, object@width) +    # Set x-axis limits
  ggplot2::ylim(0, object@length) +     # Set y-axis limits
  ggplot2::theme_bw() +    # Remove axis and background
  ggplot2::coord_equal() + # Sets the axis dimensions correctly
  ggplot2::labs(x = "x", y = "y")

  # Loop through each row of rectangle data and add rectangles to the plot
for (i in 1:nrow(object@cashregisters)) {
  p <- p +
    ggplot2::geom_rect(
      data = object@cashregisters[i,],  # Passing data for each rectangle
      ggplot2::aes(xmin = x_min, xmax = x_min + width, ymin = y_min, ymax = y_min + height),
      fill = "blue",
      color = "black"
    )
}

for (i in 1:nrow(object@shelves)) {
  p <- p +
    ggplot2::geom_rect(
      data = object@shelves[i,],  # Passing data for each rectangle
      ggplot2::aes(xmin = x_min, xmax = x_min + width, ymin = y_min, ymax = y_min + height),
      fill = "grey",
      color = "black"
    )
}


# Print the plot
print(p)

})

shelf <- data.frame(
  x_min = c(4, 4, 4, 10, 25, 26, 38), # X coordinate of bottom-left corner of each rectangle
  y_min = c(19, 15, 11, 0, 0, 23, 0), # Y coordinate of bottom-left corner of each rectangle
  width = c(6, 6, 6, 10, 13, 12, 2),  # Width of each rectangle
  height = c(2, 2, 2, 2, 2, 2, 25),   # Height of each rectangle
  fill = c("blue", "blue", "blue", "grey", "grey", "grey", "grey")
  )     

test <- new("environment_class", cashregisters = rect_data, rectangles = shelf)
Generate(test)


# An S4 class to plot the simulated environment

# Load in the Predictive Pedestrian Package
# And all of it's dependencies
source("RCore/PredictivePedestrian.R")
rm(list = ls())

# Build a class that can plot several features of a supermarket
# These are just some intial ideas for paramter input
# That I took from the minimal working example 

# @slot length; length of the environment
# @slot width;  width of the environment
# @slot circles; data frame with the agent positions in the ith iteration
# @slot rectangles; data fram with coordinates of simulated / specified location of rectangular objects
# @slot lines; data frame with coordinated of simulated lines  

plot_environment <- setClass("plot_environment", list(length = "numeric",
                                                      width = "numeric",
                                                      circles = "data.frame",
                                                      rectangles = "data.frame",
                                                      lines = "data.frame"))

# Initialise the class with some baseline values to plot an environment
setMethod("initialize",
          signature = "plot_environment",
          function(.Object,
                   length = 25,
                   width = 40,
                   circles = data.frame(0),
                   rectangles = data.frame(0),
                   lines = data.frame(0)) {

            .Object@length <- length
            .Object@width <- width
            .Object@circles <- circles
            .Object@rectangles <- rectangles
            .Object@lines <- lines
            return(.Object)
          })


# Some test data
shelf <- data.frame(
  x_min = c(4, 4, 4, 10, 25, 26, 38), # X coordinate of bottom-left corner of each rectangle
  y_min = c(19, 15, 11, 0, 0, 23, 0), # Y coordinate of bottom-left corner of each rectangle
  width = c(6, 6, 6, 10, 13, 12, 2),  # Width of each rectangle
  height = c(2, 2, 2, 2, 2, 2, 25),   # Height of each rectangle
  colour = c("blue", "blue", "blue", "grey", "grey", "grey", "grey")
  )

round <- data.frame(
    x = c(3, 7, 15),
    y = c(6, 10, 20),
    colour = c("blue", "green", "red")
)

test <- new("plot_environment", circles = round, rectangles = shelf)

# Make plot functions for individual objects & background
# Plot Method for an empty background

setGeneric("background", function(object) {standardGeneric("background")})
setMethod("background", "plot_environment", function(object) {
  # Create an empty background first
  p <- ggplot2::ggplot() +
  ggplot2::xlim(0, object@width) +    # Set x-axis limits
  ggplot2::ylim(0, object@length) +     # Set y-axis limits
  ggplot2::theme_bw() +    # Remove axis and background
  ggplot2::coord_equal() + # Sets the axis dimensions correctly
  ggplot2::labs(x = "x", y = "y") # Sets the axis labels
})

test_plot <- background(test)

# Plot Method for rectangular shapes
setGeneric("plot_rectangle", function(object, plot) {standardGeneric("plot_rectangle")})
setMethod("plot_rectangle", "plot_environment", function(object, plot) {
  for (i in 1:nrow(object@rectangles)) { 
  plot <- plot + ggplot2::geom_rect(
      data = object@rectangles[i,],  # Passing data for each rectangle
      ggplot2::aes(xmin = x_min, xmax = x_min + width, ymin = y_min, ymax = y_min + height),
      fill = object@rectangles$colour[i], # different colour for the different types of rectangular objects
      color = "black"
    ) 
}
print(plot)
})

test_plot <- plot_rectangle(test, test_plot)

# Plot Method for circular shapes
setGeneric("plot_circle", function(object, plot) {standardGeneric("plot_circle")})
setMethod("plot_circle", "plot_environment", function(object, plot) {
  for (i in 1:nrow(object@circles)) {
    plot <- plot + ggplot2::geom_point(
        data = object@circles, # passing data for each circle to be drawn
        ggplot2::aes(x = x, y = y, colour = colour, size = 3), # size is something we still need to think about
        show.legend = FALSE,
    )
  }
print(plot)
})

test_plot <- plot_circle(test, test_plot)

# Plot Method for lines to demarcate areas
setGeneric("plot_lines", function(object, plot) {standardGeneric("plot_lines")})
setMethod("plot_lines", "plot_environment", function(object, plot) {
    for (i in 1:nrow(object@lines)) {
        plot <- plot + ggplot2::geom_segment(x = x, xend = x, y = y, yend = yend, linetype = type)
    }
print(plot)
})