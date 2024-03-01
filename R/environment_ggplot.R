# An S4 class to Simulate an Environment

# Load in the Predictive Pedestrian Package
# And all of it's dependencies
source("RCore/PredictivePedestrian.R")

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
  cashregisters = "numeric" # desired number of cash registers
))

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
                   cashregisters = 4) {

            .Object@length <- length
            .Object@width <- width
            .Object@door_width <- door_width
            .Object@shelf_width <- shelf_width
            .Object@aisle_width <- aisle_width
            .Object@aisle_count <- aisle_count
            .Object@cashregisters <- cashregisters
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
   p + ggplot2::geom_segment(x = 4, xend =4, y = 0, yend = 25, linetype = "dashed") + # draw a shared entry/exit area
       ggplot2::geom_segment(x = 0, xend = 4, y = 6, yend = 6, linetype = "dashed", colour = "red") # barrier so that people entering cannot go here
})

empty_background <- build_environment(test)
empty_background