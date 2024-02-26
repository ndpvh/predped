# An S4 class to Simulate an Environment

# Load in the Predictive Pedestrian Package
# And all of it's dependencies
source("RCore/PredictivePedestrian.R")
install.packages("crayon")
library(crayon)

# Build a class that contains several features of a supermarket
# These are just some intial ideas for paramter input
# That I took from the minimal working example 

envir <-   setClass("Environment_Class", list(
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
          signature = "Environment_Class",
          function(.Object,
                   length = 40,
                   width = 25,
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
# Environment_Class object

setMethod("show",
          signature = "Environment_Class",
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


# Here I started to create an empty environment
# The aim is to add in more of the elements as specified above
# Will continue to work on this tomorrow

setGeneric("Create_Environment", function(object) { standardGeneric("Create_Environment")})
setMethod("Create_Environment", signature = "Environment_Class", function(object) {
  # This setup is taken from the minimal working example
  # It is quite neat when converted to this S4 class object
  space <- list(x = c(-.5, object@length + .5),
                y = c(-.5, object@width + .5))
  plotSpace(list(space)) # this function is taken from pp_plot.R
})

test <- new("Environment_Class")
