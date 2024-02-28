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


# Here I started to create an empty environment
# The aim is to add in more of the elements as specified above
# Will continue to work on this tomorrow
setGeneric("create_environment", function(object) { standardGeneric("create_environment")})
setMethod("create_environment", signature = "environment_class", function(object) {
  # This setup is taken from the minimal working example
  # It is quite neat when converted to this S4 class object
  space <- list(x = c(-.5, object@length + .5),
                y = c(-.5, object@width + .5))
  # This should be converted to the cashregisters
  # Not quite sure on how to do this yet, so I will look into it
  exitCounter <- list(x = c(-.5, object@door_width),
          y = c(-object@shelf_width + object@width / 1.65, object@width / 1.65))
  # This will build the shelves in the supermarket environment
  wallShelves <- list(
                    # vertical divider shelf 1
  list(x = c(-.5, object@length / 2.3),
       y = c(-object@shelf_width + object@width / 2.2, object@width / 2.2)),
                    # bottom wall shelf
  list(x = c(object@aisle_width, object@length - object@aisle_width),
       y = c(-.5, object@shelf_width - .5)),
                    # right wall shelf
  list(x = c(object@length - object@shelf_width, object@length + .5),
       y = c(2 * object@aisle_width, object@width)),
                    # left wall bottom shelf
  list(x = c(-.5, object@shelf_width),
       y = c(-.5 + object@aisle_width,
             -object@shelf_width + object@aisle_width / 2.2)),
                    # left wall top shelf
  list(x = c(-.5, object@shelf_width),
       y = c(.5 + object@width - 1.5 * object@aisle_width,
             .5 + object@width - 1.5 * object@door_width)),
                    # top wall left shelf
  list(x = c(-.5, (object@length / 2.5)),
       y = c(.5 + object@width - 1.5 * object@door_width, object@width + .5)),
                    # top wall right shelf
  list(x = c(object@length / 2.5, object@length + .5),
       y = c(object@width, object@width + .5)))
  # This will build the internal shelves in the supermarket
  internalShelves <- list(
                        # vertical divider shelf 2
  list(x = c(object@length / 2.1 + object@aisle_width, 
  object@length - object@aisle_width),
       y = c(-object@shelf_width + object@width / 2.2, object@width / 2.2)),
                        # bottom right vert midshelf
  list(x = c(object@length - 2 * object@shelf_width, 
  object@length - object@shelf_width),
       y = c(object@door_width, 1.5 * object@aisle_width)),
                        # bottom left midshelf 1
  list(x = c(1.5 * object@aisle_width, object@length / 2.2),
       y = c(object@shelf_width + object@door_width, 2 * object@shelf_width + object@door_width)),
                        # bottom left midshelf 2
  list(x = c(1.5 * object@aisle_width, object@length / 2.2),
       y = c(2 * object@shelf_width + 2 * object@door_width, 3 * object@shelf_width + 2 * object@door_width)),
                        # bottom right midshelf 1
  list(x = c(object@length / 2 + object@door_width, object@length - 1.5 * object@aisle_width),
       y = c(object@shelf_width + object@door_width, 2 * object@shelf_width + object@door_width)),
                        # bottom right midshelf 2
  list(x = c(object@length / 2 + object@door_width, object@length - 1.5 * object@aisle_width),
       y = c(2 * object@shelf_width + 2 * object@door_width, 3 * object@shelf_width + 2 * object@door_width)),
                        # top left midshelf 1
  list(x = c(1.5 * object@aisle_width, object@length / 2.1),
       y = c(object@width - (object@shelf_width + 2.3 * object@door_width),
             object@width - 2.3 * object@door_width)),
                        # top left midshelf 2
  list(x = c(1.5 * object@aisle_width, object@length / 2.1),
       y = c(object@width - (object@shelf_width + 3.8 * object@door_width),
             object@width - 3.8 * object@door_width)),
                        # top left midshelf 3
  list(x = c(1.5 * object@aisle_width, object@length / 2.1),
       y = c(object@width - (object@shelf_width + 5.3 * object@door_width),
             object@width - 5.3 * object@door_width)),
                        # top right midshelf 1
  list(x = c(object@length / 2.1 + object@aisle_width,
  object@length - object@aisle_width),
       y = c(object@width - (object@shelf_width + .8 * object@door_width),
             object@width - .8 * object@door_width)),
                        # top right midshelf 2
  list(x = c(object@length / 2.1 + object@aisle_width,
             object@length - object@aisle_width),
       y = c(object@width - (object@shelf_width + 2.3 * object@door_width),
             object@width - 2.3 * object@door_width)),
                        # top right midshelf 3
  list(x = c(object@length / 2.1 + object@aisle_width,
             object@length - object@aisle_width),
       y = c(object@width - (object@shelf_width + 3.8 * object@door_width),
             object@width - 3.8 * object@door_width)),
                        # top right midshelf 4
  list(x = c(object@length / 2.1 + object@aisle_width,
             object@length - object@aisle_width),
       y = c(object@width - (object@shelf_width + 5.3 * object@door_width),
             object@width - 5.3 * object@door_width)))
  objects <- c(list(space),      # overall space
               list(exitCounter),  # exit counter
               wallShelves,        # shelves on walls
               internalShelves)    # internal shelves 
  # this function is taken from pp_plot.R, and plots the environment
  environment <- plotSpace(objects) # nolint
  # This did not pan out as I had planned, so I am abadoning this idea
  ggplot2::ggsave(filename = "test_plot.png", plot = environment, height = (object@length * 100), width = (object@width * 100), units = "px")
})

test <- new("environment_class")
create_environment(test)

# This is my alternative idea, that works for now
# Should this be persued as the solution? 
setGeneric("build_environment", function(object) { standardGeneric("build_environment")})
setMethod("build_environment", signature = "environment_class", function(object) {
     background <- data.frame(x = c(0, object@width),
                              y = c(0, object@length))
    p <- ggplot2::ggplot(background, ggplot2::aes(x = background$x, y = background$y)) +
     ggplot2::theme_bw() +
     ggplot2::xlab("x") +
     ggplot2::ylab("y") +
     ggplot2::coord_flip() # To get the scaling right
})

empty_background <- build_environment(test)
