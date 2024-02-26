# S4 class model function to simulate an agent-based model
#
# First, set a model class that takes all of the agent parameters
# or takes in default parameters.
#
# Second, build a simulate method that is akin to the minimal working
# example. Use the scripts that are available in the GitHub.
#

model <- setClass("model",
                  list(
                    name = "character", # Name of the agent to be simulated
                    Color = "character", # Color of the agent in the simulation
                    bCA = "numeric", # Current direction
                    aCA = "numeric", # Current direction
                    bCAlr = "numeric", # Current direction
                    bGA = "numeric", # Goal Angle
                    aGA = "numeric", # Goal Angle
                    bBA = "numeric", # Blocked angle
                    aBA = "numeric", # Blocked angle
                    bID = "numeric", # Interpersonal distance
                    aID = "numeric", # Interpersonal distance
                    sSlow = "numeric", # Preferred velocity
                    sPref = "numeric", # Preferred velocity
                    bPS = "numeric", # Goal angle
                    aPS = "numeric", # Goal angle
                    bFL = "numeric", # Follow the leader
                    aFL = "numeric", # Follow the leader
                    bWB = "numeric", # Walk beside
                    aWB = "numeric", # Walk beside
                    dID = "numeric", # Interpersonal Distance
                    dFL = "numeric", # Follow the leader
                    rU = "numeric", # Utility randomness
                    bS = "numeric", # Stand still threshold
                    pReroute = "numeric"
                  ))

setMethod("initialize", 
          signature = "model", 
          function(.Object,
                   name = character(0),
                   Color = "#0000ffc5",
                   bCA = 1,
                   aCA = 2,
                   bCAlr = 1,
                   bGA = 10,
                   aGA = 2,
                   bBA = 4,
                   aBA = 2,
                   bID = 1,
                   aID = 2,
                   sSlow = 1,
                   sPref = 1.25,
                   bPS = 2,
                   aPS = 2,
                   bFL = 1,
                   aFL = 2,
                   bWB = 0,
                   aWB = 2,
                   dID = 0,
                   dFL = 0,
                   rU = 0.001,
                   bS = 100,
                   pReroute = 0) {
  
  .Object@name <- name
  .Object@Color <- Color
  .Object@bCA <- bCA
  .Object@aCA <- aCA
  .Object@bCAlr <- bCAlr
  .Object@bGA <- bGA
  .Object@aGA <- aGA
  .Object@bBA <- bBA
  .Object@aBA <- aBA
  .Object@bID <- bID
  .Object@aID <- aID
  .Object@sSlow <- sSlow
  .Object@sPref <- sPref
  .Object@bPS <- bPS
  .Object@aPS <- aPS
  .Object@bFL <- bFL
  .Object@aFL <- aFL
  .Object@bWB <- bWB
  .Object@aWB <- aWB
  .Object@dID <- dID
  .Object@dFL <- dFL
  .Object@rU <- rU
  .Object@bS <- bS
  .Object@pReroute <- pReroute
  
  return(.Object)
})

setMethod("show", signature = "model", function(object) {
  cat("Model object:\n")
  cat("Name:", object@name, "\n")
  cat("Color:", object@Color, "\n")
  cat("bCA:", object@bCA, "\n")
  cat("aCA:", object@aCA, "\n")
  cat("bCAlr:", object@bCAlr, "\n")
  cat("bGA:", object@bGA, "\n")
  cat("aGA:", object@aGA, "\n")
  cat("bBA:", object@bBA, "\n")
  cat("aBA:", object@aBA, "\n")
  cat("bID:", object@bID, "\n")
  cat("aID:", object@aID, "\n")
  cat("sSlow:", object@sSlow, "\n")
  cat("sPref:", object@sPref, "\n")
  cat("bPS:", object@bPS, "\n")
  cat("aPS:", object@aPS, "\n")
  cat("bFL:", object@bFL, "\n")
  cat("aFL:", object@aFL, "\n")
  cat("bWB:", object@bWB, "\n")
  cat("aWB:", object@aWB, "\n")
  cat("dID:", object@dID, "\n")
  cat("dFL:", object@dFL, "\n")
  cat("rU:", object@rU, "\n")
  cat("bS:", object@bS, "\n")
  cat("pReroute:", object@pReroute, "\n")

  return(object) # object is returned invisibly
})

agent_model <- new("model", name = "Niels")

setGeneric("simulate", function(gstack, p, pSD, group, types)
             standardGeneric("simulate"))


# The idea should shift to taking in a matrix object for parameters
# Setting agent_names to the rownames and individual parameters to columns
# This is achievable I believe

model_class <- setClass("model_class", list(
  parameters = "data.frame",
  environment = "list"
))

new_model <- new("model_class", parameters = data, environment = list(0))

setMethod("initialize", "model_class",
          function(.Object, parameters, environment) {
            rownames(parameters) <- paste()
          })

setMethod("Rownames", "model_class")

library(tidyverse)
data <- tibble(names = c("Dave", "Gerald", "Jane", "James"), bBA = rnorm(4))
data <- as.data.frame(data)


n_mod <- new("model_class", parameters = data, environment = list(0))

rownames(data) <- data$names
