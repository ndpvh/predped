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
                    name = "character",
                    Color = "character",
                    bCA = "numeric",
                    aCA = "numeric",
                    bCAlr = "numeric",
                    bGA = "numeric",
                    aGA = "numeric",
                    bBA = "numeric",
                    aBA = "numeric",
                    bID = "numeric",
                    aID = "numeric",
                    sSlow = "numeric",
                    sPref = "numeric",
                    bPS = "numeric",
                    aPS = "numeric",
                    bFL = "numeric",
                    aFL = "numeric",
                    bWB = "numeric",
                    aWB = "numeric",
                    dID = "numeric",
                    dFL = "numeric",
                    rU = "numeric",
                    bS = "numeric",
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

agent_model <- new("model")
