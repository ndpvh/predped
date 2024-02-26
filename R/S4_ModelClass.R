# Create an S4 Class for AgentModel

# set directoty
setwd("/Users/eceyatikci/predped/predped/R")

# get agent and parameters
source("/Users/eceyatikci/predped/predped/R/agent.R")
source("/Users/eceyatikci/predped/predped/R/parameters.R")

setClass("AgentModel",
  list(
    environment = "matrix", # represent environment as matrix
    agents = "agent", # holds instances of the "agent" class
    trace = "list", # set trace to store simulation data 
    # add parameters
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
  )
)


# initialize model
setGeneric("InitialModel", function(.Object) standardGeneric("InitialModel"))
setMethod("InitialModel", "AgentModel", function(.Object,
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
  # empty environment
  .Object@environment <- matrix(0, nrow = NULL, ncol = NULL)
  # empty agents
  .Object@agents <- list()
  # set empty trace
  .Object@trace <- list()
  # parameters
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


# add parameter transformation options to the model
setGeneric("TransformParams", function(.Object, transform) standardGeneric("TransformParams"))
setMethod("TransformParams", "AgentModel", function(.Object, transform) {
  # it can be "mu", "exponentiate", "logarithmic"
  if (transform == "mu") {
    .Object <- transform_mu(.Object)
  } else if (transform == "exponentiate") {
    .Object <- transform_exponentiate(.Object)
  } else if (transform == "logarithmic") {
    .Object <- transform_logarithmic(.Object)
  } else {
    warning("Invalid transformation method, the parameters remain the same.")
  }
  return(.Object)
})

# initialize simulation
setGeneric("InitialSim", function(.Object) standardGeneric("InitialSim"))
setMethod("InitialSim", "AgentModel", function(.Object) {
  # empty environment matrix for now
  .Object@environment <- matrix(0, nrow = 10, ncol = 10) 
  # empty list of agents
  .Object@agents <- list()

  return(.Object)
})

# set simulation method
setGeneric("simulate", function(.Object, numSteps) standardGeneric("simulate"))
setMethod("simulate", "AgentModel", function(.Object, numSteps) {
  for (i in 1:numSteps) {
    MoveAgents(.Object) # move agent
    RecordTrace(.Object) # save to trace
  }
})

# move agent method
setGeneric("MoveAgents", function(.Object) standardGeneric("MoveAgents"))
setMethod("MoveAgents", "AgentModel", function(.Object) {
  for (agent in .Object@agents) {
    move_agent(agent) # move function
  }
})

# save method
setGeneric("RecordTrace", function(.Object) standardGeneric("RecordTrace"))
setMethod("RecordTrace", "AgentModel", function(.Object) {
  # save current state of the environment and agents
  .Object$trace <- c(.Object$trace,
                     list(environment = .Object$environment,
                          agents = .Object$agents))
})