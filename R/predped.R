# S4 class model function to simulate an agent-based model
#
# First, set a model class that takes all of the agent parameters
# or takes in default parameters.
#
# Second, build a simulate method that is akin to the minimal working
# example. Use the scripts that are available in the GitHub.

#' An S4 Class to Represent the M4MA model
#'
#' @slot id A character that can be used to identify the specific model defined
#' by the class
#' @slot environment A dataframe containing the objects that define the 
#' environment in which the agents will walk around
#' @slot parameters A dataframe containing the parameters per archetype in its
#' columns. Defaults to the `params_archetype` dataframe.
#' @slot archetypes A vector of names for the archetypes one wants to use in the 
#' simulation. Defaults to all archetypes in the `params_archetype` dataframe.
#' @slot weights A numeric vector containing the probability with which an agent 
#' can be of a given archetype. The weights should be in the same order as the 
#' `archetypes` argument. Defaults to an equal weighting of all archetypes in 
#' `params_archetype`
#' 
#' @export
predped <- setClass("predped", list(id = "character",
                                    environment = "data.frame",
                                    parameters = "data.frame",
                                    archetypes = "character",
                                    weights = "numeric"))

setMethod("initialize", "predped", function(.Object,
                                            id,
                                            environment, 
                                            parameters = params_archetypes,
                                            archetypes = unique(params_archetypes$names),
                                            weights = rep(1/length(archetypes), 
                                                          each = length(archetypes))
) {

    # Check arguments
    if(length(archetypes) != length(weights)) {
        stop("Number of `archetypes` should be equal to number of weights.")
    }

    if(sum(weights) != 1) {
        stop("Weights should add up to 1.")
    }

    if(!all(archetypes %in% parameters$names)) {
        stop("Some archetypes cannot be found in the parameters list.")
    }

    .Object@id <- id 
    .Object@environment <- environment

    # Select only those parameters that matter for the simulation: Based on the 
    # `archetypes` argument. 
    #
    # To make sure we get to the same order as the one in `archetypes`, we first 
    # select only the archetypes from this vector, then we transform this into 
    # a numeric through factorization with `archetype` as its levels, and then 
    # transform this to a character again. 
    parameters <- parameters[parameters$name %in% archetypes,]
    idx <- factor(parameters, levels = archetypes)
    idx <- order(as.numeric(idx))

    .Object@parameters <- parameters[idx,] 

    .Object@archetypes <- archetypes
    .Object@weights <- archetypes
    
    return(.Object)
})

#' @rdname predped
#'
#' @export
setGeneric("id", function(object) standardGeneric("id"))

#' @rdname predped
#'
#' @export
setGeneric("id<-", function(object, value) standardGeneric("id<-"))

setMethod("id", "predped", function(object) {
    return(setNames(object@id, object@id))
})

setMethod("id<-", "predped", function(object, value) {
    object@id <- value
    return(object)
})

#' @rdname predped
#'
#' @export
setGeneric("environment", function(object) standardGeneric("environment"))

#' @rdname predped
#'
#' @export
setGeneric("environment<-", function(object, value) standardGeneric("environment<-"))

setMethod("environment", "predped", function(object) {
    return(setNames(object@environment, object@environment))
})

setMethod("environment<-", "predped", function(object, value) {
    object@environment <- value
    return(object)
})

#' @rdname predped
#'
#' @export
setGeneric("parameters", function(object) standardGeneric("parameters"))

#' @rdname predped
#'
#' @export
setGeneric("parameters<-", function(object, value) standardGeneric("parameters<-"))

setMethod("parameters", "predped", function(object) {
    return(setNames(object@parameters, object@parameters))
})

setMethod("parameters<-", "predped", function(object, value) {
    # First check whether the archetypes still add up
    if(!all(archetypes %in% value)) {
        stop("Changing the parameters will lead to some archetypes not being defined anymore.")
    }

    object@parameters <- value
    return(object)
})

#' @rdname predped
#'
#' @export
setGeneric("archetypes", function(object) standardGeneric("archetypes"))

#' @rdname predped
#'
#' @export
setGeneric("archetypes<-", function(object, value) standardGeneric("archetypes<-"))

setMethod("archetypes", "predped", function(object) {
    return(setNames(object@archetypes, object@archetypes))
})

setMethod("archetypes<-", "predped", function(object, 
                                              archetypes) {
    # Given that parameters and weights are both dependent on archetypes, 
    # reinitialize the object to accommodate the change 
    return(predped(object@id, 
                   object@environment, 
                   parameters = object@parameters, 
                   archetypes = archetypes, 
                   weights = object@weights))
})

#' @rdname predped
#'
#' @export
setGeneric("weights", function(object) standardGeneric("weights"))

#' @rdname predped
#'
#' @export
setGeneric("weights<-", function(object, value) standardGeneric("weights<-"))

setMethod("weights", "predped", function(object) {
    return(setNames(object@weights, object@weights))
})

setMethod("weights<-", "predped", function(object, value) {
    object@weights <- value
    return(object)
})

# Shows what the object looks like:
#
# TO DO: Beautify the output
setMethod("show", "predped", function(object) {
  cat("Model object:\n")
  cat("ID:", object@id, "\n")
  cat("Parameters:", object@parameters, "\n")

  return(object) # object is returned invisibly
})

################################################################################
# To examine later

# ALEXANDER
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

# ECE
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