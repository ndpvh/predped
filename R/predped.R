# S4 class model function to simulate an agent-based model
#
# First, set a model class that takes all of the agent parameters
# or takes in default parameters.
#
# Second, build a simulate method that is akin to the minimal working
# example. Use the scripts that are available in the GitHub.

#' An S4 Class to Represent the M4MA model.
#' 
#' The `predped` class defines the M4MA model that can be used to simulate 
#' data from. It contains the setting in which the agents should walk around as 
#' well as the parameters of those agents. At this moment, only supports the use
#' of archetypes: Prototypical agents that have a given parameter set.
#'
#' @slot id A character that can be used to identify the specific model defined
#' by the class
#' @slot setting A dataframe containing the objects that define the 
#' setting in which the agents will walk around
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
                                    setting = "background",
                                    parameters = "data.frame",
                                    archetypes = "character",
                                    weights = "numeric"))

setMethod("initialize", "predped", function(.Object,
                                            id,
                                            setting, 
                                            parameters = params_archetypes,
                                            archetypes = unique(params_archetypes$Name),
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

    if(!all(archetypes %in% parameters$Name)) {
        stop("Some archetypes cannot be found in the parameters list.")
    }

    .Object@id <- id 
    .Object@setting <- setting

    # Select only those parameters that matter for the simulation: Based on the 
    # `archetypes` argument. 
    #
    # To make sure we get to the same order as the one in `archetypes`, we first 
    # select only the archetypes from this vector, then we transform this into 
    # a numeric through factorization with `archetype` as its levels, and then 
    # transform this to a character again. 
    parameters <- parameters[parameters$Name %in% archetypes,]
    idx <- factor(parameters$Name, levels = archetypes)
    idx <- order(as.numeric(idx))

    .Object@parameters <- parameters[idx,] 

    .Object@archetypes <- archetypes
    .Object@weights <- weights
    
    return(.Object)
})

#' @rdname predped-class
#'
#' @export
setMethod("id", "predped", function(object) {
    return(object@id)
})

#' @rdname predped-class
#'
#' @export
setMethod("id<-", "predped", function(object, value) {
    object@id <- value
    return(object)
})

#' @rdname predped-class
#'
#' @export
setGeneric("setting", function(object) standardGeneric("setting"))

#' @rdname predped-class
#'
#' @export
setGeneric("setting<-", function(object, value) standardGeneric("setting<-"))

setMethod("setting", "predped", function(object) {
    return(object@setting)
})

setMethod("setting<-", "predped", function(object, value) {
    object@setting <- value
    return(object)
})

#' @rdname predped-class
#'
#' @export
setMethod("parameters", "predped", function(object) {
    return(object@parameters)
})

#' @rdname predped-class
#'
#' @export
setMethod("parameters<-", "predped", function(object, value) {
    # First check whether the archetypes still add up
    if(!all(archetypes %in% value)) {
        stop("Some archetypes not defined in the new parameters provided.")
    }

    object@parameters <- value
    return(object)
})

#' @rdname predped-class
#'
#' @export
setGeneric("archetypes", function(object) standardGeneric("archetypes"))

#' @rdname predped-class
#'
#' @export
setGeneric("archetypes<-", function(object, value) standardGeneric("archetypes<-"))

setMethod("archetypes", "predped", function(object) {
    return(object@archetypes)
})

setMethod("archetypes<-", "predped", function(object, value) {
    # Given that parameters and weights are both dependent on archetypes, 
    # reinitialize the object to accommodate the change 
    return(predped(object@id, 
                   object@setting, 
                   parameters = object@parameters, 
                   archetypes = value, 
                   weights = object@weights))
})

#' @rdname predped-class
#'
#' @export
setGeneric("weights", function(object) standardGeneric("weights"))

#' @rdname predped-class
#'
#' @export
setGeneric("weights<-", function(object, value) standardGeneric("weights<-"))

setMethod("weights", "predped", function(object) {
    return(object@weights)
})

setMethod("weights<-", "predped", function(object, value) {
    object@weights <- value
    return(object)
})

#' @rdname predped-class
#'
#' @export
#
# TO DO: Beautify the output
setMethod("show", "predped", function(object) {
    cat("Model object:\n")
    cat("ID:", object@id, "\n")
    cat("Parameters: \n")
    cat(write.table(object@parameters), "\n")

    return(object) # object is returned invisibly
})
