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
                                    parameters = "list",
                                    archetypes = "character",
                                    weights = "numeric"))

setMethod("initialize", "predped", function(.Object,
                                            setting, 
                                            id = character(0),
                                            database = NULL, 
                                            path_to_database = file.path("..",
                                                                         "predpedgui",
                                                                         "build", 
                                                                         "Qt_6_7_2_for_macOS-Debug"),
                                            archetypes = NULL,
                                            weights = NULL) {

    # If not provided, create an id for the model
    .Object@id <- if(length(id) == 0) paste("model", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id 

    # Load the parameters
    params <- load_parameters(x = database, 
                              path_to_database = path_to_database)

    # If archetypes are not provided, switch to the the default of including 
    # everyone 
    if(is.null(archetypes)) {
        archetypes <- params[["params_archetypes"]]$name
    }

    # First check imposed: When not all provided archetypes are known, we cannot
    # proceed
    if(!all(archetypes %in% params[["params_archetypes"]]$name)) {
        stop("Some archetypes cannot be found in the parameters list.")
    }

    # Now that the archetypes are known, we can delete those archetypes that 
    # don't really matter
    params[["params_archetypes"]] <- dplyr::filter(params[["params_archetypes"]], 
                                                   name %in% archetypes)
    params[["params_sigma"]] <- params[["params_sigma"]][archetypes]

    # If the weights are not provided, then we have several possbilities. First, 
    # we take the weights as defined in the data.frame of the parameters. If 
    # these are not defined, then we switch to the default of having everyone 
    # being equally likely to be put in the simulation. 
    #
    # Whenever weights is provided, this takes priority: When weights are 
    # defined in the parameter data.frame, these are overrided by the ones 
    # provided in this initialization function.
    if(is.null(weights) & !is.null(params[["params_archetypes"]]$weight)) {
        weights <- params[["params_archetypes"]]$weight
    } else if(is.null(weights)) {
        weights <- rep(1 / length(archetypes), each = length(archetypes))
    }

    # Second check imposed: From now on, the number of archetypes and weights 
    # should be equal
    if(length(archetypes) != length(weights)) {
        stop("Number of `archetypes` should be equal to number of weights.")
    }

    # Check whether some of the weights are 0. If so, then we want to delete 
    # these archetypes from the complete list.
    idx <- weights != 0
    weights <- weights[idx]
    archetypes <- archetypes[idx]

    # A final check concerns whether the weights add up to 1. If not, then we 
    # simply reweight so that they do
    if(sum(weights) != 1) {
        message("Weights did not add up to 1. Weights slot is reweighted to sum to 1.")
        weights <- weights / sum(weights)
    }

    # Finally, we switch the order in the parameters data.frame to match the 
    # order in `archetypes`. 
    #
    # First, we transform the archetypes into a numeric through factorization 
    # with `archetype` as its levels and then transform this to a character again. 
    idx <- factor(params[["params_archetypes"]]$name, levels = archetypes)
    idx <- order(as.numeric(idx))
    params[["params_archetypes"]] <- params[["params_archetypes"]][idx,]

    .Object@setting <- setting
    .Object@archetypes <- archetypes
    .Object@weights <- weights
    .Object@parameters <- params
    
    return(.Object)
})

setMethod("id", "predped", function(object) {
    return(object@id)
})

setMethod("id<-", "predped", function(object, value) {
    object@id <- value
    return(object)
})

setMethod("setting", "predped", function(object) {
    return(object@setting)
})

setMethod("setting<-", "predped", function(object, value) {
    object@setting <- value
    return(object)
})

setMethod("parameters", "predped", function(object) {
    return(object@parameters)
})

setMethod("parameters<-", "predped", function(object, value) {
    # First check whether the archetypes still add up
    if(!all(object@archetypes %in% value[["params_archetypes"]]$name) |
       !all(object@archetypes %in% names(value[["params_sigma"]]))) {
        stop("Some archetypes not defined in the new parameters provided.")
    }

    object@parameters <- value
    return(object)
})

#' Getter/Setter for the archetypes-slot
#' 
#' @rdname archetypes-method
#'
#' @export
setGeneric("archetypes", function(object) standardGeneric("archetypes"))

#' @rdname archetypes-method
#'
#' @export
setGeneric("archetypes<-", function(object, value) standardGeneric("archetypes<-"))

setMethod("archetypes", "predped", function(object) {
    return(object@archetypes)
})

setMethod("archetypes<-", "predped", function(object, value) {
    # Throw an error if you don't have the parameters for the provided 
    # archetypes in your data.frame already
    if(!all(value %in% object@parameters[["params_archetypes"]]$name) | 
       !all(value %in% names(object@parameters[["params_sigma"]]))) {
        stop(paste0("Parameters for the provided archetype cannot be found. ", 
                    "Consider creating a new predped model."))
    }

    # Change both the archetypes and the weights, as both are affected by the 
    # change. As you don't know which weights should be coupled to the new 
    # archetypes, you just assume that each should be equally weighted
    object@archetypes <- value
    object@weights <- rep(1 / length(value), length(value))

    return(object)
})

#' Getter/Setter for the weights-slot
#' 
#' @rdname weights-method
#'
#' @export
setGeneric("weights", function(object) standardGeneric("weights"))

#' @rdname weights-method
#'
#' @export
setGeneric("weights<-", function(object, value) standardGeneric("weights<-"))

setMethod("weights", "predped", function(object) {
    return(object@weights)
})

setMethod("weights<-", "predped", function(object, value) {
    if(length(value) != length(object@weights)) {
        stop("Provided weights should be of equal size as the provided archetypes.")
    }

    object@weights <- value
    return(object)
})

# TO DO: Beautify the output
setMethod("show", "predped", function(object) {
    cat("Model object:\n")
    cat("ID:", object@id, "\n")
    cat("Parameters: \n")
    cat(write.table(object[["params_archetypes"]]@parameters), "\n")

    return(object) # object is returned invisibly
})
