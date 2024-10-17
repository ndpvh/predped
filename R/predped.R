#' An S4 Class to Represent the M4MA model.
#' 
#' The \code{predped} class defines some of the settings that will be used when 
#' simulating data with the M4MA model. It contains the setting in which the 
#' agents should walk around as well as the parameters of those agents. At this 
#' moment, only supports the use of archetypes: Prototypical agents that have a 
#' given parameter set (see \code{\link[predped]{generate_parameters}} and 
#' \code{\link[predped]{params_from_csv}}).
#'
#' @slot id Character that defines an identifier for the model.
#' @slot setting Object of the \code{\link[predped]{background-class}}.
#' @slot parameters Dataframe containing the parameters per archetype in its
#' columns.
#' @slot archetypes Character or character vector containing the names of the 
#' archetypes that should be included in the simulation. 
#' @slot weights Numeric vector containing the probability with which an agent 
#' of each archetype can be selected to wander around in the environment. The 
#' weights should be in the same length as the \code{archetypes}-slot. 
#' 
#' @seealso 
#' \code{\link[predped]{initialize,predped-method}}
#' 
#' @rdname predped-class
#' 
#' @export
predped <- setClass("predped", list(id = "character",
                                    setting = "background",
                                    parameters = "list",
                                    archetypes = "character",
                                    weights = "numeric"))

#' Constructor for the \code{\link[predped]{predped-class}}
#' 
#' @param setting Object of the \code{\link[predped]{background-class}}.
#' @param id Character that serves as an identifier for the agent. Defaults to 
#' an empty character, triggering the random generation of an id.
#' @param filename Character denoting the path to personalized parameters. 
#' Defaults to \code{NULL}, triggering reading in the csv-files. 
#' @param sep Character denoting the separator of the delimited file in 
#' \code{filename}. If \code{filename} is not provided or if \code{filename} is
#' not a delimited file, this argument is ignored. Defaults to \code{","}.
#' @param archetypes Character or character vector denoting the archetype(s) 
#' you want to include in the simulation. Defaults to \code{NULL}, triggering 
#' the inclusion of all currently defined archetypes.
#' @param weights Numeric vector containing the probability with which an agent 
#' of each archetype can be selected to wander around in the environment. The 
#' weights should be in the same length as the \code{archetypes} argument.
#' Defaults to an equal probability for each of the archetypes. 
#' 
#' @return Object of the \code{\link[predped]{predped-class}}
#' 
#' @examples
#' # Initialize predped model
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list())
#' 
#' my_model <- predped(setting = my_background, 
#'                     archetypes = c("BaselineEuropean", 
#'                                    "DrunkAussie"), 
#'                     weights = c(0.9, 0.1))
#' 
#' # Access the two slots that were specified
#' my_model@id
#' head(my_model@parameters)
#' my_model@weights
#' 
#' @seealso 
#' \code{\link[predped]{background-class}},
#' \code{\link[predped]{predped-class}},
#' \code{\link[predped]{generate_parameters}},
#' \code{\link[predped]{load_parameters}},
#' \code{\link[predped]{params_from_csv}}
#' 
#' @rdname initialize-predped-method
#' 
#' @export
setMethod("initialize", "predped", function(.Object,
                                            setting, 
                                            id = character(0),
                                            filename = NULL,
                                            sep = ",",
                                            archetypes = NULL,
                                            weights = NULL) {

    # If not provided, create an id for the model
    .Object@id <- if(length(id) == 0) paste("model", paste0(sample(letters, 5, replace = TRUE), collapse = "")) else id 

    # Load the parameters
    params <- load_parameters(x = filename, sep = sep)

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
    tmp <- params[["params_archetypes"]]
    params[["params_archetypes"]] <- tmp[tmp$name %in% archetypes, ]
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

#' Show method for the \code{\link[predped]{predped-class}}
#' 
#' @export
setMethod("show", "predped", function(object) {
    cat("Model object:\n")
    cat("ID:", object@id, "\n")
    cat("Parameters: \n")
    cat(write.table(object[["params_archetypes"]]@parameters), "\n")
})





################################################################################
# GETTERS AND SETTERS

#' @rdname id-method
setMethod("id", "predped", function(object) {
    return(object@id)
})

#' @rdname id-method
setMethod("id<-", "predped", function(object, value) {
    object@id <- value
    return(object)
})



#' @rdname setting-method
setMethod("setting", "predped", function(object) {
    return(object@setting)
})

#' @rdname setting-method
setMethod("setting<-", "predped", function(object, value) {
    object@setting <- value
    return(object)
})



#' @rdname parameters-method
setMethod("parameters", "predped", function(object) {
    return(object@parameters)
})

#' @rdname parameters-method
setMethod("parameters<-", "predped", function(object, value) {
    # First check whether the archetypes still add up
    if(!all(object@archetypes %in% value[["params_archetypes"]]$name) |
       !all(object@archetypes %in% names(value[["params_sigma"]]))) {
        stop("Some archetypes not defined in the new parameters provided.")
    }

    object@parameters <- value
    return(object)
})



#' @rdname archetypes-method
setMethod("archetypes", "predped", function(object) {
    return(object@archetypes)
})

#' @rdname archetypes-method
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



#' @rdname weights-method
setMethod("weights", "predped", function(object) {
    return(object@weights)
})

#' @rdname weights-method
setMethod("weights<-", "predped", function(object, value) {
    if(length(value) != length(object@weights)) {
        stop("Provided weights should be of equal size as the provided archetypes.")
    }

    object@weights <- value
    return(object)
})
