#' An S4 Class to Represent a Trace.
#' 
#' Defines the \code{trace} class, which contains all characteristics of a trace, 
#' including the setting, the agents at each iteration, and the time between each
#' iteration.
#'
#' @slot id Character that serves as an identifier for the trace.
#' @slot time_step Numerical denoting the time that passes at each iteration in
#' seconds.
#' @slot setting Object of the \code{\link[predped]{background-class}}.
#' @slot states List of lists containing the state of the 
#' \code{\link[predped]{agent-class}}s at each iteration. 
#' @slot variables List of list of user-specified variables that are used to 
#' control the simulation (see the \code{fx} argument of 
#' \code{\link[predped]{simulate}}) at each iteration.
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{background-class}},
#' \code{\link[predped]{state-class}},
#' \code{\link[predped]{initialize-trace}}
#' \code{\link[predped]{simulate}}
#' 
#' @rdname trace-class
#' 
#' @concept classes
#'
#' @export
trace <- setClass("trace",
                  list(id = "character",
                       time_step = "numeric",
                       setting = "background",
                       states = "list",
                       variables = "list"))

#' Constructor for the \code{\link[predped]{trace-class}}
#' 
#' @param .Object For this class, should be left unspecified (see Example).
#' @param id Character that serves as an identifier for the trace. Defaults to 
#' an indicator \code{"trace"} pasted together with a random 5-letter string.
#' @param time_step Numerical denoting the time that passes at each iteration in
#' seconds. Defaults to \code{0.5}.
#' @param setting Object of the \code{\link[predped]{background-class}}. Defaults
#' to \code{NULL}, in which case the constructor will throw an error as this is
#' a slot that needs to be specified.
#' @param states List of lists containing the state of the 
#' \code{\link[predped]{agent-class}}s at each iteration. Defaults to an empty 
#' list.
#' @param variables List of list of user-specified variables that are used to 
#' control the simulation (see the \code{fx} argument of 
#' \code{\link[predped]{simulate}}) at each iteration. Defaults to an empty list.
#' 
#' @return Object of the \code{\link[predped]{trace-class}}
#' 
#' @examples
#' # Create a background 
#' setting <- background(
#'   shape = rectangle(center = c(0, 0), size = c(5, 5)),
#'   objects = list(circle(center = c(0, 0), radius = 1))
#' )
#' 
#' # Initialize trace
#' my_trace <- trace(
#'   id = "my trace",
#'   time_step = 1,
#'   setting = setting
#' )
#' 
#' # Access some of the slots that were specified
#' my_trace@id
#' my_trace@time_step
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{background-class}},
#' \code{\link[predped]{state-class}},
#' \code{\link[predped]{trace-class}}
#' \code{\link[predped]{simulate}}
#' 
#' @rdname initialize-trace
#' 
#' @concept classes
#' 
#' @export
setMethod("initialize", "trace", function(.Object,
                                          setting = NULL, 
                                          id = character(0),
                                          time_step = 0.5,
                                          states = list(),
                                          variables = list()) {

    # If no setting is provided, throw an error
    if(is.null(setting)) {
        stop("A setting needs to be defined in order to define a trace.")
    }

    # Check whether any states/variables have been defined. If so, then the other 
    # needs to match the length

    .Object@id <- ifelse(length(id) == 0,
                         paste(
                            "trace", 
                            paste(sample(letters, 5, replace = TRUE), collapse = "")
                         ),
                         id)
    .Object@time_step <- time_step
    .Object@setting <- setting
    .Object@states <- states
    .Object@variables <- variables

    return(.Object)
})

#' Show method for the \code{\link[predped]{trace-class}}
#' 
#' @param object Object of the \code{\link[predped]{trace-class}}
#' 
#' @concept methods
#' 
#' @export
setMethod("show", "trace", function(object) {
    cat("Trace Attributes", "\n")
    cat("  id:", object@id, "\n")
    cat("  time step:", object@time_step, "\n")
    cat("  number of iterations:", length(object@states), "\n")
    cat("  variables accounted for:\n")
    cat("    ", names(object@variables[[length(object@variables)]]), "\n\n")
    cat("For more information, please call the attributes of the trace separately.\n")
})

#' Append values to the trace
#' 
#' Method used for appending values to the \code{states} and \code{variables} 
#' slots of a \code{\link[predped]{trace-class}}. Note that this method can only 
#' append a single additional state to the trace. 
#' 
#' @param object Object of the \code{\link[predped]{trace-class}} to which you 
#' would like to append the values of \code{states} and \code{variables}.
#' @param state Object of the \code{\link[predped]{state-class}} containing the 
#' current state that you would like to append to the trace.
#' @param agents List of instances of the \code{\link[predped]{agent-class}}
#' containing the agents at the current state. Defaults to \code{NULL}, meaning
#' meaning no agents are found at the current trace.
#' @param variables Named list containing the values of the variables at the 
#' current state. Defaults to \code{NULL}, meaning no variables are currently 
#' tracked in the trace.
#' @param ... Additional arguments specified in the methods.
#' 
#' @return Object of the \code{\link[predped]{trace-class}} with appended values 
#' for its slots \code{states} and \code{variables}. Note that if both the 
#' arguments \code{agents} and \code{variables} are NULL, empty lists are added
#' to both slots.
#' 
#' @examples 
#' # This is my example
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{state-class}},
#' \code{\link[predped]{trace-class}}
#' 
#' @rdname append_trace
#' 
#' @concept helper
#' 
#' @export
setGeneric("append_trace", function(object, state, ...) standardGeneric("append_trace"))

#' @rdname append_trace
setMethod("append_trace", 
          signature(object = "trace"), 
          function(object, 
                   agents = NULL, 
                   variables = NULL) {
    
    # Transform the NULLs to empty lists
    if(is.null(agents)) {
        agents <- list()
    }

    if(is.null(variables)) {
        variables <- list()
    }

    # Check whether both slots are actually lists. If not, appending cannot be 
    # performed
    if(!inherits(agents, "list") | !inherits(variables, "list")) {
        stop("The provided `agents` or `variables` are not lists. Cannot proceed.")
    }

    # Add these lists to the corresponding slots.
    N <- length(object@states)
    object@states[[N + 1]] <- agents
    object@variables[[N + 1]] <- variables

    return(object)
})

#' @rdname append_trace
setMethod("append_trace", 
          signature(object = "trace", state = "state"), 
          function(object, 
                   state) {

    # Add these lists to the corresponding slots.
    N <- length(object@states)
    object@states[[N + 1]] <- state@agents
    object@variables[[N + 1]] <- state@variables

    return(object)
})






################################################################################
# GETTERS AND SETTERS

#' @rdname id
setMethod("id", "trace", function(object) {
    return(object@id)
})

#' @rdname id
setMethod("id<-", "trace", function(object, value) {
    object@id <- value
    return(object)
})



#' @rdname setting
setMethod("setting", "trace", function(object) {
    return(object@setting)
})

#' @rdname setting
setMethod("setting<-", "trace", function(object, value) {
    # Perform a check before allowing this: If the `states` slot is not empty, 
    # then throw an incompatibility warning
    if(length(object@states) > 0) {
        warning(paste("The `states` slot of this trace is not empty.", 
                      "Note that changing the setting in non-empty traces may lead to",
                      "issues in continuing simulations or in estimating the model's parameters."))
    } 

    object@setting <- value
    return(object)
})



#' @rdname states
setMethod("states", "trace", function(object) {
    return(object@states)
})

#' @rdname states
setMethod("states<-", "trace", function(object, value) {
    # Perform a check before allowing this: If the new value for the `states` 
    # slot does not match the variables slot, then you cannot perform this 
    # operation in good conscience
    if(length(object@variables) != length(value)) {
        stop("The provided value for the slot `states` does not have the same ",
             "length as the `variables` slot in the trace. ", 
             "Both need to be the same length.")
    }

    object@states <- value
    return(object)
})



#' @rdname time_step
setMethod("time_step", "trace", function(object) {
    return(object@time_step)
})

#' @rdname time_step
setMethod("time_step<-", "trace", function(object, value) {
    # Perform a check before allowing this: If the `states` slot is not empty, 
    # then throw an incompatibility warning
    if(length(object@states) > 0) {
        warning(paste("The `states` slot of this trace is not empty.", 
                      "Note that changing the time step in non-empty traces may lead to",
                      "issues in continuing simulations or in estimating the model's parameters."))
    } 

    object@time_step <- value
    return(object)
})



#' @rdname variables
setMethod("variables", "trace", function(object) {
    return(object@variables)
})

#' @rdname variables
setMethod("variables<-", "trace", function(object, value) {
    # Perform a check before allowing this: If the new value for the `variables` 
    # slot does not match the states slot, then you cannot perform this 
    # operation in good conscience
    if(length(object@states) != length(value)) {
        stop("The provided value for the slot `variables` does not have the same ",
             "length as the `states` slot in the trace. ", 
             "Both need to be the same length.")
    }

    object@variables <- value
    return(object)
})