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
#' @param id Character that serves as an identifier for the trace.
#' @param time_step Numerical denoting the time that passes at each iteration in
#' seconds.
#' @param setting Object of the \code{\link[predped]{background-class}}.
#' @param states List of lists containing the state of the 
#' \code{\link[predped]{agent-class}}s at each iteration. 
#' @param variables List of list of user-specified variables that are used to 
#' control the simulation (see the \code{fx} argument of 
#' \code{\link[predped]{simulate}}) at each iteration.
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
#'   id = "my trace"
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
                                          setting, 
                                          id = character(0),
                                          time_step = 0.5,
                                          states = list(),
                                          variables = list()) {

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
    cat("id:", object@id, "\n")
    cat("time step:", object@time_step, "\n")
    cat("number of iterations:", length(object@states), "\n")
    cat("variables accounted for:\n")
    cat("    ", names(object@variables), "\n\n")
    cat("For more information, please call the attributes of the trace separately.")
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