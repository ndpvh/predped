#' An S4 Class to Represent the State
#'
#' @slot iteration An integer denoting the iteration number of the state
#' @slot setting An instance of \code{\link[predped]{background-class}}
#' @slot agents A list containing instances of the \code{\link[predped]{agent-class}}
#' 
#' @rdname state-class
#' 
#' @export
state <- setClass("state", list(iteration = "numeric", 
                                setting = "background",
                                agents = "list"))

setMethod("initialize", "state", function(.Object, 
                                          iteration,
                                          setting, 
                                          agents = list()) {

    # Some checks on the objects
    if(length(agents) != 0 & !all(sapply(agents, is, class2 = "agent"))) {
        stop("All elements in the list for slot 'agents' must be of type 'agent'")
    }

    .Object@iteration <- floor(iteration)
    .Object@setting <- setting
    .Object@agents <- agents

    return(.Object)
})

#' Getter/Setter for the iteration-slot
#' 
#' @rdname iteration-method
#' 
#' @export
setGeneric("iteration", function(object) standardGeneric("iteration"))

#' @rdname iteration-method
#' 
#' @export
setGeneric("iteration<-", function(object, value) standardGeneric("iteration<-"))

setMethod("iteration", "state", function(object) {
    return(object@iteration)
})

setMethod("iteration<-", "state", function(object, value) {
    object@iteration <- floor(value)
    return(object)
})

#' Getter/Setter for the setting-slot
#' 
#' @rdname setting-method
#' 
#' @export
setGeneric("setting", function(object) standardGeneric("setting"))

#' @rdname setting-method
#' 
#' @export
setGeneric("setting<-", function(object, value) standardGeneric("setting<-"))

setMethod("setting", "state", function(object) {
    return(object@setting)
})

setMethod("setting<-", "state", function(object, value) {
    # Check
    if(!inherits(value, "background")) {
        stop("Provided value for slot `setting` should be of class `background`.")
    }

    object@setting <- value
    return(object)
})

#' Getter/Setter for the agents-slot
#' 
#' @rdname agents-method
#' 
#' @export
setGeneric("agents", function(object) standardGeneric("agents"))

#' @rdname agents-method
#' 
#' @export
setGeneric("agents<-", function(object, value) standardGeneric("agents<-"))

setMethod("agents", "state", function(object) {
    return(object@agents)
})

setMethod("agents<-", "state", function(object, value) {
    # Check
    if(length(value) != 0 & !all(sapply(value, is, class2 = "agent"))) {
        stop("All elements in the list for slot 'agents' must be of type 'agent'")
    }

    object@agents <- value
    return(object)
})