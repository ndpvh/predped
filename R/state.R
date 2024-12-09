#' An S4 Class to Represent the State
#'
#' @slot iteration Numeric denoting the iteration number. 
#' @slot setting Object of the \code{\link[predped]{background-class}}.
#' @slot agents List containing objects of the \code{\link[predped]{agent-class}}
#' representing the agents that are currently walking around in the \code{setting}.
#' @slot potential_agents List containing objects of the 
#' \code{\link[predped]{agent-class}} representing agents that are waiting to 
#' enter the \code{setting}.
#' @slot iteration_variables Dataframe containing values for variables that 
#' control the simulation under the hood, such as \code{max_agents}.
#' 
#' @seealso 
#' \code{\link[predped]{agents}},
#' \code{\link[predped]{initialize,state-method}},
#' \code{\link[predped]{iteration}},
#' \code{\link[predped]{potential_agents}},
#' \code{\link[predped]{setting}}
#' 
#' @rdname state-class
#' 
#' @export
state <- setClass("state", list(iteration = "numeric", 
                                setting = "background",
                                agents = "list",
                                potential_agents = "list",
                                iteration_variables = "data.frame", 
                                variables = "list"))

#' Constructor for the \code{\link[predped]{state-class}}
#' 
#' @param iteration Numeric denoting the iteration number that this state 
#' represents. Makes it possible to order states into one coherent trace, showing
#' how agents walked around over time.
#' @param setting Object of the \code{\link[predped]{background-class}}.
#' @param agents List containing objects of the \code{\link[predped]{agent-class}}
#' representing the agents that are currently walking around in the \code{setting}.
#' Defaults to an empty list.
#' @param potential_agents List containing objects of the 
#' \code{\link[predped]{agent-class}} representing agents that are waiting to 
#' enter the \code{setting}. Defaults to an empty list.
#' @param iteration_variables Dataframe containing values for variables that 
#' control the simulation under the hood, such as \code{max_agents}. Defaults 
#' to an empty data.frame.
#' @param variables Named list containing variables that you want to use to 
#' control the simulation in the \code{fx} argument of the 
#' \code{\link[predped]{simulate,predped-method}}. Defaults to an empty list.
#' 
#' @return Object of the \code{\link[predped]{state-class}}
#' 
#' @examples
#' # Create a background in which agents will walk around
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)))
#' 
#' # Initialize state
#' my_state <- state(iteration = 0,
#'                   setting = my_background)
#' 
#' # Access the two slots that were specified
#' my_state@iteration
#' my_state@agents
#' 
#' @seealso 
#' \code{\link[predped]{state-class}},
#' \code{\link[predped]{agents}},
#' \code{\link[predped]{iteration}},
#' \code{\link[predped]{potential_agents}},
#' \code{\link[predped]{setting}}
#' 
#' @rdname initialize-state-method
#' 
#' @export
setMethod("initialize", "state", function(.Object, 
                                          iteration,
                                          setting, 
                                          agents = list(),
                                          potential_agents = list(),
                                          iteration_variables = data.frame(), 
                                          variables = list()) {

    # Some checks on the objects
    if(length(agents) != 0 & !all(sapply(agents, is, class2 = "agent"))) {
        stop("All elements in the list for slot 'agents' must be of type 'agent'")
    }

    if(length(agents) != 0 & !all(sapply(potential_agents, is, class2 = "agent"))) {
        stop("All elements in the list for slot 'potential_agents' must be of type 'agent'")
    }

    .Object@iteration <- floor(iteration)
    .Object@setting <- setting
    .Object@agents <- agents
    .Object@potential_agents <- potential_agents
    .Object@iteration_variables <- iteration_variables # Having this as a complete dataframe is not my preferred way of doing things, but does allow users to change specifications on the fly
    .Object@variables <- variables

    return(.Object)
})





################################################################################
# GETTERS AND SETTERS

#' @rdname agents-method
setMethod("agents", "state", function(object) {
    return(object@agents)
})

#' @rdname agents-method
setMethod("agents<-", "state", function(object, value) {
    # Check
    if(length(value) != 0 & !all(sapply(value, is, class2 = "agent"))) {
        stop("All elements in the list for slot 'agents' must be of type 'agent'")
    }

    object@agents <- value
    return(object)
})



#' @rdname iteration-method
setMethod("iteration", "state", function(object) {
    return(object@iteration)
})

#' @rdname iteration-method
setMethod("iteration<-", "state", function(object, value) {
    object@iteration <- floor(value)
    return(object)
})



#' @rdname iteration_variables-method
setMethod("iteration_variables", "state", function(object) {
    return(object@iteration_variables)
})

#' @rdname iteration-method
setMethod("iteration_variables<-", "state", function(object, value) {
    object@iteration_variables <- floor(value)
    return(object)
})



#' @rdname potential_agents-method
setMethod("potential_agents", "state", function(object) {
    return(object@potential_agents)
})

#' @rdname potential_agents-method
setMethod("potential_agents<-", "state", function(object, value) {
    # Check
    if(length(value) != 0 & !all(sapply(value, is, class2 = "agent"))) {
        stop("All elements in the list for slot 'potential_agents' must be of type 'agent'")
    }

    object@potential_agents <- value
    return(object)
})



#' @rdname setting-method
setMethod("setting", "state", function(object) {
    return(object@setting)
})

#' @rdname setting-method
setMethod("setting<-", "state", function(object, value) {
    # Check
    if(!inherits(value, "background")) {
        stop("Provided value for slot `setting` should be of class `background`.")
    }

    object@setting <- value
    return(object)
})



#' @rdname variables-method
setMethod("variables", "state", function(object) {
    return(object@variables)
})

#' @rdname variables-method
setMethod("variables<-", "state", function(object, value) {
    object@variables <- value
    return(object)
})