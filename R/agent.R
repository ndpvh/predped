#' An S4 Class to Represent Agents.
#' 
#' Defines the \code{agent} class, which contains all characteristics of an agent. 
#' Some of these characteristics are time-independent (e.g., parameters and 
#' set of goals) while others change during each time step in the simulation or 
#' estimation (e.g., speed and orientation).
#'
#' @slot id Character that serves as an identifier for the agent.
#' @slot center Numerical vector of two elements denoting the current position
#' of the agent (x and y coordinate).
#' @slot radius Numeric denoting the size of agent. As agents are circular, this 
#' is equal to the radius of the agent.
#' @slot speed Numeric denoting the current speed of the agent (in m/sec).
#' @slot orientation Numeric denoting the orientation of the agent (in degrees)
#' @slot group Numeric indicating the group to which the agent belongs. 
#' Influences the behavior of the agent through the social utility functions.
#' @slot status Character denoting the current status of the agent, or what they
#' will be doing to make the next decision. Is one of "move" (the agent will 
#' move to another cell), "plan" (the agent will plan the route to their next 
#' goal), "reroute" (the agent will replan the route of their current goal), 
#' "reorient" (the agent will check other movement options), "completing goal"
#' (the agent will interact with their goal), "exit" (the agent will exit the 
#' space), or "wait" (the agent will wait for another agent to finish their goal 
#' and step out of the way).
#' @slot waiting_counter Numeric defining how long the agent will remain in the 
#' status "wait" once triggered. At the start of the waiting period is set to be 
#' equal to the number of iterations the other agent still has to interact with 
#' their goal plus 2.
#' @slot cell Numeric denoting the cell the agent will move to.
#' @slot current_goal Object of class \code{\link[predped]{goal-class}} which
#' represents the current goal of the agent.
#' @slot goals List of goals the agent has to achieve. The current goal is not 
#' included in this list.
#' @slot parameters Dataframe containing the values of the parameters for the 
#' agent. Should contain all parameters relevant for the utility functions 
#' (use \code{predped::draw_parameters(1)} for an example).
#' @slot color Character denoting the color in which the agent should be plotted.
#' 
#' @seealso 
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{goal-class}},
#' \code{\link[predped]{object-class}},
#' \code{\link[predped]{cell}},
#' \code{\link[predped]{current_goal}},
#' \code{\link[predped]{goals}},
#' \code{\link[predped]{group}},
#' \code{\link[predped]{id}},
#' \code{\link[predped]{orientation}},
#' \code{\link[predped]{parameters}}
#' \code{\link[predped]{position}},
#' \code{\link[predped]{size}},
#' \code{\link[predped]{speed}},
#' \code{\link[predped]{status}},
#' \code{\link[predped]{waiting_counter}},
#' \code{\link[predped]{initialize,agent-method}}
#' 
#' @rdname agent-class
#'
#' @export
agent <- setClass("agent", 
                  list(id = "character",
                       speed = "numeric",
                       orientation = "numeric",
                       group = "numeric",
                       status = "character",
                       waiting_counter = "numeric",
                       cell = "numeric",
                       current_goal = "goal",
                       goals = "list", 
                       parameters = "data.frame",
                       color = "character",
                       cell_centers = "matrix",
                       utility_variables = "data.frame"), 
                  contains = c("circle"))

#' Constructor for the \code{\link[predped]{agent-class}}
#' 
#' @param center Numerical vector of two elements denoting the current position
#' of the agent (x and y coordinate).
#' @param radius Numeric denoting the size of agent. As agents are circular, this 
#' is equal to the radius of the agent.
#' @param id Character that serves as an identifier for the agent. Defaults to 
#' an empty character, triggering the random generation of an id.
#' @param speed Numeric denoting the current speed of the agent (in m/sec). 
#' Defaults to \code{0.1}.
#' @param orientation Numeric denoting the orientation of the agent (in degrees).
#' Defaults to \code{0}.
#' @param current_goal Object of the \code{\link[predped]{goal-class}}. 
#' Defaults to \code{NULL}, triggering the creation of a placeholder.
#' @param goals List containing instances of the \code{\link[predped]{goal-class}}.
#' Defaults to an empty list.
#' @param group Numeric indicating the group to which the agent belongs. 
#' Influences the behavior of the agent through the social utility functions.
#' Defaults to \code{0}.
#' @param status Character denoting the current status of the agent, or what they
#' will be doing to make the next decision. Is one of \code{"move"} (the agent 
#' will move to another cell), \code{"plan"} (the agent will plan the route to 
#' their next goal), \code{"reroute"} (the agent will replan the route of their 
#' current goal), \code{"reorient"} (the agent will check other movement 
#' options), \code{"completing goal"} (the agent will interact with their goal), 
#' \code{"exit"} (the agent will exit the space), or \code{"wait"} (the agent 
#' will wait for another agent to finish their goal and step out of the way). 
#' Defaults to \code{"move"}.
#' @param waiting_counter Numeric defining how long the agent will remain in the 
#' status "wait" once triggered. At the start of the waiting period is set to be 
#' equal to the number of iterations the other agent still has to interact with 
#' their goal plus 2. Defaults to \code{0}
#' @param cell Numeric denoting the cell the agent will move to. Defaults to 
#' \code{0}.
#' @param current_goal Object of class \code{\link[predped]{goal-class}} which
#' represents the current goal of the agent. Defaults to a placeholder of class 
#' class \code{\link[predped]{goal-class}}.
#' @param goals List of goals the agent has to achieve. The current goal is not 
#' included in this list. Defaults to an empty list.
#' @param parameters Dataframe containing the values of the parameters for the 
#' agent. Should contain all parameters relevant for the utility functions 
#' (use \code{predped::draw_parameters(1)} for an example). Defaults to a random 
#' set of parameters from the default parameter list in \code{predped}.
#' @param color Character denoting the color in which the agent should be plotted.
#' 
#' @return Object of the \code{\link[predped]{agent-class}}
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), radius = 0.25)
#' 
#' # Access the two slots that were specified
#' my_agent@center
#' my_agent@radius
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{cell}},
#' \code{\link[predped]{current_goal}},
#' \code{\link[predped]{goals}},
#' \code{\link[predped]{group}},
#' \code{\link[predped]{id}},
#' \code{\link[predped]{orientation}},
#' \code{\link[predped]{parameters}}
#' \code{\link[predped]{position}},
#' \code{\link[predped]{size}},
#' \code{\link[predped]{speed}},
#' \code{\link[predped]{status}},
#' \code{\link[predped]{waiting_counter}}
#' 
#' @rdname initialize-agent-method
#' 
#' @export
setMethod("initialize", "agent", function(.Object,
                                          center, 
                                          radius,
                                          id = character(0),
                                          speed = 0.1,
                                          orientation = 0,
                                          current_goal = NULL,
                                          goals = list(),
                                          group = 0,
                                          status = "move",
                                          waiting_counter = 0,
                                          cell = 0,
                                          parameters = data.frame(),                                   
                                          color = "black",
                                          cell_centers = matrix(0, nrow = 33, ncol = 2),
                                          utility_variables = data.frame()) {

    # Use the circular object as the basis of the agent     
    .Object <- callNextMethod(.Object, 
                              center = center, 
                              radius = radius, 
                              moveable = TRUE, 
                              interactable = TRUE)

    # Fill all agent-specific slots
    .Object@id <- if(length(id) == 0) paste(sample(letters, 5, replace = TRUE), collapse = "") else id
    .Object@speed <- speed
    .Object@orientation <- orientation
    .Object@goals <- goals
    .Object@group <- group
    .Object@status <- status
    .Object@waiting_counter <- waiting_counter
    .Object@cell <- cell    
    .Object@color <- color
    .Object@cell_centers <- cell_centers
    .Object@utility_variables <- data.frame()

    # If the parameters are empty, add the BaselineEuropean as default. Otherwise
    # use the defined parameters
    if(nrow(parameters) == 0) {
        params <- params_from_csv[["params_archetypes"]]
        .Object@parameters <- params[params$name == "BaselineEuropean", ]
    } else {
        .Object@parameters <- parameters
    }
    
    # Only add the provided current goal if it is defined.
    if(!is.null(current_goal)) {
        .Object@current_goal <- current_goal
    }

    return(.Object)
})

#' Show method for the \code{\link[predped]{agent-class}}
#' 
#' @export
setMethod("show", "agent", function(object) {
    params <- parameters(object)
    cols <- colnames(params)
    params <- as.numeric(params)
    names(params) <- cols
    params <- as.matrix(params)

    cat(crayon::bold("Agent Attributes"), "\n")
    cat("center:", object@center, "\n")
    cat("cell:", object@cell, "\n")
    cat("color:", object@color, "\n")
    cat("current_goal: (a) position:", object@current_goal@position, "(b) path:", object@current_goal@path, "\n")
    cat("goals (number):", length(object@goals), "\n")
    cat("group:", object@group, "\n")
    cat("id:", object@id, "\n")
    cat("orientation:", object@orientation, "\n")
    cat("parameters:", "\n")
    if(is.null(params)){
        print("Not defined")
    } else {
        print(params)
    }
    cat("\nradius:", object@radius, "\n")
    cat("speed:", object@speed, "\n")
    cat("status:", object@status, "\n")
    cat("\nFor more detailed information, please extract the wanted information from the agent directly.\n")
})





################################################################################
# GETTERS AND SETTERS

#' @rdname cell-method
setMethod("cell", "agent", function(object) {
    return(setNames(object@cell, object@id))
})

#' @rdname cell-method
setMethod("cell<-", "agent", function(object, value) {
    object@cell <- value
    return(object)
})



#' @rdname cell_centers-method
setMethod("cell_centers", "agent", function(object) {
    return(object@cell_centers)
})

#' @rdname cell_centers-method
setMethod("cell_centers<-", "agent", function(object, value) {
    object@cell_centers <- value
    return(object)
})



#' @rdname color-method
setMethod("color", "agent", function(object) {
    return(setNames(object@color, object@id))
})

#' @rdname color-method
setMethod("color<-", "agent", function(object, value) {
    object@color <- value
    return(object)
})



#' @rdname current_goal-method
setMethod("current_goal", "agent", function(object) {
    return(object@current_goal)
})

#' @rdname current_goal-method
setMethod("current_goal<-", "agent", function(object, value) {
    object@current_goal <- value
    return(object)
})



#' @rdname goals-method
setMethod("goals", "agent", function(object) {
    return(object@goals)
})

#' @rdname goals-method
setMethod("goals<-", "agent", function(object, value) {
    object@goals <- value
    return(object)
})



#' @rdname group-method
setMethod("group", "agent", function(object) {
    return(setNames(object@group, object@id))
})

#' @rdname group-method
setMethod("group<-", "agent", function(object, value) {
    object@group <- value
    return(object)
})



#' @rdname id-method
setMethod("id", "agent", function(object) {
    return(setNames(object@id, object@id))
})

#' @rdname id-method
setMethod("id<-", "agent", function(object, value) {
    object@id <- value
    return(object)
})



#' @rdname orientation-method
setMethod("orientation", "agent", function(object) {
    return(setNames(object@orientation, object@id))
})

#' @rdname orientation-method
setMethod("orientation<-", "agent", function(object, value) {
    object@orientation <- value
    return(object)
})



#' @rdname parameters-method
setMethod("parameters", "agent", function(object) {
    return(object@parameters)
})

#' @rdname parameters-method
setMethod("parameters<-", "agent", function(object, value) {
    object@parameters <- value
    return(object)
})



#' @rdname position-method
setMethod("position", "agent", function(object, return_matrix = FALSE) {
    if (return_matrix) {
        return(matrix(object@center, nrow = 1, ncol = 2, dimnames = list(object@id, names(object@center))))
    }
    return(object@center)
})

#' @rdname position-method
setMethod("position<-", "agent", function(object, value) {
    object@center <- as(value, "coordinate")
    return(object)
})



#' @rdname size-method
setMethod("size", "agent", function(object) {
    return(setNames(object@radius, object@id))
})

#' @rdname size-method
setMethod("size<-", "agent", function(object, value) {
    object@radius <- value
    return(object)
})



#' @rdname speed-method
setMethod("speed", "agent", function(object) {
    return(setNames(object@speed, object@id))
})

#' @rdname speed-method
setMethod("speed<-", "agent", function(object, value) {
    object@speed <- value
    return(object)
})



#' @rdname status-method
setMethod("status", "agent", function(object) {
    return(setNames(object@status, object@id))
})

#' @rdname status-method
setMethod("status<-", "agent", function(object, value) {
    stopifnot(value %in% c("move", "plan", "reroute", "reorient", "completing goal", "exit", "wait"))
    object@status <- value
    return(object)
})



#' @rdname utility_variables-method
setMethod("utility_variables", "agent", function(object) {
    return(object@utility_variables)
})

#' @rdname utility_variables-method
setMethod("utility_variables<-", "agent", function(object, value) {
    object@utility_variables <- value
    return(object)
})



#' @rdname waiting_counter-method
setMethod("waiting_counter", "agent", function(object) {
    return(setNames(object@waiting_counter, object@id))
})

#' @rdname waiting_counter-method
setMethod("waiting_counter<-", "agent", function(object, value) {
    object@waiting_counter <- value
    return(object)
})