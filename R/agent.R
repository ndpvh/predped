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
#' \code{\link[predped]{initialize-agent}},
#' \code{\link[predped]{object-class}}
#' 
#' @rdname agent-class
#'
#' @export
agent <- setClass("agent", list(id = "character",
                                speed = "numeric",
                                orientation = "numeric",
                                group = "numeric",
                                status = "character",
                                waiting_counter = "numeric",
                                cell = "numeric",
                                current_goal = "goal",
                                goals = "list", 
                                parameters = "data.frame",
                                color = "character"), contains = c("circle"))

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
#' \code{\link[predped]{cell-method}},
#' \code{\link[predped]{current_goal-method}},
#' \code{\link[predped]{goals-method}},
#' \code{\link[predped]{group-method}},
#' \code{\link[predped]{id-method}},
#' \code{\link[predped]{orientation-method}},
#' \code{\link[predped]{parameters-method}}
#' \code{\link[predped]{position-method}},
#' \code{\link[predped]{size-method}},
#' \code{\link[predped]{speed-method}},
#' \code{\link[predped]{status-method}},
#' \code{\link[predped]{waiting_counter-method}}
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
                                          group = 0,
                                          status = "move",
                                          waiting_counter = 0,
                                          cell = 0,                                          
                                          color = "black") {

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
    .Object@group <- group
    .Object@status <- status
    .Object@waiting_counter <- waiting_counter
    .Object@cell <- cell    
    .Object@color <- color    

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
    print(params)
    cat("\nradius:", object@radius, "\n")
    cat("speed:", object@speed, "\n")
    cat("status:", object@status, "\n")
    cat("\nFor more detailed information, please extract the wanted information from the agent directly.\n")
})





################################################################################
# GETTERS AND SETTERS

#' Getter/Setter for the \code{cell}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   cell = 1)
#' 
#' # Access the cell slot for the agent
#' cell(my_agent)
#' 
#' # Change the cell slot for the agent
#' cell(my_agent) <- 2
#' cell(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}
#' 
#' @rdname cell-method
#' 
#' @export
setGeneric("cell", function(object) standardGeneric("cell"))

#' @rdname cell-method
#' 
#' @export
setGeneric("cell<-", function(object, value) standardGeneric("cell<-"))

setMethod("cell", "agent", function(object) {
    return(setNames(object@cell, object@id))
})

setMethod("cell<-", "agent", function(object, value) {
    object@cell <- value
    return(object)
})

#' Getter/Setter for the \code{color}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   color = "black")
#' 
#' # Access the color slot for the agent
#' color(my_agent)
#' 
#' # Change the color slot for the agent
#' color(my_agent) <- "blue"
#' color(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{goal-class}}
#' 
#' @rdname color-method
#' 
#' @export
setGeneric("color", function(object) standardGeneric("color"))

#' @rdname color-method
#' 
#' @export
setGeneric("color<-", function(object, value) standardGeneric("color<-"))

setMethod("color", "agent", function(object) {
    return(setNames(object@color, object@id))
})

setMethod("color<-", "agent", function(object, value) {
    object@color <- value
    return(object)
})

#' Getter/Setter for the \code{current_goal}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   current_goal = goal(position = c(1, 0)))
#' 
#' # Access the current_goal slot for the agent
#' current_goal(my_agent)
#' 
#' # Change the current_goal slot for the agent
#' current_goal(my_agent) <- goal(position = c(-1, 0))
#' current_goal(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{goal-class}}
#' 
#' @rdname current_goal-method
#' 
#' @export
setGeneric("current_goal", function(object) standardGeneric("current_goal"))

#' @rdname current_goal-method
#' 
#' @export
setGeneric("current_goal<-", function(object, value) standardGeneric("current_goal<-"))

setMethod("current_goal", "agent", function(object) {
    return(object@current_goal)
})

setMethod("current_goal<-", "agent", function(object, value) {
    object@current_goal <- value
    return(object)
})

#' Getter/Setter for the \code{goals}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   goals = list(goal(position = c(1, 0))))
#' 
#' # Access the goals slot for the agent
#' goals(my_agent)
#' 
#' # Change the goals slot for the agent
#' goals(my_agent) <- list(goal(position = c(-1, 0)))
#' goals(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{goal-class}}
#' 
#' @rdname goals-method
#' 
#' @export
setGeneric("goals", function(object) standardGeneric("goals"))

#' @rdname goals-method
#' 
#' @export
setGeneric("goals<-", function(object, value) standardGeneric("goals<-"))

setMethod("goals", "agent", function(object) {
    return(object@goals)
})

setMethod("goals<-", "agent", function(object, value) {
    object@goals <- value
    return(object)
})

#' Getter/Setter for the \code{group}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   group = 1)
#' 
#' # Access the speed slot for the agent
#' group(my_agent)
#' 
#' # Change the speed slot for the agent
#' group(my_agent) <- 2
#' group(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}
#' 
#' @rdname group-method
#' 
#' @export
setGeneric("group", function(object) standardGeneric("group"))

#' @rdname group-method
#' 
#' @export
setGeneric("group<-", function(object, value) standardGeneric("group<-"))

setMethod("group", "agent", function(object) {
    return(setNames(object@group, object@id))
})

setMethod("group<-", "agent", function(object, value) {
    object@group <- value
    return(object)
})

# Getter/Setter for id slot for agent
setMethod("id", "agent", function(object) {
    return(setNames(object@id, object@id))
})

setMethod("id<-", "agent", function(object, value) {
    object@id <- value
    return(object)
})

# Getter/Setter for orientation slot for agent
setMethod("orientation", "agent", function(object) {
    return(setNames(object@orientation, object@id))
})

setMethod("orientation<-", "agent", function(object, value) {
    object@orientation <- value
    return(object)
})

#' Getter/Setter for the \code{parameters}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   parameters = draw_parameters(1))
#' 
#' # Access the parameters slot for the agent
#' parameters(my_agent)
#' 
#' # Change the parameters slot for the agent
#' parameters(my_agent) <- draw_parameters(1)
#' parameters(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}
#' 
#' @rdname parameters-method
#' 
#' @export
setGeneric("parameters", function(object) standardGeneric("parameters"))

#' @rdname parameters-method
#' 
#' @export
setGeneric("parameters<-", function(object, value) standardGeneric("parameters<-"))

setMethod("parameters", "agent", function(object) {
    return(object@parameters)
})

setMethod("parameters<-", "agent", function(object, value) {
    object@parameters <- value
    return(object)
})

# Getter/Setter for position slot for agent
setMethod("position", "agent", function(object, return_matrix = FALSE) {
    if (return_matrix) {
        return(matrix(object@center, nrow = 1, ncol = 2, dimnames = list(object@id, names(object@center))))
    }
    return(object@center)
})

setMethod("position<-", "agent", function(object, value) {
    object@center <- as(value, "coordinate")
    return(object)
})

# Getter/Setter for size slot for agent
setMethod("size", "agent", function(object) {
    return(setNames(object@radius, object@id))
})

setMethod("size<-", "agent", function(object, value) {
    object@radius <- value
    return(object)
})

#' Getter/Setter for the \code{speed}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   speed = 1)
#' 
#' # Access the speed slot for the agent
#' speed(my_agent)
#' 
#' # Change the speed slot for the agent
#' speed(my_agent) <- 2
#' speed(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}
#' 
#' @rdname speed-method
#' 
#' @export
setGeneric("speed", function(object) standardGeneric("speed"))

#' @rdname speed-method
#' 
#' @export
setGeneric("speed<-", function(object, value) standardGeneric("speed<-"))

setMethod("speed", "agent", function(object) {
    return(setNames(object@speed, object@id))
})

setMethod("speed<-", "agent", function(object, value) {
    object@speed <- value
    return(object)
})

#' Getter/Setter for the \code{status}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   status = "move")
#' 
#' # Access the status slot for the agent
#' status(my_agent)
#' 
#' # Change the status slot for the agent
#' status(my_agent) <- "reroute"
#' status(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}
#' 
#' @rdname status-method
#' 
#' @export
setGeneric("status", function(object) standardGeneric("status"))

#' @rdname status-method
#' 
#' @export
setGeneric("status<-", function(object, value) standardGeneric("status<-"))

setMethod("status", "agent", function(object) {
    return(setNames(object@status, object@id))
})

setMethod("status<-", "agent", function(object, value) {
    stopifnot(value %in% c("move", "plan", "reroute", "reorient", "completing goal", "exit", "wait"))
    object@status <- value
    return(object)
})

#' Getter/Setter for the \code{waiting_counter}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   waiting_counter = 0)
#' 
#' # Access the waiting_counter slot for the agent
#' waiting_counter(my_agent)
#' 
#' # Change the waiting_counter slot for the agent
#' waiting_counter(my_agent) <- 5
#' waiting_counter(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{goal-class}}
#' 
#' @rdname waiting_counter-method
#' 
#' @export
setGeneric("waiting_counter", function(object) standardGeneric("waiting_counter"))

#' @rdname waiting_counter-method
#' 
#' @export
setGeneric("waiting_counter<-", function(object, value) standardGeneric("waiting_counter<-"))

setMethod("waiting_counter", "agent", function(object) {
    return(setNames(object@waiting_counter, object@id))
})

setMethod("waiting_counter<-", "agent", function(object, value) {
    object@waiting_counter <- value
    return(object)
})