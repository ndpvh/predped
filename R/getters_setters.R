################################################################################
# CONTAINS GENERIC OF GETTERS AND SETTERS

#' Getter/Setter for the \code{agents}-slot
#' 
#' Works for the \code{\link[predped]{state-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{state-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{agents} slot.
#' 
#' @examples 
#' # Initialize state
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5))) 
#' 
#' my_state <- state(iteration = 0, 
#'                   setting = my_background,
#'                   agents = list(agent(center = c(0, 0), radius = 0.25)))
#' 
#' # Access agents slot
#' agents(my_state)
#' 
#' # Change the agents slot
#' agents(my_state) <- list(agent(center = c(1, 1), radius = 0.25))
#' agents(my_state)
#' 
#' @seealso 
#' \code{\link[predped]{state-class}}
#' 
#' 
#' @rdname agents
#' 
#' @export
setGeneric("agents", function(object) standardGeneric("agents"))

#' @rdname agents
#' 
#' @export
setGeneric("agents<-", function(object, value) standardGeneric("agents<-"))



#' Getter/Setter for the \code{archetypes}-slot
#' 
#' Works for \code{\link[predped]{predped-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{predped-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{archetypes} slot. Note that you will get an error if the newly provided
#' value is not contained within the \code{parameters} slot of the \code{object}.
#' 
#' @examples 
#' # Initialize a predped model
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list())
#' 
#' my_model <- predped(setting = my_background, 
#'                     archetypes = c("BaselineEuropean", 
#'                                    "DrunkAussie"))
#' 
#' # Access the archetypes slot 
#' archetypes(my_model)
#' 
#' # Change the archetypes slot
#' archetypes(my_model) <- c("BaselineEuropean")
#' archetypes(my_model)
#' 
#' # Note that this also changes the parameters contained in the model
#' head(my_model@parameters)
#' 
#' # Furthermore note that you may get an error when parameters are not 
#' # available
#' archetypes(my_model) <- c("DrunkAussie")
#' 
#' @seealso 
#' \code{\link[predped]{predped-class}}
#' 
#' 
#' @rdname archetypes
#'
#' @export
setGeneric("archetypes", function(object) standardGeneric("archetypes"))

#' @rdname archetypes
#'
#' @export
setGeneric("archetypes<-", function(object, value) standardGeneric("archetypes<-"))



#' Getter/Setter for the \code{busy}-slot
#' 
#' Works for \code{\link[predped]{goal-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{goal-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{busy} slot.
#' 
#' @examples 
#' # Initialize a goal with a given business status
#' my_goal <- goal(position = c(0, 0), 
#'                 busy = TRUE)
#' 
#' # Access the business 
#' busy(my_goal)
#' 
#' # Change the business
#' busy(my_goal) <- FALSE
#' busy(my_goal)
#' 
#' 
#' @rdname busy
#' 
#' @export
setGeneric("busy", function(object) standardGeneric("busy"))

#' @rdname busy
#' 
#' @export
setGeneric("busy<-", function(object, value) standardGeneric("busy<-"))



#' Getter/Setter for the \code{cell}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{cell} slot.
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
#' 
#' @rdname cell
#' 
#' @export
setGeneric("cell", function(object) standardGeneric("cell"))

#' @rdname cell
#' 
#' @export
setGeneric("cell<-", function(object, value) standardGeneric("cell<-"))



#' Getter/Setter for the \code{cell_centers}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{cell_centers} slot.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   cell_centeres = matrix(1, nrow = 33, ncol = 2))
#' 
#' # Access the cell centers for the agent
#' cell_centers(my_agent)
#' 
#' # Change the cell centers for the agent
#' cell_centers(my_agent) <- compute_centers(my_agent)
#' cell_centers(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}
#' 
#' 
#' @rdname cell_centers
#' 
#' @export
setGeneric("cell_centers", function(object) standardGeneric("cell_centers"))

#' @rdname cell_centers
#' 
#' @export
setGeneric("cell_centers<-", function(object, value) standardGeneric("cell_centers<-"))



#' Getter/Setter for the \code{center}-slot
#' 
#' Is a more specific version of \code{\link[predped]{position}} that 
#' works for all extensions of the \code{\link[predped]{object-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{object-class}} or the 
#' \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{center} slot.
#' 
#' @examples
#' # Initialize a circle
#' my_circle <- circle(center = c(0, 0), 
#'                     radius = 1)
#' 
#' # Access the center slot for the agent
#' center(my_agent)
#' 
#' # Change the center slot for the agent
#' center(my_agent) <- c(1, 1)
#' center(my_agent)
#' 
#' # Note that for some object, changing the center also changes other slots
#' my_rectangle <- rectangle(center = c(0, 0), 
#'                           size = c(2, 2))
#' 
#' points(my_rectangle)
#' 
#' center(my_rectangle) <- c(1, 1)
#' points(my_rectangle)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}, 
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}}
#' 
#' 
#' @rdname center
#' 
#' @export
setGeneric("center", function(object) standardGeneric("center"))

#' @rdname center
#' 
#' @export 
setGeneric("center<-", function(object, value) standardGeneric("center<-"))



#' Getter/Setter for the \code{color}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{color} slot.
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
#' 
#' @rdname color
#' 
#' @export
setGeneric("color", function(object) standardGeneric("color"))

#' @rdname color
#' 
#' @export
setGeneric("color<-", function(object, value) standardGeneric("color<-"))



#' Getter/Setter for the \code{counter}-slot
#' 
#' Works for \code{\link[predped]{goal-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{goal-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{counter} slot.
#' 
#' @examples 
#' # Initialize a goal with a given counter
#' my_goal <- goal(position = c(0, 0), 
#'                 counter = 5)
#' 
#' # Access the counter 
#' counter(my_goal)
#' 
#' # Change the counter
#' counter(my_goal) <- 10
#' counter(my_goal)
#' 
#' 
#' @rdname counter
#' 
#' @export
setGeneric("counter", function(object) standardGeneric("counter"))

#' @rdname counter
#' 
#' @export
setGeneric("counter<-", function(object, value) standardGeneric("counter<-"))



#' Getter/Setter for the \code{current_goal}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{current_goal} slot.
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
#' 
#' @rdname current_goal
#' 
#' @export
setGeneric("current_goal", function(object) standardGeneric("current_goal"))

#' @rdname current_goal
#' 
#' @export
setGeneric("current_goal<-", function(object, value) standardGeneric("current_goal<-"))



#' Getter/Setter for the \code{done}-slot
#' 
#' Works for \code{\link[predped]{goal-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{goal-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{done} slot.
#' 
#' @examples 
#' # Initialize a goal with a given done argument
#' my_goal <- goal(position = c(0, 0), 
#'                 done = FALSE)
#' 
#' # Access the done status
#' done(my_goal)
#' 
#' # Change the done status
#' done(my_goal) <- TRUE
#' done(my_goal)
#' 
#' 
#' @rdname done
#' 
#' @export
setGeneric("done", function(object, return_matrix = FALSE) standardGeneric("done"))

#' @rdname done
#' 
#' @export
setGeneric("done<-", function(object, value) standardGeneric("done<-"))



#' Getter/Setter for the \code{entrance}-slot
#' 
#' Works for \code{\link[predped]{background-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{background-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{entrance} slot.
#' 
#' @examples
#' # Initialize background
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5)),
#'                             limited_access = list(segment(from = c(-1, -1), 
#'                                                           to = c(1, 1))), 
#'                             entrance = c(-1, 0), 
#'                             exit = c(1, 0)) 
#' 
#' # Access the entrance slot for the background
#' entrance(my_background)
#' 
#' # Change the entrance slot for the background
#' entrance(my_background) <- c(1, 0)
#' entrance(my_background)
#' 
#' @seealso 
#' \code{\link[predped]{background-class}}
#' 
#' 
#' @rdname entrance
#' 
#' @export
setGeneric("entrance", function(object) standardGeneric("entrance"))

#' @rdname entrance
#' 
#' @export
setGeneric("entrance<-", function(object, value) standardGeneric("entrance<-"))



#' Getter/Setter for the \code{exit}-slot
#' 
#' Works for \code{\link[predped]{background-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{background-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{exit} slot.
#' 
#' @examples
#' # Initialize background
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5)),
#'                             limited_access = list(segment(from = c(-1, -1), 
#'                                                           to = c(1, 1))), 
#'                             entrance = c(-1, 0), 
#'                             exit = c(1, 0)) 
#' 
#' # Access the exit slot for the background
#' exit(my_background)
#' 
#' # Change the exit slot for the background
#' exit(my_background) <- c(-1, 0)
#' exit(my_background)
#' 
#' @seealso 
#' \code{\link[predped]{background-class}}
#' 
#' 
#' @rdname exit
#' 
#' @export
setGeneric("exit", function(object) standardGeneric("exit"))

#' @rdname exit
#' 
#' @export
setGeneric("exit<-", function(object, value) standardGeneric("exit<-"))



#' Getter/Setter for the \code{forbidden}-slot
#' 
#' Works for the all instances of the \code{\link[predped]{object-class}}, 
#' except for the \code{\link[predped]{segment-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{object-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{forbidden} slot.
#' 
#' @examples 
#' # Initialize an object
#' my_rectangle <- rectangle(center = c(0, 0), 
#'                           size = c(1, 1), 
#'                           forbidden = 1)
#' 
#' # Access iteration slot
#' forbidden(my_rectangle)
#' 
#' # Change the iteration slot
#' forbidden(my_rectangle) <- 2:3
#' forbidden(my_rectangle)
#' 
#' @seealso 
#' \code{\link[predped]{object-class}}
#' 
#' 
#' @rdname forbidden
#' 
#' @export
setGeneric("forbidden", function(object) standardGeneric("forbidden"))

#' @rdname forbidden
#' 
#' @export
setGeneric("forbidden<-", function(object, value) standardGeneric("forbidden<-"))



#' Getter/Setter for the \code{from}-slot
#' 
#' Works for the \code{\link[predped]{segment-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{segment-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{from} slot.
#' 
#' @examples 
#' # Create a segment
#' my_segment <- segment(from = c(0, 0), to = c(1, 1))
#' 
#' # Access the from slot
#' from(my_segment)
#' 
#' # Change the from slot
#' from(my_segment) <- c(4, 4)
#' from(my_segment)
#' 
#' # Note that changing this slot also changes the orientation and size of the 
#' # line
#' orientation(my_segment)
#' size(my_segment)
#' 
#' @seealso 
#' \code{\link[predped]{object-class}}
#' \code{\link[predped]{segment-class}}
#' 
#' 
#' @rdname from
#' 
#' @export 
setGeneric("from", function(object, ...) standardGeneric("from"))

#' @rdname from
#' 
#' @export 
setGeneric("from<-", function(object, value) standardGeneric("from<-"))



#' Getter/Setter for the \code{goals}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{goals} slot.
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
#' 
#' @rdname goals
#' 
#' @export
setGeneric("goals", function(object) standardGeneric("goals"))

#' @rdname goals
#' 
#' @export
setGeneric("goals<-", function(object, value) standardGeneric("goals<-"))



#' Getter/Setter for the \code{group}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{group} slot.
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
#' 
#' @rdname group
#' 
#' @export
setGeneric("group", function(object) standardGeneric("group"))

#' @rdname group
#' 
#' @export
setGeneric("group<-", function(object, value) standardGeneric("group<-"))



#' Getter/Setter for the \code{id}-slot
#' 
#' Works for all objects that have an \code{id}-slot, such as all extensions of 
#' \code{\link[predped]{object-class}}, the \code{\link[predped]{agent-class}}, 
#' and the \code{\link[predped]{goal-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{agent-class}}, or \code{\link[predped]{goal-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{id} slot.
#' 
#' @details
#' Note that while the \code{\link[predped]{agent-class}}, 
#' \code{\link[predped]{circle-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{rectangle-class}}, and
#' \code{\link[predped]{segment-class}} are not explicitly mentioned, this 
#' getter/setter works for these classes as well.#' 
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   id = "my agent")
#' 
#' # Access the id slot for the agent
#' id(my_agent)
#' 
#' # Change the id slot for the agent
#' id(my_agent) <- "renamed agent"
#' id(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}, 
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{goal-class}}, 
#' \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}}
#' 
#' 
#' @rdname id
#' 
#' @export
setGeneric("id", function(object) standardGeneric("id"))

#' @rdname id
#' 
#' @export
setGeneric("id<-", function(object, value) standardGeneric("id<-"))



#' Getter/Setter for the \code{iteration}-slot
#' 
#' Works for the \code{\link[predped]{state-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{state-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{iteration} slot.
#' 
#' @examples 
#' # Initialize state
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5))) 
#' 
#' my_state <- state(iteration = 0, 
#'                   setting = my_background)
#' 
#' # Access iteration slot
#' iteration(my_state)
#' 
#' # Change the iteration slot
#' iteration(my_state) <- 1
#' iteration(my_state)
#' 
#' @seealso 
#' \code{\link[predped]{state-class}}
#' 
#' 
#' @rdname iteration
#' 
#' @export
setGeneric("iteration", function(object) standardGeneric("iteration"))

#' @rdname iteration
#' 
#' @export
setGeneric("iteration<-", function(object, value) standardGeneric("iteration<-"))



#' Getter/Setter for the \code{iteration_variables}-slot
#' 
#' Works for the \code{\link[predped]{state-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{state-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{iteration_variables} slot.
#' 
#' @examples 
#' # Initialize state
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5))) 
#' 
#' my_state <- state(iteration = 0, 
#'                   setting = my_background)
#' 
#' # Access iteration_variables slot
#' iteration_variables(my_state)
#' 
#' # Change the iteration_variables slot
#' iteration_variables(my_state) <- data.frame(x = rep(1,3))
#' iteration_variables(my_state)
#' 
#' @seealso 
#' \code{\link[predped]{state-class}}
#' 
#' 
#' @rdname iteration_variables
#' 
#' @export
setGeneric("iteration_variables", function(object) standardGeneric("iteration_variables"))

#' @rdname iteration_variables
#' 
#' @export
setGeneric("iteration_variables<-", function(object, value) standardGeneric("iteration_variables<-"))



#' Getter/Setter for the \code{limited_access}-slot
#' 
#' Works for \code{\link[predped]{background-class}}.
#' 
#' #' @param object An instance of the \code{\link[predped]{background-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{limited_access} slot.
#' 
#' @examples
#' # Initialize background
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5)),
#'                             limited_access = list(segment(from = c(-1, -1), 
#'                                                           to = c(1, 1))), 
#'                             entrance = c(-1, 0), 
#'                             exit = c(1, 0)) 
#' 
#' # Access the limited_access slot for the background
#' limited_access(my_background)
#' 
#' # Change the limited_access slot for the background
#' limited_access(my_background) <- list(segment(from = c(-1, 1), to = c(1, -1)))
#' limited_access(my_background)
#' 
#' @seealso 
#' \code{\link[predped]{background-class}}
#' 
#' 
#' @rdname limited_access
#' 
#' @export
setGeneric("limited_access", function(object) standardGeneric("limited_access"))

#' @rdname limited_access
#' 
#' @export
setGeneric("limited_access<-", function(object, value) standardGeneric("limited_access<-"))



#' Getter/Setter for the \code{objects}-slot
#' 
#' Works for \code{\link[predped]{background-class}}.
#' 
#' @param name,object An instance of the \code{\link[predped]{background-class}}.
#' Note that \code{name} and \code{object} are synonymous for this function: The
#' former is only used to ensure compatibility with the \code{objects} function 
#' in base R.
#' @param value Value with which to replace the original value of the 
#' \code{objects} slot.
#' 
#' @examples
#' # Initialize background
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5)),
#'                             limited_access = list(segment(from = c(-1, -1), 
#'                                                           to = c(1, 1))), 
#'                             entrance = c(-1, 0), 
#'                             exit = c(1, 0)) 
#' 
#' # Access the objects slot for the background
#' objects(my_background)
#' 
#' # Change the objects slot for the background
#' #
#' # Note that the exit is blocked by the new object, which will lead to errors 
#' # if run in a simulation
#' objects(my_background) <- list(circle(center = c(1, 0), radius = 0.5))
#' objects(my_background)
#' 
#' 
#' @rdname objects
#' 
#' @export
setGeneric("objects<-", function(object, value) standardGeneric("objects<-"))



#' Getter/Setter for the \code{orientation}-slot
#' 
#' Works for all objects that are extensions of the 
#' \code{\link[predped]{object-class}}, except for the 
#' \code{\link[predped]{circle-class}} and the 
#' \code{\link[predped]{polygon-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{rectangle-class}}, 
#' \code{\link[predped]{segment-class}}, or \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{orientation} slot.
#' 
#' @examples
#' # Initialize a rectangle
#' my_rectangle <- rectangle(center = c(0, 0), 
#'                           size = c(2, 2),
#'                           orientation = 0)
#' 
#' # Access the orientation slot
#' orientation(my_rectangle)
#' 
#' # Change the orientation slot
#' orientation(my_rectangle) <- pi / 4
#' orientation(my_rectangle)
#' 
#' # Note that for some object, changing the orientation also changes other slots
#' points(my_rectangle)
#' 
#' @seealso 
#' \code{\link[predped]{object-class}},
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}}
#' 
#' 
#' @rdname orientation
#' 
#' @export
setGeneric("orientation", function(object) standardGeneric("orientation"))

#' @rdname orientation
#' 
#' @export 
setGeneric("orientation<-", function(object, value) standardGeneric("orientation<-"))



#' Getter/Setter for the \code{parameters}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}} or 
#' \code{\link[predped]{predped-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{parameters} slot.
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
#' 
#' @rdname parameters
#' 
#' @export
setGeneric("parameters", function(object) standardGeneric("parameters"))

#' @rdname parameters
#' 
#' @export
setGeneric("parameters<-", function(object, value) standardGeneric("parameters<-"))



#' Getter/Setter for the \code{path}-slot
#' 
#' Works for \code{\link[predped]{goal-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{goal-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{path} slot.
#' 
#' @examples 
#' # Initialize a goal with a given path
#' my_goal <- goal(position = c(0, 0), 
#'                 path = matrix(1:4, nrow = 2, ncol = 2))
#' 
#' # Access the path 
#' path(my_goal)
#' 
#' # Change the path
#' path(my_goal) <- matrix(5:8, nrow = 2, ncol = 2)
#' path(my_goal)
#' 
#' 
#' @rdname path
#' 
#' @export
setGeneric("path", function(object) standardGeneric("path"))

#' @rdname path
#' 
#' @export
setGeneric("path<-", function(object, value) standardGeneric("path<-"))



#' Getter/Setter for the \code{points}-slot
#' 
#' Works for all extensions of the \code{\link[predped]{object-class}}. Note
#' that you can only change the \code{points} slot for the 
#' \code{\link[predped]{polygon-class}} and the 
#' \code{\link[predped]{segment-class}}.
#' 
#' @param x,object An instance of the \code{\link[predped]{object-class}}. Note
#' that for this function \code{x} and \code{object} are synonymous: The \code{x}
#' argument is included to ensure compatibility with the \code{points} function 
#' from base R.
#' @param value Value with which to replace the original value of the 
#' \code{points} slot.
#' 
#' @details 
#' Note that for the \code{\link[predped]{circle-class}}, you can only access
#' the \code{points} slot and not change it. This is because circles don't have 
#' a finite number of points that make up their shape, and we therefore sample
#' points from the circumference of the circle when calling the \code{points}
#' method. 
#' 
#' @examples
#' # Initialize a rectangle
#' my_polygon <- polygon(cbind(c(1, 1, -1, -1), 
#'                             c(1, -1, -1, 1)))
#' 
#' # Access the points slot
#' points(my_polygon)
#' 
#' # Change the points slot
#' points(my_polygon) <- cbind(c(2, 2, -2, -2), 
#'                             c(2, -2, -2, 2))
#' points(my_polygon)
#' 
#' # For circles, we can only access the points slot and provide an argument 
#' # denoting how many points we want to sample from the circle's 
#' # circumference
#' my_circle <- circle(center = c(0, 0), 
#'                     radius = 1)
#' 
#' points(my_circle, length.out = 10)#' 
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}, 
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}}
#' 
#' 
#' @rdname points
#' 
#' @export
setGeneric("points<-", function(object, value) standardGeneric("points<-"))



#' Getter/Setter for the \code{position}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}, \code{\link[predped]{circle-class}},
#' \code{\link[predped]{goal-class}}, \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{polygon-class}}, \code{\link[predped]{rectangle-class}},
#' and \code{\link[predped]{segment-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}, 
#' \code{\link[predped]{object-class}}, or \code{\link[predped]{goal-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{position} or \code{center} slot.
#' 
#' @examples
#' # Initialize all objects for which this getter works
#' my_agent <- agent(center = c(1, 1), radius = 0.25)
#' my_circle <- circle(center = c(1, 0), radius = 0.25)
#' my_goal <- goal(position = c(0, 1))
#' my_polygon <- polygon(cbind(c(1, 1, -1, -1), c(1, -1, -1, 1)))
#' my_rectangle <- rectangle(center = c(1, 2), size = c(1, 1))
#' my_segment <- segment(from = c(0, 0), to = c(2, 2))
#' 
#' # Access the position slot for the different objects
#' position(my_agent)
#' position(my_circle)
#' position(my_goal)
#' position(my_polygon)
#' position(my_rectangle)
#' position(my_segment)
#' 
#' # Change the goals slot for the agent
#' position(my_agent) <- c(0, 0)
#' position(my_agent)
#' 
#' position(my_circle) <- c(0, 0)
#' position(my_circle)
#' 
#' position(my_goal) <- c(0, 0)
#' position(my_goal)
#' 
#' position(my_polygon) <- c(0, 0)
#' position(my_polygon)
#' 
#' position(my_rectangle) <- c(0, 0)
#' position(my_rectangle)
#' 
#' position(my_segment) <- c(0, 0)
#' position(my_segment)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}}, 
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{goal-class}}, 
#' \code{\link[predped]{object-class}}, 
#' \code{\link[predped]{polygon-class}}, 
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}}
#' 
#' 
#' @rdname position
#' 
#' @export
setGeneric("position", function(object, return_matrix = FALSE) standardGeneric("position"))

#' @rdname position
#' 
#' @export
setGeneric("position<-", function(object, value) standardGeneric("position<-"))



#' Getter/Setter for the \code{potential_agents}-slot
#' 
#' Works for the \code{\link[predped]{state-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{state-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{potential_agents} slot.
#' 
#' @examples 
#' # Initialize state
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5))) 
#' 
#' my_state <- state(iteration = 0, 
#'                   setting = my_background,
#'                   potential_agents = list(agent(center = c(0, 0), radius = 0.25)))
#' 
#' # Access agents slot
#' potential_agents(my_state)
#' 
#' # Change the potential_agents slot
#' potential_agents(my_state) <- list(agent(center = c(1, 1), radius = 0.25))
#' potential_agents(my_state)
#' 
#' @seealso 
#' \code{\link[predped]{state-class}}
#' 
#' 
#' @rdname potential_agents
#' 
#' @export
setGeneric("potential_agents", function(object) standardGeneric("potential_agents"))

#' @rdname potential_agents
#' 
#' @export
setGeneric("potential_agents<-", function(object, value) standardGeneric("potential_agents<-"))



#' Getter/Setter for the \code{radius}-slot
#' 
#' Works for the \code{\link[predped]{circle-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{circle-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{radius} slot.
#' 
#' @examples 
#' # Initialize a circle
#' my_circle <- circle(center = c(0, 0), 
#'                     radius = 1)
#' 
#' # Access the radius slot in circle
#' radius(my_circle)
#' 
#' # Change the radius slot in circle
#' radius(my_circle) <- 2
#' radius(my_circle)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{circle-class}}
#' 
#' 
#' @rdname radius
#' 
#' @export 
setGeneric("radius", function(object) standardGeneric("radius"))

#' @rdname radius
#' 
#' @export
setGeneric("radius<-", function(object, value) standardGeneric("radius<-"))



#' Getter/Setter for the \code{setting}-slot
#' 
#' Works for the \code{\link[predped]{state-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{state-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{setting} slot.
#' 
#' @examples 
#' # Initialize state
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5))) 
#' 
#' my_state <- state(iteration = 0, 
#'                   setting = my_background)
#' 
#' # Access setting slot
#' setting(my_state)
#' 
#' # Change the setting slot
#' other_background <- background(shape = rectangle(center = c(0, 0), 
#'                                                  size = c(2, 2)), 
#'                                objects = list(circle(center = c(0, 0), 
#'                                                      radius = 1))) 
#' 
#' setting(my_state) <- other_background
#' setting(my_state)
#' 
#' @seealso 
#' \code{\link[predped]{state-class}}
#' 
#' 
#' @rdname setting
#' 
#' @export
setGeneric("setting", function(object) standardGeneric("setting"))

#' @rdname setting
#' 
#' @export
setGeneric("setting<-", function(object, value) standardGeneric("setting<-"))



#' Getter/Setter for the \code{shape}-slot
#' 
#' Works for \code{\link[predped]{background-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{background-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{shape} slot.
#' 
#' @examples
#' # Initialize background
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5)),
#'                             limited_access = list(segment(from = c(-1, -1), 
#'                                                           to = c(1, 1))), 
#'                             entrance = c(-1, 0), 
#'                             exit = c(1, 0)) 
#' 
#' # Access the shape slot for the background
#' shape(my_background)
#' 
#' # Change the shape slot for the background
#' shape(my_background) <- circle(center = c(1, 0), radius = 1)
#' shape(my_background)
#' 
#' 
#' @rdname shape
#' 
#' @export
setGeneric("shape", function(object) standardGeneric("shape"))

#' @rdname shape
#' 
#' @export
setGeneric("shape<-", function(object, value) standardGeneric("shape<-"))



#' Getter/Setter for the \code{size}-slot
#' 
#' Works for the \code{\link[predped]{agent}},
#' \code{\link[predped]{circle}},
#' \code{\link[predped]{object}},
#' \code{\link[predped]{rectangle}}, and
#' \code{\link[predped]{segment}}.
#' 
#' @param object An instance of the \code{\link[predped]{object-class}} or 
#' \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{size} slot.
#' 
#' @details 
#' Note that for \code{\link[predped]{circle}}, this getter outputs the 
#' radius (and similarly, the setter changes the radius).
#' 
#' @examples 
#' # Initialize a circle
#' my_circle <- circle(center = c(0, 0), 
#'                     radius = 1)
#' 
#' # Access the radius slot in circle
#' size(my_circle)
#' 
#' # Change the radius slot in circle
#' size(my_circle) <- 2
#' size(my_circle)
#' 
#' # Also works for objects in which other slots change with a change in size
#' my_rectangle <- rectangle(center = c(0, 0), 
#'                           size = c(2, 2))
#' 
#' # Access points before and after changing its size
#' points(my_rectangle)
#' 
#' size(my_rectangle) <- c(4, 4)
#' points(my_rectangle) 
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{circle-class}},
#' \code{\link[predped]{object-class}},
#' \code{\link[predped]{rectangle-class}},
#' \code{\link[predped]{segment-class}}
#' 
#' 
#' @rdname size
#' 
#' @export 
setGeneric("size", function(object) standardGeneric("size"))

#' @rdname size
#' 
#' @export 
setGeneric("size<-", function(object, value) standardGeneric("size<-"))



#' Getter/Setter for the \code{speed}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{speed} slot.
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
#' 
#' @rdname speed
#' 
#' @export
setGeneric("speed", function(object) standardGeneric("speed"))

#' @rdname speed
#' 
#' @export
setGeneric("speed<-", function(object, value) standardGeneric("speed<-"))



#' Getter/Setter for the \code{status}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{status} slot.
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
#' 
#' @rdname status
#' 
#' @export
setGeneric("status", function(object) standardGeneric("status"))

#' @rdname status
#' 
#' @export
setGeneric("status<-", function(object, value) standardGeneric("status<-"))



#' Getter/Setter for the \code{to}-slot
#' 
#' Works for the \code{\link[predped]{segment-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{segment-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{to} slot.
#' 
#' @examples 
#' # Create a segment
#' my_segment <- segment(from = c(0, 0), to = c(1, 1))
#' 
#' # Access the to slot
#' to(my_segment)
#' 
#' # Change the to slot
#' to(my_segment) <- c(4, 4)
#' to(my_segment)
#' 
#' # Note that changing this slot also changes the orientation and size of the 
#' # line
#' orientation(my_segment)
#' size(my_segment)
#' 
#' @seealso 
#' \code{\link[predped]{object-class}}
#' \code{\link[predped]{segment-class}}
#' 
#' 
#' @rdname to
#' 
#' @export 
setGeneric("to", function(object, ...) standardGeneric("to"))

#' @rdname to
#' 
#' @export 
setGeneric("to<-", function(object, value) standardGeneric("to<-"))



#' Getter/Setter for the \code{utility_variables}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{utility_variables} slot.
#' 
#' @examples
#' # Initialize agent
#' my_agent <- agent(center = c(0, 0), 
#'                   radius = 0.25, 
#'                   utility_variables = data.frame())
#' 
#' # Access the utility_variables slot for the agent
#' utility_variables(my_agent)
#' 
#' # Change the utility_variables slot for the agent
#' utility_variables(my_agent) <- data.frame(value = 1)
#' utility_variables(my_agent)
#' 
#' @seealso 
#' \code{\link[predped]{agent-class}},
#' \code{\link[predped]{goal-class}}
#' 
#' 
#' @rdname utility_variables
#' 
#' @export
setGeneric("utility_variables", function(object) standardGeneric("utility_variables"))

#' @rdname utility_variables
#' 
#' @export
setGeneric("utility_variables<-", function(object, value) standardGeneric("utility_variables<-"))



#' Getter/Setter for the \code{variables}-slot
#' 
#' Works for the \code{\link[predped]{state-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{state-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{variables} slot.
#' 
#' @examples 
#' # Initialize state
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list(circle(center = c(0, 0), 
#'                                                   radius = 0.5))) 
#' 
#' my_state <- state(iteration = 0, 
#'                   setting = my_background)
#' 
#' # Access variables slot
#' variables(my_state)
#' 
#' # Change the variables slot
#' variables(my_state) <- list("my_variable" = 10)
#' variables(my_state)
#' 
#' @seealso 
#' \code{\link[predped]{state-class}}
#' 
#' 
#' @rdname variables
#' 
#' @export
setGeneric("variables", function(object) standardGeneric("variables"))

#' @rdname variables
#' 
#' @export
setGeneric("variables<-", function(object, value) standardGeneric("variables<-"))



#' Getter/Setter for the \code{waiting_counter}-slot
#' 
#' Works for \code{\link[predped]{agent-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{agent-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{waiting_counter} slot.
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
#' 
#' @rdname waiting_counter
#' 
#' @export
setGeneric("waiting_counter", function(object) standardGeneric("waiting_counter"))

#' @rdname waiting_counter
#' 
#' @export
setGeneric("waiting_counter<-", function(object, value) standardGeneric("waiting_counter<-"))



#' Getter/Setter for the \code{weights}-slot
#' 
#' Works for \code{\link[predped]{predped-class}}.
#' 
#' @param object An instance of the \code{\link[predped]{predped-class}}.
#' @param value Value with which to replace the original value of the 
#' \code{weights} slot.
#' 
#' @examples
#' # Initialize a predped model
#' my_background <- background(shape = rectangle(center = c(0, 0), 
#'                                               size = c(2, 2)), 
#'                             objects = list())
#' 
#' my_model <- predped(setting = my_background, 
#'                     archetypes = c("BaselineEuropean", 
#'                                    "DrunkAussie"))
#' 
#' # Access the archetypes slot 
#' weights(my_model)
#' 
#' # Change the archetypes slot
#' weights(my_model) <- c(0.9, 0.1)
#' weights(my_model)
#' 
#' @seealso 
#' \code{\link[predped]{predped-class}}
#' 
#' 
#' @rdname weights
#'
#' @export
setGeneric("weights", function(object) standardGeneric("weights"))

#' @rdname weights
#'
#' @export
setGeneric("weights<-", function(object, value) standardGeneric("weights<-"))