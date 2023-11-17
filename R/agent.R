#' An S4 class to represent agents 
#'
#' @slot id A numerical index for the agent
#' @slot position A numerical vector of two elements denoting the position of
#' the agent in 2D space
#' @slot size A numeric indicating the size of the radius of the agent
#' @slot orientation A numeric denoting the orientation of the agent in degrees
#' @slot interacting A logical denoting whether the agent is interacting with 
#' something in its environment
#' @slot parameters A named list that contains the individual-specific parameters 
#' for the agent
#' @slot goals A list containing the id's of the goals that are assigned to the 
#' agent
#' 
#' @exportClass 
setClass("agent", list(id = "numeric", 
                       position = "numeric",
                       size = "numeric",
                       orientation = "numeric", 
                       interacting = "logical",
                       parameters = "list",
                       goals = "list"))
