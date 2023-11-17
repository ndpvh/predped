#' Move an object
#' 
#' Move an object around the space. This function changes the positions and/or 
#' orientation of the provided object. For the class `agent`, this involves a 
#' computation of the utility of a given movement. For other classes, this 
#' involves checking whether an agent is interacting with the object before it 
#' can move.
#'
#' @param object The object of which the position should be moved.
#' @param ... Additional arguments that can differ for agents or environmental 
#' objects.
#'
#' @return The provided object with a changed position and/or orientation
#' 
#' @export
#' @docType methods
#' @rdname move-methods
setGeneric("move", function(object, ...){})

#' @rdname move-methods
#' @aliases move, agent
setMethod("move", 
          "agent",
          function(object, ...){
              # Actual movement function: to create based on existing functions
          })

#' @rdname move-methods
#' @aliases move, rectangle
setMethod("move", 
          "rectangle",
          function(object, ...){
              # Actual movement function: to create myself
          })

#' @rdname move-methods
#' @aliases move, circle
setMethod("move", 
          "circle",
          function(object, ...){
              # Actual movement function: to create myself
          })    