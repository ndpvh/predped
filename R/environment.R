# TO DO: Define what should be the output of the `shape` functions and make this 
# explicit in the documentation. Also provide some of these functions, for example 
# `rectangle` and `circle`

#' An S4 class to represent the background 
#'
#' @slot shape A function that defines the shape of the background based on the 
#' singular `size` argument that is provided 
#' @slot size A numeric indicating the size of corresponding to the required 
#' shape in the format as required (see `rectangle` or `circle` classes)
#' 
#' @exportClass 
setClass("background", list(shape = "closure", 
                            size = "numeric"))
                            
#' An S4 class to represent an object 
#'
#' @slot id A character giving the object a name.
#' @slot shape A function that defines the shape of the object based on the 
#' different arguments provided by this class
#' @slot position A numerical vector of 2 coordinates denoting the object's 
#' position
#' @slot size A numeric indicating the size of corresponding to the required 
#' shape in the format as required (see `rectangle` or `circle` classes)
#' @slot orientation A numeric denoting the orientation of the object in degrees
#' @slot busy A logical indicating whether an agent is interacting with it
#' @slot moveable A logical indicating whether the object can be moved from its 
#' position 
#' 
#' @exportClass 
setClass("object", list(id = "character",
                        shape = "closure", 
                        position = "numeric",
                        size = "numeric",
                        orientation = "numeric",
                        busy = "logical",
                        moveable = "logical"))

#' Materialize environment objects
#' 
#' This function will read the properties of the object that is passed on and 
#' materialize the object based on the `shape` function and the provided arguments. 
#'
#' @param object The object to be initialized
#'
#' @return TBD
#' 
#' @export
#' @docType methods
#' @rdname materialize-methods
setGeneric("materialize", 
           function(object){
               return(object@shape(object))
           })

#' @rdname materialize-methods
#' @aliases materialize, background
