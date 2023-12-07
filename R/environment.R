
setGeneric("rotate", 
           function(object, degrees, radians, center) standardGeneric("rotate"),
           signature = "object"
)

#' An S4 Class to Represent Coordinate Vectors
#' 
#' A named numeric vector that can only have two elements (x and y coordinates).
#'
#' @export
coordinate <- setClass("coordinate", contains = "numeric")

setMethod("initialize", "coordinate", function(.Object, ...) {
  .Object <- callNextMethod()
  
  if (length(.Object) != 2) stop("Coordinate vector must have length two (x and y)")
  
  names(.Object) <- c("x", "y")
  
  return(.Object)
})

#' Rotate Point Around Center
#' 
#' @param object A numeric vector of length two with the point coordinates.
#' @param degrees A single numeric element indicating the degrees of rotation.
#' @param center A numerical vector of length two with the x and y coordinates
#' around which to rotate the point. Defaults to the origin (0, 0).
#' 
#' @export 
setMethod("rotate", 
          signature(object = "numeric"), 
          function(object, radians, center = c(0, 0)) {
            object <- as(object, "coordinate")
            center <- as(center, "coordinate")
            # Change coordinates to move around the origin
            x_origin <- object - center
            
            # Rotate the point to a new position
            M <- matrix(c(cos(radians), -sin(radians), 
                          sin(radians), cos(radians)),
                        nrow = 2, 
                        ncol = 2)
            
            y_origin <- as.numeric(M %*% x_origin)
            
            # Change coordinates back to the original center
            return(as(y_origin + center, "coordinate"))
          })

setAs("numeric", "coordinate", function(from) new("coordinate", from))

#' An S4 Abstract Base Class to Represent Objects 
#'
#' @slot id A character giving the object a name.
#' @slot moveable A logical indicating whether the object can be moved from its 
#' position.
#' @slot interactable A logical indicating whether the object can be interacted
#' with.
#' @slot busy A logical indicating whether an agent is interacting with it.
#' @slot interacted_with A logical indicating whether the object was interacted
#' with.
#' 
#' @export
setClass("object", list(id = "character",
                        moveable = "logical",
                        interactable = "logical",
                        busy = "logical",
                        interacted_with = "logical"),
         contains = "VIRTUAL")

#' Move an Object
#'
#' Move the center of the object to the target coordinates.
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#' @param target A numeric of length two with the target coordinates.
#'
#' @return The object moved to the target coordinates.
#' @export
#' @name move-method
setGeneric("move", function(object, target) standardGeneric("move"))

#' Calculate the Area of an Object
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#'
#' @return The area of the object.
#' @export
#' @name area-method
setGeneric("area", function(object) standardGeneric("area"))

#' An S4 Class to Represent Rectangle Objects
#' 
#' Rectangles can be defined by either \code{center} and \code{size} or 
#' \code{lower} and \code{upper}.
#' 
#' @slot center A numeric vector of length two with the coordinates of the center.
#' @slot size A numeric vector of length two with the object width and height.
#' @slot lower A numeric vector of length two with the coordinates of the
#' lower left corner. 
#' @slot upper A numeric vector of length two with the coordinates of the
#' upper right corner. 
#' @slot orientation A single numeric element indicating the orientation of the 
#' rectangle in radians.
#' 
#' @export
#' @name rectangle
rectangle <- setClass("rectangle", list(center = "numeric", size = "numeric", 
                                        lower = "numeric", upper = "numeric",
                                        orientation = "numeric"), 
                      contains = c("object"))

setMethod("initialize", "rectangle", function(
    .Object, 
    orientation = 0, 
    moveable = FALSE, ...
) {
  .Object <- callNextMethod(.Object, ...)
  if (length(.Object@center) && length(.Object@size)) {
    center <- .Object@center
    lower <- center - .Object@size/2
    upper <- center + .Object@size/2
  } else if (length(.Object@lower) && length(.Object@lower)) {
    lower <- .Object@lower
    upper <- .Object@upper
    size <- upper - lower
    center <- upper - size/2
  } else {
    stop("Either 'center' and 'size' or 'lower' and 'upper' must be provided")
  }
  if (length(size) != 2) stop("Size vector must have length two (x and y)")
  if (any(size <= 0)) stop("Size vector must be positive")
  if (length(orientation) != 1) stop("Orientation must be a single element")
  .Object@center <- as(center, "coordinate")
  .Object@lower <- as(lower, "coordinate")
  .Object@upper <- as(upper, "coordinate")
  .Object@size <- size
  .Object@orientation <- orientation
  .Object@moveable <- moveable
  return(.Object)
})

#' @rdname move-method
#' @export
setMethod("move", signature(object = "rectangle", target = "numeric"), function(object, target) {
  target <- as(target, "coordinate")
  
  if (!object@moveable) {
    return(object)
  }
  
  d <- target - object@center
  object@center <- target
  object@lower <- object@lower + d
  object@upper <- object@upper + d
  
  return(object)
})

#' Rotate an Rectangle
#'
#' @param object An object of a type \code{\link[predped]{rectangle-class}}.
#' @param degrees A single numeric element indicating the target rotation in degrees.
#' @param radians A single numeric element indicating the target rotation in radians.
#'
#' @return The object moved to the target coordinates.
#' @export
setMethod("rotate", 
          signature(object = "rectangle"), 
          function(object, degrees, radians) {
  if (!object@moveable) {
    return(object)
  }
  
  # Transform degrees to radians
  if (missing(radians) && !missing(degrees)) {
    radians <- degrees * pi / 180
  } else if (!(!missing(radians) && missing(degrees))) {
    stop("Either 'degress' or 'radians' must be provided")
  }
  
  object@orientation <- radians
  
  return(object)
})

#'@rdname area-method
#'
setMethod("area", signature(object = "rectangle"), function(object) prod(object@upper - object@lower))

#' Get the Corners of an Object
#'
#' Get the object corners taking its orientation into account.
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#'
#' @return A two-column matrix containing the x- and y-coordinates of the corners.
#' @export
#' @name corners-method
setGeneric("corners", function(object) standardGeneric("corners"))

#'@rdname corners-method
#'
setMethod("corners", signature(object = "rectangle"), function(object) {
  mat <- list(
    object@lower,
    as(c(object@lower[1], object@upper[2]), "coordinate"),
    object@upper,
    as(c(object@upper[1], object@lower[2]), "coordinate")
  )
  names(mat) <- c("lowerleft", "upperleft", "upperright", "lowerright")
  
  if (object@orientation != 0) {
    mat <- lapply(mat, rotate, radians = object@orientation, center = object@center)
  }
  
  return(mat)
})

#' Check Whether a Point Lies Within an Object
#'
#' @param object An object of a type that extends \code{\link[predped]{object-class}}.
#' @param x A numeric vector of length two with x and y coordinates of the point.
#' @param outside A logical indicating whether to return \code{TRUE} if the point
#' is outside the object.
#'
#' @return Logical whether the point is inside the object 
#' (if \code{outside} is \code{FALSE}) or outside.
#' @export
#' @name inObject-method
setGeneric("inObject", function(object, x, outside = TRUE) standardGeneric("inObject"))

#'@rdname inObject-method
#'
setMethod("inObject", signature(object = "rectangle", x = "numeric"), function(object, x, outside = TRUE) {
  x <- as(x, "coordinate")
  ok <- all(x > object@lower & x < object@upper)
  if (outside) {
    return(!ok)
  }
  return(ok)
})

#' An S4 Class to Represent Circle Objects
#'
#' @slot center A numeric vector of length two indicating the center of the circle.
#' @slot radius A numeric indicating the radius of the circle.
#' 
#' @export
circle <- setClass("circle", list(center = "numeric", radius = "numeric"), contains = c("object"))

setMethod("initialize", "circle", function(.Object, moveable = FALSE, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@center <- as(.Object@center, "coordinate")
  .Object@moveable <- moveable
  if (length(.Object@radius) != 1) stop("Slot 'radius' should be a single numeric value")
  return(.Object)
})

#' @rdname move-method
#' @export
setMethod("move", signature(object = "circle", target = "numeric"), function(object, target) {
  target <- as(target, "coordinate")
  
  if (!object@moveable) {
    return(object)
  }
  
  object@center <- target
  
  return(object)
})

#'@rdname area-method
#'
setMethod("area", signature(object = "circle"), function(object) pi*object@radius^2)
     
#' An S4 Class to Represent the Background
#'
#' @slot shape An object of a type that extends \code{\link[predped]{object-class}} 
#' defining the shape of the background.
#' @slot objects A list of objects of a type that extends 
#' \code{\link[predped]{object-class}} defining the objects in the background.
#'
#' @export
background <- setClass("background", list(shape = "object", objects = "list"))

setMethod("initialize", "background", function(.Object, ...) {
  .Object <- callNextMethod()
  if (!all(sapply(.Object@objects, is, class2 = "object"))) {
    stop("All elements in slot 'objects' must be of type 'object'")
  }
  return(.Object)
})
