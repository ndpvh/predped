#' Plot an Object
#'
#' @param object An object of kind \code[predped]{object-class}
#' @param ... Additional arguments passed on to the geom used to plot the object.
#'
#' @export
setGeneric("plot", function(object) {standardGeneric("plot")})

#'@rdname plot-method
#'
setMethod("plot", "circle", function(object) {
    ggplot2::geom_point(
        ggplot2::aes(x = object@center[[1]], y = object@center[[2]], size = object@radius),
        show.legend = FALSE
    )
})

# NIELS: I'll leave this in there for now, but should be deleted at some point
#
# Background is not the background itself, rather the entire environment that is generated
# perpendicular orientation
# slope orientation to extract radian which can be used to 
# dictate where to draw rectangles on the wall

#'@rdname plot-method
#'
setMethod("plot", "rectangle", function(object) {
  ggplot2::geom_tile(
    ggplot2::aes(x = object@center[[1]], y = object@center[[2]], width = object@size[[1]], height = object@size[[2]])
  )
})

#'@rdname plot-method
#'
setMethod("plot", "polygon", function(object) {
  ggplot2::geom_polygon(
    ggplot2::aes(x = object@points[, 1], y = object@points[, 2])
  )
})
