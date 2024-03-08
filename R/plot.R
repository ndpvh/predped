#' Plot an Object.
#'
#' @param object An object of kind \code[predped]{object-class}
#' @param ... Additional arguments passed on to the geom used to plot the object.
#'
#' @return Either a geom or a ggplot
#' @export
#' @name plot-method
setGeneric("plot", function(object) {standardGeneric("plot")})

#'@rdname plot-method
#'
setMethod("plot", "circle", function(object) {
    ggplot2::geom_point(
        ggplot2::aes(x = object@center[[1]], y = object@center[[2]], size = object@radius),
        show.legend = FALSE
    )
})

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

#'@rdname plot-method
#'
setMethod("plot", "background", function(object) {
  plt <- ggplot2::ggplot()
  for (i in seq_along(objects(object))) {
    plt <- plt + plot(objects(object)[[i]])
  }
  return(plt)
})
