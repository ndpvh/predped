#' Plot an Object.
#'
#' @param object An object of kind \code[predped]{object-class}
#' @param ... Additional arguments passed on to the geom used to plot the object.
#'
#' @return Either a geom or a ggplot
#' @export
#' @name plot-method
setGeneric("plot", function(object, ...) standardGeneric("plot"))

#'@rdname plot-method
#'
setMethod("plot", "circle", function(object, ...) {
  # Create circle point from center x,y and radius r
  t <- seq(0, 2 * pi, length.out = 100)
  cp <- as.matrix(data.frame(
    x = object@center[[1]] + object@radius * cos(t),
    y = object@center[[2]] + object@radius * sin(t)
  ))
  
  # Plot circle using geom_polygon
  ggplot2::geom_polygon(
    ggplot2::aes(x = cp[, 1], y = cp[, 2]),
    ...
  )
})


#'@rdname plot-method
#'
setMethod("plot", "rectangle", function(object, ...) {
  ggplot2::geom_tile(
    ggplot2::aes(x = object@center[[1]], y = object@center[[2]], width = object@size[[1]], height = object@size[[2]]),
    ...
  )
})

#'@rdname plot-method
#'
setMethod("plot", "polygon", function(object, ...) {
  ggplot2::geom_polygon(
    ggplot2::aes(x = object@points[, 1], y = object@points[, 2]), 
    ...
  )
})

#'@rdname plot-method
#'
setMethod("plot", "background", function(object, ...) {
  plt <- ggplot2::ggplot() + 
         predped::plot(shape(object), fill = "white") +
         ggplot2::coord_equal() +
         ggplot2::labs(x = "x", y = "y") +
         ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "black"),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
         )
  for (i in seq_along(objects(object))) {
    plt <- plt + plot(objects(object)[[i]], ...)
  }
  return(plt)
})