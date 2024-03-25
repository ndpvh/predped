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
  ggplot2::geom_polygon(
    ggplot2::aes(x = object@points[,1], y = object@points[,2]),
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
setMethod("plot", "agent", function(object, plot_goal = TRUE,...) {
  angle <- object@orientation * 2 * pi / 360
  plt <- list(plot(circle(center = object@center, 
                          radius = object@radius), 
                   fill = NA,
                   color = object@color,
                   ...),
              ggplot2::annotate("segment", 
                                x = object@center[[1]], 
                                y = object@center[[2]], 
                                xend = object@center[[1]] + object@radius * cos(angle), 
                                yend = object@center[[2]] + object@radius * sin(angle), 
                                color = object@color,
                                ...))

    if(plot_goal) {
        plt <- append(plt, 
                      ggplot2::geom_point(ggplot2::aes(x = current_goal(object)@position[1],
                                                       y = current_goal(object)@position[2]),
                                          color = object@color,
                                          ...))
    }

    return(plt)
})

#'@rdname plot-method
#'
setMethod("plot", "list", function(object, ...) {
    plt <- list()
    for(i in seq_along(object)) {
        plt <- append(plt, plot(object[[i]], ...))
    }
    return(plt)
})


#'@rdname plot-method
#'
setMethod("plot", "background", function(object, 
                                         entry_exit_width,
                                         ...) {
    # Not my preferred way of doing things: 
    #
    # When making gifs out of the plots we create, the boundaries move around 
    # with the agent when they get close to the sides of the grid. Changing the 
    # `expand` argument did not fix that, so we have to fix the limits of the 
    # plot manually. This is what I am doing here. 
    xlims <- range(shape(object)@points[,1])
    ylims <- range(shape(object)@points[,2])

    x_padding <- 0.05 * (xlims[2] - xlims[1])
    y_padding <- 0.05 * (ylims[2] - ylims[1])

    plt <- ggplot2::ggplot() + 
        predped::plot(shape(object), fill = "white") +
        ggplot2::coord_fixed() +
        ggplot2::scale_x_continuous(limits = c(xlims[1] - x_padding, xlims[2] + x_padding)) +
        ggplot2::scale_y_continuous(limits = c(ylims[1] - y_padding, ylims[2] + y_padding)) +
        ggplot2::labs(x = "x", y = "y") +
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "black"),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank()
        )
    for (i in seq_along(objects(object))) {
        plt <- plt + plot(objects(object)[[i]], ...)
    }

    # Convert entrance & exit to polygon coordinates
    entrance <- to_polygon(circle(center = c(object@entrance[1], object@entrance[2]), 
                                  radius = (entry_exit_width / 2)))
    exit <- to_polygon(circle(center = c(object@exit[1], object@exit[2]), 
                              radius = (entry_exit_width / 2)))
    
    # Check that polygon coordinates are in the background
    # If not, set those coorinates to NA                   
    for (i in seq_len(nrow(entrance))) {
      tst <- in_object(object@shape, entrance[i,])
      if (tst) {
        entrance[i,] <- NA
      }
    }
    
    # Remove the NA values to make half moon shape
    entrance <- na.omit(entrance)
    for (i in seq_len(nrow(exit))) {
      tst <- in_object(object@shape, exit[i,])
      if (tst) {
        exit[i,] <- NA
      }
    }
    exit <- na.omit(exit)
    plt <- plt + plot(polygon(points = entrance), fill = NA, colour = "black")
    plt <- plt + plot(polygon(points = exit), fill = NA, colour = "black")
    return(plt)
})