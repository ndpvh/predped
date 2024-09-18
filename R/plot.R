#' Plot an Object.
#'
#' @param object An object of kind \code{\link[predped]{object-class}}
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
    # Determine the orientation of the agent to be plotted
    angle <- object@orientation * 2 * pi / 360

    # Plot a circle with the 
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
setMethod("plot", "background", function(object, 
                                         entry_exit_width = 0.4,
                                         shape.fill = "white",
                                         object.fill = "grey",
                                         object.color = "black",
                                         ...) {
    # Not my preferred way of doing things: 
    #
    # When making gifs out of the plots we create, the boundaries move around 
    # with the agent when they get close to the sides of the grid. Changing the 
    # `expand` argument did not fix that, so we have to fix the limits of the 
    # plot manually. This is what I am doing here. 
    if(inherits(shape(object), "circle")) {
        circ <- shape(object)
        xlims <- center(circ)[1] + c(-radius(circ), radius(circ))
        ylims <- center(circ)[2] + c(-radius(circ), radius(circ))
    } else {
        xlims <- range(shape(object)@points[,1])
        ylims <- range(shape(object)@points[,2])
    }

    x_padding <- 0.05 * (xlims[2] - xlims[1])
    y_padding <- 0.05 * (ylims[2] - ylims[1])

    plt <- ggplot2::ggplot() + 
        # Plot the shape and objects of the environment
        predped::plot(shape(object), 
                      fill = shape.fill) +
        predped::plot(objects(object), 
                      fill = object.fill, 
                      color = object.color,
                      ...) +
        # Other things to make the plot beautiful
        ggplot2::coord_fixed() +
        ggplot2::scale_x_continuous(limits = c(xlims[1] - x_padding, xlims[2] + x_padding)) +
        ggplot2::scale_y_continuous(limits = c(ylims[1] - y_padding, ylims[2] + y_padding)) +
        ggplot2::labs(x = "x", y = "y") +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "black"),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank()) 

    # for (i in seq_along(objects(object))) {
    #     plt <- plt + plot(objects(object)[[i]], ...)
    # }

    # Convert entrance & exit to polygon coordinates
    entrance <- lapply(seq_len(nrow(object@entrance)), 
                       \(i) points(circle(center = c(object@entrance[i, 1], object@entrance[i, 2]), 
                                       radius = (entry_exit_width / 2))))
    exit <- lapply(seq_len(nrow(object@exit)), 
                   \(i) points(circle(center = c(object@exit[i, 1], object@exit[i, 2]), 
                                   radius = (entry_exit_width / 2))))
    
    # Check that polygon coordinates are in the background
    # If not, set those coorinates to NA
    for(i in seq_along(entrance)) {
        idx <- in_object(object@shape, entrance[[i]], outside = FALSE)
        plt <- plt + plot(polygon(points = entrance[[i]][idx,]), fill = NA, colour = "black")
    } 

    for(i in seq_along(exit)) {
        idx <- in_object(object@shape, exit[[i]], outside = FALSE)
        plt <- plt + plot(polygon(points = exit[[i]][idx,]), fill = NA, colour = "black")
    }
    
    return(plt)
})

#' @rdname plot-method
#' 
setMethod("plot", "state", function(object, 
                                    plot_goal = TRUE,
                                    plot.title.size = 10,
                                    plot.title.hjust = 0.5,
                                    axis.title.size = 10,
                                    ...) {

    # Create the plot for the setting, which will serve as the basis of 
    base_plot <- predped::plot(setting(object), ...)

    # If there are currently no agents, then we just return the base_plot
    if(length(object@agents) == 0) {
        return(base_plot)
    }
    
    # Otherwise, we will have to add the agents in the base_plot
    return(base_plot + 
        predped::plot(agents(object), 
                      plot_goal = plot_goal,
                      ...) +
        ggplot2::labs(title = paste("iteration", object@iteration)) +
        ggplot2::theme(legend.position = "none",
                       plot.title = ggplot2::element_text(size = plot.title.size,
                                                          hjust = plot.title.hjust),
                       axis.title = ggplot2::element_text(size = axis.title.size)))                
})

#'@rdname plot-method
#'
setMethod("plot", "list", function(object, ...) {
    
    # First a check of whether anything is contained within this list. Otherwise
    # return an error
    if(length(object) == 0) {
        stop("No elements are contained within this list")
    }

    # If the list in question is the trace, then we have to output the plots for
    # each state in the simulation. Otherwise, we have to output all the geom's 
    # that are in the trace. In both cases, we can simply dispatch based on the 
    # elements of the list, and this can thus be as simple as having an lapply
    #
    # However, if it is a trace, we would like some information on the iteration
    # of the state, which we need to do some bookkeeping for
    trace <- inherits(object[[1]], "state")

    if(trace) {
        cat("\n")
    }

    plt <- lapply(seq_along(object),
                  function(i) {
                      if(trace) {
                          cat(paste0("\rMaking plot for state ", object[[i]]@iteration))
                      }

                      return(predped::plot(object[[i]], ...))
                  })

    if(trace) {
        cat("\n")
    }

    return(plt)
})