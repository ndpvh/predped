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
setMethod("plot", "list", function(object, 
                                   trace = FALSE, 
                                   print_progress = TRUE, 
                                   iterations = NULL,
                                   ...) {
    # If the list in question is the trace, then we have to output the plots for
    # each state in the simulation. This is a little more complicated than for 
    # a simple list
    plt <- list()
    if(trace) {
        # Setting doesn't change, so can be saved immediately
        base_plot <- predped::plot(object[[1]]$setting, fill = "grey", color = "black")

        # Loop over each state
        for(i in seq_along(object)) {
            iter <- ifelse(is.null(iterations[i]), i, iterations[i])

            if(print_progress) {
                cat(paste0("\rMaking plot for iteration ", iter))
            }

            # If there are currently no agents, then we just return the base_plot
            if(length(object[[i]]$agents) == 0) {
                plt[[i]] <- base_plot + 
                    ggplot2::labs(title = paste("iteration", iter))

            # Otherwise, we will have to add the agents in the base_plot
            } else {
                # Transform the complete state to a collection of segments
                segments <- matrix(0, nrow = 0, ncol = 5)
                goals <- matrix(0, nrow = 0, ncol = 3)
                color_code <- c()
                for(j in object[[i]]$agents) {
                    # Get coordinates of the agent themselves and turn into 
                    # segments
                    if(inherits(j, "circle")) {
                        agent_coords <- to_polygon(j, length.out = 25)
                    } else {
                        agent_coords <- j@points
                    }
                    agent_segments <- cbind(agent_coords, 
                                            agent_coords[c(2:nrow(agent_coords), 1),])

                    # Keep track of the location of the agent's goals. Furthermore
                    # add a segment that denotes the orientation of the agent
                    agent_goals <- current_goal(j)@position

                    angle <- j@orientation * 2 * pi / 360
                    agent_segments <- rbind(agent_segments, 
                                            c(center(j), 
                                              center(j) + radius(j) * c(cos(angle), sin(angle))))

                    # Add the color to the color code if it is not already in 
                    # there
                    if(!(j@color %in% color_code)) {
                        color_code[j@color] <- j@color
                    }

                    # Add information on the color to the dataframes
                    agent_segments <- cbind(agent_segments, j@color)
                    agent_goals <- c(agent_goals, j@color)

                    segments <- rbind(segments, agent_segments)
                    goals <- rbind(goals, agent_goals)
                }

                # Once done, plot all this information as a collection of segments 
                # and points
                segments <- as.data.frame(segments) |>
                    setNames(c("x", "y", "xend", "yend", "color"))
                goals <- as.data.frame(goals) |>
                    setNames(c("x", "y", "color"))

                plt[[i]] <- suppressWarnings(base_plot +
                    ggplot2::geom_segment(data = segments, 
                                        ggplot2::aes(x = as.numeric(x), 
                                                    y = as.numeric(y), 
                                                    xend = as.numeric(xend),
                                                    yend = as.numeric(yend),
                                                    color = color),
                                        ...) +
                    ggplot2::geom_point(data = goals, 
                                        ggplot2::aes(x = as.numeric(x), 
                                                    y = as.numeric(y), 
                                                    color = color),
                                        ...) +
                    ggplot2::scale_color_manual(values = color_code) +
                    ggplot2::labs(title = paste("iteration", iter)) +
                    ggplot2::theme(legend.position = "none"))
            }
        }

    # If it is not the trace, then we need to output the list of geom's that are
    # created for each of the elements in the list
    } else {
        for(i in seq_along(object)) {
            plt <- append(plt, plot(object[[i]], ...))
        }        
    }

    if(print_progress) {
        cat("\n")
    }

    return(plt)
})


#'@rdname plot-method
#'
setMethod("plot", "background", function(object, 
                                         entry_exit_width = 0.4,
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
    idx <- in_object(object@shape, entrance, outside = FALSE)
    idy <- in_object(object@shape, exit, outside = FALSE)

    entrance <- entrance[idx,]    
    exit <- exit[idy,] 
    
    plt <- plt + plot(polygon(points = entrance), fill = NA, colour = "black")
    plt <- plt + plot(polygon(points = exit), fill = NA, colour = "black")
    return(plt)
})