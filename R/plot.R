#' Plot an object
#' 
#' @details 
#' Returns a geom whenever providing separate instances of the
#' \code{\link[predped]{object-class}}. Returns a ggpplot when providing 
#' instances of the \code{\link[predped]{background-class}} or of the
#' \code{\link[predped]{state-class}}. Whenever either is provided in a list, 
#' a list containing the respective geoms or ggplots is returned.
#'
#' @param object Object of the \code{\link[predped]{background-class}}, 
#' \code{\link[predped]{object-class}}, or \code{\link[predped]{state-class}}, 
#' or a list containing multiple of these objects.
#' @param agent.linewidth Numeric denoting the width of the line with which to 
#' plot the agent. Defaults to \code{1}.
#' @param plot_goal Logical denoting whether to plot the position of the current
#' goal of the agent together with the agent. Defaults to \code{TRUE}.
#' @param goal.size Numeric denoting the size of the point that denotes the goal 
#' position. Defaults to \code{1}.
#' @param entry.width Numeric denoting the radius of the entrances and exits to 
#' be plotted in the background. Defaults to \code{0.3},
#' @param shape.fill Character defining the fill color of the shape of the 
#' background. Defaults to \code{"white"}.
#' @param shape.color Character defining the color of circumference of the shape 
#' of the background. Defaults to \code{"black"}.
#' @param shape.linewidth Numeric denoting the width of the circumference of the 
#' shape of the background. Also concerns the width of the line of the entrances 
#' and exits. Defaults to \code{1}.
#' @param object.fill Character defining the fill color of the objects contained 
#' in the background. Defaults to \code{"grey"}.
#' @param object.color Character defining the color of the circumference of the 
#' objects contained in the background. Defaults to \code{"black"}.
#' @param object.linewidth Numeric denoting the width of the circumference of 
#' the objects contained in the background. Defaults to \code{1}.
#' @param plot.title.size Numeric denoting the text size of the plot title. 
#' Defaults to \code{10}.
#' @param plot.title.hjust Numeric denoting the position of the plot title, with
#' \code{0} coding for left, \code{1} for right, and \code{0.5} for the middle. 
#' Defaults to \code{0.5}.
#' @param axis.title.size Numeric of the text size of the axis title. Defaults 
#' to \code{10}.
#' @param axis.text.size Numeric denoting the text size of the axis text. 
#' Defaults to \code{8}.
#' @param dark_mode Logical that can toggle the default colorpallette of predped's 
#' dark mode. Defaults to \code{FALSE}.
#' @param ... Additional ggplot arguments passed on to the geoms for the objects.
#'
#' @return Either a geom or a ggplot, depending on the object provided (see 
#' Details).
#' 
#' @docType method
#' 
#' @rdname plot-method
#' 
#' @export
setGeneric("plot", function(object, ...) standardGeneric("plot"))

#'@rdname plot-method
setMethod("plot", "agent", function(object, 
                                    plot_goal = TRUE,
                                    agent.linewidth = 1,
                                    goal.size = 1,
                                    ...) {
    # Determine the orientation of the agent to be plotted
    angle <- object@orientation * 2 * pi / 360

    # Extract the points of the agent and plot those here. The reason why we 
    # use points here is to ensure that the agent can take on all shapes that 
    # they want
    pts <- points(object)
    half <- ifelse(inherits(object, "circle"), size(object), 0.5 * size(object)[1])

    plt <- list(ggplot2::annotate("polygon", 
                                  x = pts[,1], 
                                  y = pts[,2], 
                                  color = object@color,
                                  fill = NA,
                                  linewidth = agent.linewidth),
                ggplot2::annotate("segment", 
                                  x = object@center[[1]], 
                                  y = object@center[[2]], 
                                  xend = object@center[[1]] + half * cos(angle), 
                                  yend = object@center[[2]] + half * sin(angle), 
                                  color = object@color,
                                  linewidth = agent.linewidth))

    # If the agent's goal should be plotted as well, then do so
    if(plot_goal) {
        plt <- append(plt, 
                      ggplot2::geom_point(ggplot2::aes(x = current_goal(object)@position[1],
                                                       y = current_goal(object)@position[2]),
                                          color = object@color,
                                          size = goal.size))
    }

    return(plt)
})

#'@rdname plot-method
setMethod("plot", "background", function(object, 
                                         entry.width = 0.3,
                                         shape.fill = "white",
                                         shape.color = "black",
                                         shape.linewidth = 1,
                                         object.fill = "grey",
                                         object.color = "black",
                                         object.linewidth = 1,
                                         dark_mode = FALSE,
                                         ...) {

    # If you want to toggle the default dark mode, then change the default colors
    if(dark_mode) {
        shape.fill <- "black"
        shape.color <- "white"
        object.fill <- "black"
        object.color <- "white"
    }

    # Create some limits to prevent the limits of the plot changing across 
    # iterations.
    pts <- points(object@shape)
    limits <- rbind(range(pts[,1]), range(pts[,2]))

    # Make the entries into a matrix of segments that can be added to the plot.
    # This is done in several steps.
    
    # Step 1: Bind together the entrances and exits: all they need to be handled 
    # in the same way
    entries <- rbind(entrance(object), exit(object))

    # Step 2: Loop over these entries, convert them to points, and only retain 
    # those that fall into the shape of the background. Then transform them to 
    # a segment structure (x0, y0, x, y)
    segments <- list()
    for(i in seq_len(nrow(entries))) {
        pts <- points(predped::circle(center = entries[i,], radius = entry.width))
        pts <- pts[predped::in_object(shape(object), pts),]

        segments[[i]] <- cbind(pts[2:nrow(pts) - 1,], pts[2:nrow(pts),])
    }

    # Step 3: Bind together these segments
    segments <- do.call("rbind", segments)

    # Finally, with all elements in place, we can now create the plot itself
    return(ggplot2::ggplot() + 
        # Plot the shape and objects of the environment
        predped::plot(shape(object), 
                      fill = shape.fill, 
                      color = shape.color) +
        predped::plot(objects(object), 
                      fill = object.fill, 
                      color = object.color,
                      linewidth = object.linewidth,
                      ...) +
        # Plot the entrances and exits
        ggplot2::annotate("segment", 
                          x = segments[,1], 
                          y = segments[,2], 
                          xend = segments[,3], 
                          yend = segments[,4], 
                          color = shape.color,
                          linewidth = shape.linewidth) +
        # Things related to the general look of the plot
        ggplot2::scale_x_continuous(limits = limits[1,]) +
        ggplot2::scale_y_continuous(limits = limits[2,]) +
        ggplot2::labs(x = "x", 
                      y = "y") +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "black"),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank()) +
        ggplot2::coord_fixed())
})

#'@rdname plot-method
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

#'@rdname plot-method
setMethod("plot", "object", function(object, ...) {
  # Extract the points of the object
  pts <- points(object)
  return(ggplot2::annotate("polygon", 
                           x = pts[,1], 
                           y = pts[,2], 
                           ...))
})

#' @rdname plot-method
setMethod("plot", "state", function(object, 
                                    agent.linewidth = 1, 
                                    goal.size = 1,
                                    plot_goal = TRUE,
                                    plot.title.size = 10,
                                    plot.title.hjust = 0.5,
                                    axis.title.size = 10,
                                    axis.text.size = 8,
                                    ...) {

    # Create the plot for the setting, which will serve as the basis of 
    base_plot <- predped::plot(setting(object), ...) +
        ggplot2::labs(title = paste("iteration", object@iteration)) +
        ggplot2::theme(legend.position = "none",
                       plot.title = ggplot2::element_text(size = plot.title.size,
                                                          hjust = plot.title.hjust),
                       axis.title = ggplot2::element_text(size = axis.title.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

    # If there are currently no agents, then we just return the base_plot
    if(length(object@agents) == 0) {
        return(base_plot)
    }
    
    # Otherwise, we will have to add the agents in the base_plot
    return(base_plot + 
        predped::plot(agents(object), 
                      plot_goal = plot_goal,
                      agent.linewidth = agent.linewidth,
                      goal.size = goal.size,
                      ...))            
})