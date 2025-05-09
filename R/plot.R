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
#' @param agent.fill Character denoting the color with which the agent should
#' be filled in. Defaults to \code{"white"} if only plotting an agent or to
#' \code{shape.fill} when plotting with a \code{background}.
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
#' @param plot_forbidden Logical denoting whether to plot forbidden edges
#' explicitly. Defaults to \code{FALSE}.
#' @param forbidden.color Character denoting the color of the forbidden edges.
#' Defaults to \code{"red"}.
#' @param plot_segment Logical denoting whether to plot segments (if there are
#' any). If \code{TRUE}, it will add arrows to the plot that indicate the
#' direction in which agents can walk. These arrows will be placed around the
#' center of the line created by the instances of the
#' \code{\link[predped]{segment-class}}. Defaults to \code{TRUE}.
#' @param segment.color Character defining the color of the arrows drawn when
#' you impose one-directional flow. Ignored if \code{plot_segment = FALSE}.
#' Defaults to \code{"black"}.
#' @param segment.size Numeric denoting the length of the arrow-line drawn when
#' you impose one-directional flow. Ignored if \code{plot_segment = FALSE}.
#' Defaults to \code{0.6}.
#' @param arrow.size Numeric denoting the size of the arrow-heads drawn when
#' you impose one-directional flow in cm. Ignored if \code{plot_segment = FALSE}.
#' Defaults to \code{0.3}.
#' @param segment.linewidth Numeric denoting the linewidth of the arrows drawn
#' when you impose one-directional flow. Ignored if \code{plot_segment = FALSE}.
#' Defaults to \code{1}.
#' @param segment.hjust Numeric bounded between 0 and 1 which defines how to
#' place the arrow relative to the center of the segment. If \code{0.5}, the
#' arrow's center coincides with the center of the segment. If \code{0}, the
#' arrow's tail will coincide with the center of the segment, while if \code{1},
#' the arrow's head will coincide with this center. Defaults to \code{0.5}.
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
#' @param optimize Logical that defines whether to use an optimized version of 
#' the plotting or an unoptimized version. Defaults to \code{TRUE}.
#' @param print_progress Logical that denotes whether to print the iteration of 
#' the state that is currently being plotted. Only applies when plotting a trace.
#' Defaults to \code{TRUE}.
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
                                    agent.fill = "white",
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
                                  fill = agent.fill,
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
                                         plot_segment = TRUE,
                                         shape.fill = "white",
                                         shape.color = "black",
                                         shape.linewidth = 1,
                                         object.fill = "grey",
                                         object.color = "black",
                                         object.linewidth = 1,
                                         segment.color = "black",
                                         segment.linewidth = 1,
                                         segment.size = 0.6,
                                         arrow.size = 0.3,
                                         segment.hjust = 0.5,
                                         plot_forbidden = FALSE,
                                         forbidden.color = "red",
                                         dark_mode = FALSE,
                                         optimize = TRUE,
                                         ...) {

    # If you want to toggle the default dark mode, then change the default colors
    if(dark_mode) {
        shape.fill <- "black"
        shape.color <- "white"
        object.fill <- "black"
        object.color <- "white"
        segment.color <- "white"
    }

    # Create some limits to prevent the limits of the plot changing across
    # iterations.
    pts <- points(object@shape)
    limits <- rbind(range(pts[,1]), range(pts[,2]))

    # Check whether you want to optimize the plot. If you do, then the objects
    # and agents are first transformed to one comprehensive dataframe that allows
    # plotting within a single `geom_polygon`. If you don't, then the objects
    # and agents will all be plotted with their own `plot` functions, leading
    # to a lot of `geom`s being added together.
    if(optimize) {
        pts <- transform_df(object, 
                            entry.width = entry.width)        
        plt <- ggplot2::ggplot(data = pts, 
                               ggplot2::aes(x = x, 
                                            y = y, 
                                            group = group, 
                                            fill = kind, 
                                            color = kind, 
                                            linewidth = kind)) +
            ggplot2::geom_polygon() +
            ggplot2::scale_fill_manual(values = c("shape" = shape.fill, 
                                                  "object" = object.fill)) +
            ggplot2::scale_color_manual(values = c("shape" = shape.color, 
                                                   "object" = object.color)) +
            ggplot2::scale_linewidth_manual(values = c("shape" = shape.linewidth, 
                                                       "object" = object.linewidth))

        

        # Also check whether there is something forbidden going on.
        if(plot_forbidden) {
            segments <- transform_df(object@objects, 
                                     plot_forbidden = TRUE)

            # Plot these edges
            plt <- plt + 
                ggplot2::annotate("segment", 
                                  x = segments$x,
                                  y = segments$y,
                                  xend = segments$xend,
                                  yend = segments$yend,
                                  color = forbidden.color,
                                  linewidth = object.linewidth)
        }

            # ggplot2::geom_polygon(data = shape_pts,
            #                       ggplot2::aes(x = x,
            #                                    y = y,
            #                                    group = group),
            #                       fill = shape.fill,
            #                       color = shape.color,
            #                       linewidth = shape.linewidth) +
            # ggplot2::geom_polygon(data = object_pts,
            #                       ggplot2::aes(x = x,
            #                                    y = y,
            #                                    group = group),
            #                       fill = object.fill,
            #                       color = object.color,
            #                       linewidth = object.linewidth,
            #                       ...)

    } else {
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

        # With all elements in place, we can now create the plot itself
        plt <- ggplot2::ggplot() +
            # Plot the shape and objects of the environment
            predped::plot(shape(object),
                          fill = shape.fill,
                          color = shape.color) +
            predped::plot(objects(object),
                          fill = object.fill,
                          color = object.color,
                          linewidth = object.linewidth,
                          plot_forbidden = plot_forbidden,
                          forbidden.color = forbidden.color,
                          ...) +
            # Plot the entrances and exits
            ggplot2::annotate("segment",
                              x = segments[,1],
                              y = segments[,2],
                              xend = segments[,3],
                              yend = segments[,4],
                              color = shape.color,
                              linewidth = shape.linewidth)
    }

    # Add general theme things to the plot
    plt <- plt +
        # Things related to the general look of the plot
        ggplot2::scale_x_continuous(limits = limits[1,]) +
        ggplot2::scale_y_continuous(limits = limits[2,]) +
        ggplot2::labs(x = "x",
                      y = "y") +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "black"),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       legend.position = "none") +
        ggplot2::coord_fixed()

    # If you want to plot the segments as well, add these to the plot
    if(plot_segment) {
        plt <- plt +
            predped::plot(limited_access(object),
                          segment.color = segment.color,
                          segment.size = segment.size,
                          segment.linewidth = segment.linewidth,
                          arrow.size = arrow.size,
                          segment.hjust = segment.hjust)
    }

    return(plt)
})

#'@rdname plot-method
setMethod("plot", "list", function(object, print_progress = TRUE, ...) {

    # First a check of whether anything is contained within this list. Otherwise
    # return an error
    if(length(object) == 0) {
        return(list())
    }

    # If the list in question is the trace, then we have to output the plots for
    # each state in the simulation. Otherwise, we have to output all the geom's
    # that are in the trace. In both cases, we can simply dispatch based on the
    # elements of the list, and this can thus be as simple as having an lapply
    #
    # However, if it is a trace, we would like some information on the iteration
    # of the state, which we need to do some bookkeeping for
    trace <- inherits(object[[1]], "state")

    if(trace & print_progress) {
        cat("\n")
    }

    plt <- lapply(seq_along(object),
                  function(i) {
                      if(trace & print_progress) {
                          cat(paste0("\rMaking plot for state ", object[[i]]@iteration))
                      }

                      return(predped::plot(object[[i]], ...))
                  })

    if(trace & print_progress) {
        cat("\n")
    }

    return(plt)
})

#'@rdname plot-method
setMethod("plot", "object", function(object,
                                     color = "black",
                                     fill = "gray",
                                     plot_forbidden = FALSE,
                                     forbidden.color = "red",
                                     ...) {

    # Extract the points of the object
    pts <- points(object)

    # Create the basic geom
    plt <- ggplot2::annotate("polygon",
                             x = pts[,1],
                             y = pts[,2],
                             color = color,
                             fill = fill,
                             ...)

    # If you should plot forbidden edges, identify these and plot them
    # altogether
    if(plot_forbidden) {
        # Check whether the object can be interacted with. If not, then we can 
        # just annotate the same polygon but then with the forbidden.color
        if(!object@interactable) {
            return(ggplot2::annotate("polygon", 
                                     x = pts[,1], 
                                     y = pts[,2], 
                                     color = forbidden.color, 
                                     fill = fill, 
                                     ...))
        }

        # If only some parts cannot be interacted with, then we need to plot 
        # these edges separately. First, create the edges themselves
        edges <- cbind(pts, pts[c(2:nrow(pts), 1), ])

        # If it is a circle, we need to find the indices of those edges that are
        # forbidden and those who are not.
        if(inherits(object, "circle")) {
            # Get relative angle from center to points and correct so that they
            # are positive
            angles <- atan2(pts[,2] - center(object)[2],
                            pts[,1] - center(object)[1])
            angles <- ifelse(angles < 0, angles + 2 * pi, angles)

            # Extract the forbidden angles from the circle
            forbidden <- forbidden(object)

            if(nrow(forbidden) == 0) {
                idx <- logical(0)
            } else {
                idx <- lapply(1:nrow(forbidden),
                              \(i) angles >= forbidden[i, 1] & angles <= forbidden[i, 2])
                idx <- Reduce("|", idx)
            }
        } else {
            idx <- forbidden(object)
        }

        # Add another annotate with a different color
        plt <- list(plt,
                    ggplot2::annotate("segment",
                                      x = edges[idx, 1],
                                      y = edges[idx, 2],
                                      xend = edges[idx, 3],
                                      yend = edges[idx, 4],
                                      color = forbidden.color,
                                      ...))
    }

    return(plt)
})

#'@rdname plot-method
setMethod("plot", "segment", function(object,
                                      segment.size = 0.6,
                                      segment.color = "black",
                                      segment.linewidth = 1,
                                      arrow.size = 0.3,
                                      segment.hjust = 0.5) {

    # Get the orientation of the segment and subtract 90 degrees
    angle <- orientation(object) - pi / 2

    # Define a line of length `segment.size` centered around a given `center`.
    # To compute this center, you use the `hjust` argument.
    center <- center(object)

    # Adjust the center if you want the arrow to start at the center of the
    # segment
    center <- center + (segment.hjust - 0.5) * segment.size * c(cos(angle), sin(angle))
    from <- center + 0.5 * segment.size * c(cos(angle), sin(angle))
    to <- center - 0.5 * segment.size * c(cos(angle), sin(angle))

    # Create the actual arrow
    return(ggplot2::annotate("segment",
                             x = from[1],
                             y = from[2],
                             xend = to[1],
                             yend = to[2],
                             arrow = ggplot2::arrow(length = ggplot2::unit(arrow.size, "cm")),
                             color = segment.color,
                             linewidth = segment.linewidth))
})

#' @rdname plot-method
setMethod("plot", "state", function(object,
                                    agent.linewidth = 1,
                                    plot.title.size = 10,
                                    plot.title.hjust = 0.5,
                                    axis.title.size = 10,
                                    axis.text.size = 8,
                                    plot_goal = TRUE,
                                    goal.size = 2,
                                    plot_segment = TRUE,
                                    agent.fill = shape.fill,
                                    shape.fill = "white",
                                    shape.color = "black",
                                    shape.linewidth = 1,
                                    object.fill = "grey",
                                    object.color = "black",
                                    object.linewidth = 1,
                                    segment.color = "black",
                                    segment.linewidth = 1,
                                    segment.size = 0.6,
                                    arrow.size = 0.3,
                                    segment.hjust = 0.5,
                                    plot_forbidden = FALSE,
                                    forbidden.color = "red",
                                    dark_mode = FALSE,
                                    optimize = TRUE,
                                    ...) {

    # Change colors of shape and agent if needed
    if(dark_mode) {
        agent.fill <- "black"
        shape.fill <- "black"
        shape.color <- "white"
        object.fill <- "black"
        object.color <- "white"
        segment.color <- "white"

        # If any of the agents have a "black" color, we need to change that to 
        # white. Makes sure agents without specified color can still be seen.
        agents(object) <- lapply(agents(object),
                                 function(x) {
                                     if(color(x) == "black") {
                                         color(x) <- "white"
                                     }
 
                                     return(x)
                                 })
    }

    # Change the goal.size if we are in the optimized scenario. Allows users to
    # more easily change the values without having to think about very small
    # numbers
    if(optimize) {
        goal.size <- goal.size / 100
    }

    if(optimize) {
        # Create all polygons that are needed
        pts <- transform_df(object,
                            plot_goal = plot_goal,
                            goal.size = goal.size,
                            ...)

        # Get the limits of the shape
        tmp <- points(setting(object)@shape)
        lims <- rbind(range(tmp[,1]), range(tmp[,2]))

        # Create the different lists
        names_lists <- unique(pts$kind)

        colors <- unique(pts$kind)
        names(colors) <- names_lists
        colors["shape"] <- shape.color
        colors["object"] <- object.color

        fills <- rep(shape.fill, each = length(names_lists))
        names(fills) <- names_lists
        fills["object"] <- object.fill

        linewidths <- rep(agent.linewidth, each = length(names_lists))
        names(linewidths) <- names_lists
        linewidths["shape"] <- shape.linewidth
        linewidths["object"] <- object.linewidth

        # Create the plot itself
        plt <- ggplot2::ggplot(data = pts, 
                               ggplot2::aes(x = x, 
                                            y = y, 
                                            group = group, 
                                            fill = kind, 
                                            color = kind, 
                                            linewidth = kind)) +
            ggplot2::geom_polygon() +
            # The different specifications per kind of object
            ggplot2::scale_fill_manual(values = fills) +
            ggplot2::scale_color_manual(values = colors) +
            ggplot2::scale_linewidth_manual(values = linewidths) +
            # Titles and axes
            ggplot2::labs(title = paste("iteration", object@iteration), 
                          x = "x", 
                          y = "y") +
            ggplot2::scale_x_continuous(limits = lims[1,]) +
            ggplot2::scale_y_continuous(limits = lims[2,]) +
            # Theme
            ggplot2::theme(panel.background = ggplot2::element_rect(fill = "black"),
                           panel.grid.major = ggplot2::element_blank(),
                           panel.grid.minor = ggplot2::element_blank(),
                           plot.title = ggplot2::element_text(size = plot.title.size,
                                                              hjust = plot.title.hjust),
                           axis.title = ggplot2::element_text(size = axis.title.size),
                           axis.text = ggplot2::element_text(size = axis.text.size), 
                           legend.position = "none") +
            ggplot2::coord_fixed()

        # Check whether to add the segments to the plot, and if so, add them
        # to the plot
        if(plot_segment) {
            segments <- transform_df(setting(object)@limited_access,
                                     segment.size = segment.size, 
                                     segment.hjust = segment.hjust)

            # Create the actual arrows
            plt <- plt + 
                ggplot2::annotate("segment", 
                                  x = segments$x,
                                  y = segments$y,
                                  xend = segments$xend,
                                  yend = segments$yend,
                                  arrow = ggplot2::arrow(length = ggplot2::unit(arrow.size, "cm")),
                                  color = segment.color,
                                  linewidth = segment.linewidth)
        }

        # Also check whether there is something forbidden going on.
        if(plot_forbidden) {
            segments <- transform_df(setting(object)@objects, 
                                     plot_forbidden = TRUE)

            # Plot these edges
            plt <- plt + 
                ggplot2::annotate("segment", 
                                  x = segments$x,
                                  y = segments$y,
                                  xend = segments$xend,
                                  yend = segments$yend,
                                  color = forbidden.color,
                                  linewidth = object.linewidth)
        }

        return(plt)

    } else {

        # Create the plot for the setting, which will serve as the basis of
        base_plot <- predped::plot(setting(object),
                                   shape.fill = shape.fill,
                                   dark_mode = dark_mode,
                                   optimize = optimize,
                                   plot_segment = plot_segment,
                                   plot_forbidden = plot_forbidden, 
                                   forbidden.color = forbidden.color,
                                   ...) +
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

        # Add agents through their innate plot-function
        return(base_plot +
            predped::plot(agents(object),
                          plot_goal = plot_goal,
                          agent.fill = agent.fill,
                          agent.linewidth = agent.linewidth,
                          goal.size = goal.size,
                          ...))
    }
})

#' Plot the edges in a \code{\link[predped]{background-class}}
#'
#' The color of the edges is determined by the argument \code{object.color},
#' thus following the color of the outer lines of the objects in the background.
#' Note that \code{dark_mode} also works here!
#'
#' @param setting Object of the \code{\link[predped]{background-class}}
#' @param coords Numeric matrix containing the positions of the agent and the
#' goal. Defaults to \code{NULL}, leaving them out of the equation.
#' @param space_between Numeric denoting the space to leave between the
#' circumference of the object and the nodes created under the hood (see
#' \code{\link[predped]{add_nodes}}). Defaults to \code{2.5} times the maximal
#' radius of an agent.
#' @param many_nodes Logical denoting whether to create many nodes or leave
#' it at the minimum. Defaults to \code{FALSE}.
#' @param coords.color Character denoting the color to provide to the agent and
#' goal positions. Allows one to distinguish between simple path points and the
#' start and end positions. Defaults to \code{"cornflowerblue"}.
#' @param edges.color Character denoting the color of the edges that are
#' plotted. Defaults to \code{"black"}.
#' @param edges.linewidth Numeric denoting the linewidth of the edges that are
#' plotted. Defaults to \code{1}.
#' @param nodes.color Character denoting the color of the nodes that are plotted.
#' Defaults to \code{"black"}.
#' @param nodes.size Numeric denoting the size of the nodes that are plotted.
#' Defaults to \code{1}.
#' @param dark_mode Logical denoting whether to use the predped-default dark
#' mode for plotting. Overrides the color arguments provided to the plotting
#' function. Defaults to \code{FALSE}.
#' @param ... Arguments provided to the \code{\link[predped]{plot}} function
#' for the \code{setting}
#'
#' @return Plot created by \code{ggplot2}
#'
#' @seealso
#' \code{\link[predped]{background-class}},
#' \code{\link[predped]{compute_edges}},
#' \code{\link[predped]{create_edges}},
#' \code{\link[predped]{plot}}
#'
#' @rdname plot_edges
#'
#' @export
plot_edges <- function(setting,
                       coords = NULL,
                       space_between = 1.25 * max(params_from_csv[["params_bounds"]]["radius", ]),
                       many_nodes = FALSE,
                       coords.color = "cornflowerblue",
                       edges.color = "black",
                       edges.linewidth = 1,
                       nodes.color = "black",
                       nodes.size = 1,
                       dark_mode = FALSE,
                       ...) {

    # If you want to toggle the default dark mode, then change the default colors
    if(dark_mode) {
        edges.color <- "gray25"
        nodes.color <- "gray25"
    }

    # Compute the edges. If `coords` is provided, it will create all edges
    # including the provided coordinates. If coords is not provided, we will
    # only plot the default edges
    if(is.null(coords)) {
        edges <- compute_edges(setting,
                               space_between = space_between,
                               many_nodes = many_nodes)
    } else {
        edges <- create_edges(coords[1,],
                              coords[2,],
                              setting,
                              space_between = space_between,
                              many_nodes = many_nodes)
    }

    # Extract nodes and edges in two separate dataframes
    nodes <- edges$nodes
    edges <- edges$edges_with_coords

    # Plot everything. Start with the background, which will already provide us
    # with a nice shell. Then proceed in plotting all of the edges, followed
    # by the nodes that make up these edges. Whenever a start and end coordinate
    # are provided, plot these separately as well.
    plt <- plot(setting,
                dark_mode = dark_mode,
                ...) +
        ggplot2::annotate("segment",
                          x = edges$from_x,
                          y = edges$from_y,
                          xend = edges$to_x,
                          yend = edges$to_y,
                          color = edges.color,
                          linewidth = edges.linewidth) +
        ggplot2::annotate("point",
                          x = nodes$X,
                          y = nodes$Y,
                          color = nodes.color,
                          size = nodes.size)

    if(!is.null(coords)) {
        plt <- plt +
            ggplot2::annotate("point",
                              x = coords[,1],
                              y = coords[,2],
                              color = coords.color,
                              size = nodes.size)
    }

    return(plt)
}





#' Transform to dataframe of segments
#'
#' @rdname transform_df-method
setGeneric("transform_df", function(object,...) standardGeneric("transform_df"))

#' @rdname transform_df-method
setMethod("transform_df", "agent", function(object,
                                            plot_goal = TRUE,
                                            goal.size = 1) {

    # Determine the orientation of the agent to be plotted
    angle <- object@orientation * 2 * pi / 360

    # Extract the points of the object and create a group-id
    pts <- points(object)
    group <- rep(id(object), each = nrow(pts))

    # Using points here and using the correction add a segment that contains the
    # orientation of the agent. By using `half`, we can adjust any object shape
    # the agent can have. Give this segment its own identifier
    half <- ifelse(inherits(object, "circle"), size(object), 0.5 * size(object)[1])
    pts <- rbind(pts,
                 rbind(object@center,
                       object@center + half * c(cos(angle), sin(angle))))
    group <- c(group, rep(paste(id(object), "segment"), each = 2))

    # Check whether the goal should also be plotted. And if so, add this to
    # polygons to be outputted
    if(plot_goal) {
        # Create the goal points and add them to the list
        angles <- seq(0, 2 * pi, length.out = 10)
        goal_pts <- goal.size * cbind(cos(angles), sin(angles))
        goal_pts <- cbind(current_goal(object)@position[1] + goal_pts[,1],
                          current_goal(object)@position[2] + goal_pts[,2])

        pts <- rbind(pts, goal_pts)
        group <- c(group, rep(paste(id(object), "goal"), each = 10))
    }

    # Manipulate to have a dataframe containing information on the segments as
    # well as the what this object is (used to determine colors, which polygon
    # is which etc)
    return(data.frame(x = pts[,1],
                      y = pts[,2],
                      group = group,
                      kind = rep(object@color, each = nrow(pts))))
})

#' @rdname transform_df-method
setMethod("transform_df", "background", function(object, 
                                                 entry.width = 0.3,
                                                 ...) {
    
    # Create some limits to prevent the limits of the plot changing across
    # iterations.
    pts <- points(object@shape)
    limits <- rbind(range(pts[,1]), range(pts[,2]))

    # Get the points that make up the shape of the background as well as the
    # entries and exits.
    pts <- points(object@shape)
    pts <- data.frame(x = pts[,1],
                      y = pts[,2],
                      group = rep("0 shape", each = nrow(pts)),
                      kind = rep("shape", each = nrow(pts)))

    # Step 1: Bind together the entrances and exits: all they need to be handled
    # in the same way
    entries <- rbind(entrance(object), exit(object))

    # Step 2: Loop over these entries, convert them to points, and only retain
    # those that fall into the shape of the background. Then transform them to
    # a segment structure (x0, y0, x, y)
    angles <- seq(0, 2 * pi, length.out = 100)
    entries <- lapply(1:nrow(entries),
                      \(x) data.frame(x = entries[x,1] + entry.width * cos(angles),
                                      y = entries[x,2] + entry.width * sin(angles),
                                      group = rep(paste("1 entry", x), each = 100),
                                      kind = rep("shape", each = 100)))

    # Step 3: Retain those entrances that fall within the specified bounds
    # of the shape
    entries <- do.call("rbind", entries)
    entries <- entries[in_object(object@shape, entries[, c("x", "y")]), ]

    # Return all points created here
    return(rbind(pts, 
                 entries, 
                 transform_df(object@objects)))
})

#'@rdname transform_df-method
setMethod("transform_df", "list", function(object,
                                           ...) {

    # Loop over all elements in the list and transform them accordingly
    pts <- lapply(object, \(x) transform_df(x, ...))

    # Bind them together and return
    return(do.call("rbind", pts))
})

#' @rdname transform_df-method
setMethod("transform_df", "object", function(object,
                                             kind = "object",
                                             plot_forbidden = FALSE,
                                             ...) {

    # Extract the points of the object
    pts <- points(object)

    # Manipulate to have a dataframe containing information on the segments as
    # well as the what this object is (used to determine colors, which polygon
    # is which etc)
    data <- data.frame(x = pts[,1],
                       y = pts[,2],
                       group = rep(paste("1", id(object)), each = nrow(pts)),
                       kind = rep(kind, each = nrow(pts)))

    # If you should plot forbidden edges, identify these and put them in a 
    # separate dataframe.
    if(plot_forbidden) {
        # If the object cannot be fully interacted with, then we need to separate
        # these edges from the dataframe.
        edges <- cbind(pts, pts[c(2:nrow(pts), 1), ])

        # Check whether the object can be interacted with. If not, then we can 
        # return all edges with the tag "forbidden"
        if(!object@interactable) {
            return(data.frame(x = edges[, 1],
                              y = edges[, 2],
                              xend = edges[, 3],
                              yend = edges[, 4],
                              group = rep(paste("2", id(object), "forbidden"), each = nrow(edges)),
                              kind = rep("forbidden", each = nrow(edges))))
        }

        # If it is a circle, we need to find the indices of those edges that are
        # forbidden and those who are not.
        if(inherits(object, "circle")) {
            # Get relative angle from center to points and correct so that they
            # are positive
            angles <- atan2(pts[,2] - center(object)[2],
                            pts[,1] - center(object)[1])
            angles <- ifelse(angles < 0, angles + 2 * pi, angles)

            # Extract the forbidden angles from the circle
            forbidden <- forbidden(object)

            if(nrow(forbidden) == 0) {
                idx <- logical(0)
            } else {
                idx <- lapply(1:nrow(forbidden),
                              \(i) angles >= forbidden[i, 1] & angles <= forbidden[i, 2])
                idx <- Reduce("|", idx)
            }
        } else {
            idx <- 1:nrow(points(object)) %in% forbidden(object)
        }

        # Add another annotate with a different color
        data <- data.frame(x = edges[idx, 1],
                           y = edges[idx, 2],
                           xend = edges[idx, 3],
                           yend = edges[idx, 4],
                           group = rep(paste("2", id(object), "forbidden"), each = sum(idx)),
                           kind = rep("forbidden", each = sum(idx)))
    }

    return(data)
})

#' @rdname transform_df-method
setMethod("transform_df", "segment", function(object,
                                              kind = "segment",
                                              segment.hjust = 0.5,
                                              segment.size = 0.6,
                                              ...) {
    
    # Get the orientation of the segment and subtract 90 degrees
    angle <- orientation(object) - pi / 2

    # Define a line of length `segment.size` centered around a given `center`.
    # To compute this center, you use the `hjust` argument.
    center <- center(object)

    # Adjust the center if you want the arrow to start at the center of the
    # segment
    center <- center + (segment.hjust - 0.5) * segment.size * c(cos(angle), sin(angle))
    from <- center + 0.5 * segment.size * c(cos(angle), sin(angle))
    to <- center - 0.5 * segment.size * c(cos(angle), sin(angle))
   
    # Manipulate to have a dataframe containing information on the segments
    return(data.frame(x = from[1],
                      y = from[2],
                      xend = to[1],
                      yend = to[2],
                      group = paste("1", id(object)),
                      kind = kind))
})

#' @rdname transform_df-method
setMethod("transform_df", "state", function(object, 
                                            entry.width = 0.3, 
                                            plot_goal = TRUE,
                                            goal.size = 2/100) {

    # Create a plot for each of its components
    return(rbind(transform_df(object@setting, 
                              entry.width = entry.width),
                 transform_df(object@agents, 
                              plot_goal = plot_goal, 
                              goal.size = goal.size)))
})

