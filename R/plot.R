source("RCore/PredictivePedestrian.R")
# Alternative approach
coordinate <- setClass("coordinate", contains = "numeric")

setMethod("initialize", "coordinate", function(.Object, ...) {
    .Object <- callNextMethod()

    if (length(.Object) != 2) stop("Coordinate vector must have length two (x and y)")

    names(.Object) <- c("x", "y")

    return(.Object)
})

setClass("object", list(id = "character",
                        moveable = "logical",
                        interactable = "logical"),
         contains = "VIRTUAL")

polygon <- setClass("polygon", list(points = "matrix", clock_wise = "logical"), contains = "object")

setMethod("initialize", "polygon", function(.Object, 
                                            clock_wise = TRUE, 
                                            moveable = FALSE, 
                                            interactable = FALSE,
                                            ...) {
    .Object <- callNextMethod(.Object, ...)

    if (ncol(.Object@points) != 2) {
        stop("All points must have an x- and y-coordinate (two-column matrix)")
    }

    .Object@clock_wise <- clock_wise
    .Object@moveable <- moveable
    .Object@interactable <- interactable

    return(.Object)
})

rectangle <- setClass("rectangle", list(
        center = "numeric",
        size = "numeric",
        orientation = "numeric"
    ),
    contains = c("polygon")
)

setMethod("initialize", "rectangle", function(.Object,
                                              center,
                                              size,
                                              clock_wise = TRUE,
                                              orientation = 0,
                                              moveable = FALSE,
                                              interactable = FALSE,
                                              ...
) {
    if (length(size) != 2) stop("Size vector must have length two (x and y)")
    if (any(size <= 0)) stop("Size vector must be positive")
    if (length(orientation) != 1) stop("Orientation must be a single element")

    .Object@center <- as(center, "coordinate")

    size_half <- size/2

    lower <- center - size_half
    upper <- center + size_half

    points <- matrix(c(lower, c(lower[1], upper[2]), upper, c(upper[1], lower[2])), 4, 2, byrow = TRUE)

    if (orientation != 0) {
        points <- t(apply(points, 1, rotate, radians = orientation, center = center))
    }

    if(!clock_wise) {
        points <- points[4:1,]
    }

    .Object <- callNextMethod(.Object, points = points, clock_wise = clock_wise)

    .Object@size <- size
    .Object@moveable <- moveable
    .Object@orientation <- orientation
    .Object@interactable <- interactable

    return(.Object)
})

circle <- setClass("circle", list(center = "numeric", radius = "numeric"), contains = c("object"))

setMethod("initialize", "circle", function(.Object, moveable = FALSE, interactable = FALSE, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@center <- as(.Object@center, "coordinate")
    .Object@moveable <- moveable
    .Object@interactable <- interactable
    if (length(.Object@radius) != 1) stop("Slot 'radius' should be a single numeric value")
    return(.Object)
})

#'@rdname area-method
#'
setGeneric("area", function(object) {standardGeneric("area")})
setMethod("area", signature(object = "circle"), function(object) pi*object@radius^2)


circular <- circle(center = coordinate(c(2, 3)), radius = 20)
circular@radius

# Plot function for a circle

setGeneric("plot", function(object) {standardGeneric("plot")})
setMethod("plot", "circle", function(object) {
    ggplot2::geom_point(
        ggplot2::aes(x = object@center[[1]], y = object@center[[2]], size = object@radius),
        show.legend = FALSE
    )
})

# Background is not the background itself, rather the entire environment that is generated
# perpendicular orientation
# slope orientation to extract radian which can be used to 
# dictate where to draw rectangles on the wall

# Plot function for a rectangle
setMethod("plot", "rectangle", function(object) {
  ggplot2::geom_tile(
    ggplot2::aes(x = object@center[[1]], y = object@center[[2]], width = object@size[[1]], height = object@size[[2]])
  )
})

rectangular <- rectangle(center = coordinate(c(1,2)), size = c(2,2))

test_rect <- plot(rectangular)

p <- ggplot2::ggplot() +
     ggplot2::xlim(0, 10) +
     ggplot2::ylim(0, 10)

p <- p + test_rect

# Plot function for a polygon 
setMethod("plot", "polygon", function(object) {
  ggplot2::geom_polygon(
    ggplot2::aes(x = c(object@points[1], object@points[2]), y = c(object@points[3], object@points[4]))
  )
})

polygonous <- polygon(points = pts)

pts <- matrix(c(1, 5, 10, 15), ncol = 2)

matrix()

test_polygon <- plot(polygonous)

p <- ggplot2::ggplot() +
     ggplot2::xlim(0, 20) +
     ggplot2::ylim(0, 20)

p <- p + test_polygon
