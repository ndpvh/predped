## Generate a Random Distribution of Shelves 

# set a class for shelf distribution
ShelfDistribution <- setClass("ShelfDistribution",
                               slots = list(num_cols = "numeric",
                                            num_shelves = "numeric",
                                            shelves_dist = "numeric"))

# set an initialization for the Class ShelfDistribution
setMethod("initialize", "ShelfDistribution", function(.Object, num_cols, num_shelves, shelves_dist, ...) {
    .Object@num_cols <- num_cols
    .Object@num_shelves <- num_shelves
    .Object@shelves_dist <- shelves_dist
    return(.Object)
})

# define the generic
setGeneric("generate_dist",
           function(object) {
               standardGeneric("generate_dist")
           })

# define a method to generate the distribution
setMethod("generate_dist", "ShelfDistribution", function(object) {
    num_cols <- sample(1:10, 1)  # random nr of cols between 1-10
    num_shelves <- max(sample(1:20, 1), num_cols)  # random nr of shelves between 1-20

    # initialize the distribution
    shelves_dist <- rep(0, num_cols)

    # make sure each column has at least one shelf
    for (i in 1:min(num_shelves, num_cols)) {
        shelves_dist[i] <- 1 
    }

    # update the num_shelves
    num_shelves <- num_shelves - sum(shelves_dist)

    # distribute remaining shelves
    while (num_shelves > 0) {
        col_index <- sample(1:num_cols, 1) 
        shelves_dist[col_index] <- shelves_dist[col_index] + 1
        num_shelves <- num_shelves - 1
    }

    # update the object slots
    object@num_cols <- num_cols
    object@num_shelves <- sum(shelves_dist)
    object@shelves_dist <- shelves_dist

    return(object)
})


############################################
#### A METHOD TO COMBINE IT WITH RECTANGLES
#### How to make this more tidy? It works but not good

# A class for shelves as rectangles
setClass("ShelfRectangle",
    contains = "rectangle",
    slots = list(
    distribution = "ShelfDistribution"
    )
)

# initialize ShelfRectangle
setMethod("initialize", "ShelfRectangle", function(.Object, center, size, orientation = 0, distribution = NULL, ...) {
    .Object <- callNextMethod(.Object, center = center, size = size, orientation = orientation, ...)
    .Object@distribution <- distribution
    return(.Object)
})

# a method to plot shelf distribution using ShelfRectangle
# it should not be a df, should be changed
# define the generic
setGeneric("getShelfCoordinates",
           function(object, ...) {
               standardGeneric("getShelfCoordinates")
           })

setMethod("getShelfCoordinates", "ShelfDistribution", function(object, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5, ...) {
    shelf_coordinates <- data.frame(xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0))
    index <- 1
    for (col in 1:object@num_cols) {
        for (shelf in 1:object@shelves_dist[col]) {
            shelf_center <- c((col - 1) * (shelf_width + aisle_width) + shelf_width / 2, shelf)
            shelf_coordinates[index, ] <- c(shelf_center[1] - shelf_width/2, shelf_center[1] + shelf_width/2, shelf - shelf_length/2, shelf + shelf_length/2)
            index <- index + 1
        }
    }
    return(shelf_coordinates)
})


# example use & plot the shelf distribution
# shelf_dist <- new("ShelfDistribution")
shelf_dist <- generate_dist(shelf_dist)
getShelfCoordinates(shelf_dist, shelf_length = 1, shelf_width = 0.5, aisle_width = 1.5)
print(shelf_dist)

## add circles
