## Generate a Random Distribution of Shelves 
# Class for Shelf Distribution

# set a class for shelf distribution
ShelfDistribution <- setClass("ShelfDistribution",
                               slots = list(num_cols = "numeric",
                                            num_shelves = "numeric",
                                            shelves_dist = "numeric"))

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

# an example for ShelfDistribution Class
shelf_dist <- new("ShelfDistribution", num_cols = num_cols, num_shelves = num_shelves)
shelf_dist <- generate_dist(shelf_dist)
print(shelf_dist)
