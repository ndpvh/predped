# Information about the alphahull algorithm to construct a polygon around the space
# Some note on how the alphahull package works
# But, first lets install the package.

install.packages("alphahull")
library("alphahull")

# Random sample in the unit square
x <- matrix(runif(100), nc = 2)
# Value of alpha
alpha <- 0.2
# Alpha-convex hull
ahull.obj <- ahull(x, alpha = alpha)
plot(ahull.obj)

# Use the random shelves generator to get shelves_coordinates
# Extract the points of all rectangles and force them into one large matrix
obj <- unlist(shelves_coordinates@rectangles)
ob <- lapply(obj, \(x) x@points)
ob <- do.call(rbind, ob)
head(ob)



alpha <- 0.5
ahull.obj <- ahull(x = ob, alpha = alpha) # This is where the issue arises. 
# There is a function that is used from another package `tri.mesh`
# The default is that is throws an error when there are duplicate values
# Examining `tri.mesh` revealed to me that there is an option to remove duplicates
# Rather than throwing an errror message. 
# This would require an awful lot of coding to do this one thing. 

# Investigate tri.mesh
install.packages("tripack")
library("tripack")
tri_test <- tri.mesh(x = ob[,1], y = ob[,2], duplicate = "remove")
# When duplicate set to remove the error disappears and the triangulation is possible

