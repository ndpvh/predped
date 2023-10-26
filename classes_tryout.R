# library(float) # For potential conversion to Float32
# library(methods)

# Working example of what a class could be
# make_environment <- setRefClass("Environment",
#                                 fields = list(size = "float32",
#                                               coordinate = "float32"))

# And here with nested classes, which also works
# rectangle <- setRefClass("Rectangle",
#                          fields = list(size = "float32",
#                                        coordinate = "float32"))


# make_environment <- setRefClass("Environment", 
#                                 fields = list(object = "Rectangle"))

# rect <- rectangle(size = c(10, 15) |> fl(), 
#                   coordinate = c(5, 7.5) |> fl())
# make_environment(object = rect)

# Let's try a dispatch of a methods in a reference class
# rectangle <- setRefClass("Rectangle",
#                          fields = list(size = "numeric"),
#                          methods = list(compute_area = function() {
#                                             return( size[1] * size[2] )
#                                         }))
# circle <- setRefClass("Circle",
#                       fields = list(radius = "numeric"),
#                       methods = list(compute_area = function() {
#                                          return( radius * pi )
#                                      }))

# rect1 <- rectangle( size = c(1, 2) )
# circ1 <- circle( radius = 1 )

# rect1$compute_area()
# circ1$compute_area()

