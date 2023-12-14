#' Find all objects of a given class
#' 
#' This function is a simple utility function to extract all objects from a given 
#' class from a list containing several of those.
#' 
#' @param class_name A string with the name of the class
#' @param lst The list in which to search for these objects
#' 
#' @return A list containing all instances of the class.
find_class <- function(class_name, lst) {
    return(Filter(function(x) class_name %in% class(x), lst))
}
