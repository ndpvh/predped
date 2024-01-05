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

#' Separate agent and object
#' 
#' This function extracts agents and objects from a single `state` list.
#' 
#' @param state A list containing the current state of the simulation
#' 
#' @return Two lists containing the objects or the agents
separate_agent_object <- function(state) {
    # Identify the agents from the `state` list
    agents <- find_class("agent", state)

    # Identify the objects and delete the agents from this list
    objects <- find_class("object", state) 
    objects[[objects %in% agents]] <- NULL 

    return(list("agents" = agents, 
                "objects" = objects))
}
