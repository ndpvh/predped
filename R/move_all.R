#' Unpack all states across agents
#' 
#' @param state List that contains the current state of the full simulation.
#' @param names Vector of characters that contains the names to give to the 
#' combined states.
#' @param positions Matrix of numerics denoting the positions of all agents.
unpack_state <- function(state, names, P_n) {
    return(list(p = unpack_list(state, "p"),
                v = unpack_list(state, "v"),
                a = unpack_list(state, "a"),
                r = unpack_list(state, "r"),
                P = P_n,
                group = unpack_list(state, "group"),
                pMat = unpack_list(state, "pMat"),
                cell = unpack_list(state, "cell")))
}

# Create a singular function for the unpacking for a given part of the list
unpack_list <- function(index){
    # Extract the wanted variable from the list across all agents. Then, 
    # transpose the result and give the result row names
    transposed <- apply(state, 2, function(x) x[[index]]) |>
        t()
    row.names(transposed) <- names
    return(transposed)        
}
