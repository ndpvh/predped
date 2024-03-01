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
