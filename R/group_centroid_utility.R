###### Necesarry Information to develop utility ######

# Identification that all pedestrians belong to a social group in a given iteration
# Positions of all group members, representing the group centroid in a given iteration
# Euclidean distance of pedestrian_i to group centroid in a given iteration
# Optimal distance of pedestrian_i to group centroid in a given iteration

#' Group Centroid
#'
#' Get distances to group centroid for pedestrian `n`
#'
#' @details Returns `NULL` if the pedestrian `n` does not belong to a pedestrian social group.
#'
#' @param agent_idx Integer scalar indexing the pedestrian in the state.
#' @param p_pred Numeric matrix with shape Nx2 (x & y) containing predicted positions
#' of all pedestrians.
#' @param agent_group Numeric vector with group membership indices of all pedestrians.
#' @param centers Numeric matrix with shape (33x2) containing (x & y) for each cell centre.
#'
#' @return Numeric vector `cell_dist` with distances from each cell center to the group centroid. 
#'
#' @export 
get_group_centroid <- function(agent_idx, p_pred, agent_group, centers) {
   
    # First need to identify whether a pedestrian belongs to a social group
    inGroup <- agent_group[-agent_idx] == agent_group[agent_idx]
    p_pred <- p_pred[inGroup, , drop = FALSE]
    nped <- dim(p_pred)[1]
    
    if (nped == 0) {
        return(NULL)    
    }

    # All of the positions of all in-group pedestrians need to be averaged
    # Which represents the group centroid
    centroid <- c(mean(p_pred[,1]), mean(p_pred[,2]))

    # Euclidean distance for pedestrian_i is calculated for each cell
    return(m4ma::dist1_rcpp(centroid, centers))
}

#' Group Centroid Utility
#'
#' @details Returns `empty numeric` if the pedestrian `n` does not belong to a pedestrian social group.
#'
#' @param a_gc Numeric scalar power parameter
#' @param b_gc Numeric scalar steepness parameter
#' @param optm_d Numeric scalar indicating optimal distance to group centroid.
#' @param cell_dist Matrix with cells as columns, each column containing the distance from the cell
#'                   to the predicted group centroid.
#' 
#' @return Numeric vector `centroid_util` with group centroid utilities for each cell.
#' @export
#'
gc_utility <- function(a_gc, b_gc, optm_d, cell_dist) {
    
    if (is.null(cell_dist)) {
        return(numeric(33))
    }
    
    # Calculate centroid 
    centroid_util <- -sapply(cell_dist, 
                                \(x) ifelse(x < optm_d, 0, b_gc * (x - optm_d)^a_gc))

    # return the utility
    return(centroid_util)
}

