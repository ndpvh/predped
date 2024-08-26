#' Distances to Group Centroid
#'
#' Get distances to group centroid for pedestrian `n`
#' 
#' @details Group centroid is calculated using the mean predicted (x & y)
#'          coordinates of pedestrians belonging to the group of pedestrian `n`.
#' @details Returns `NULL` if the pedestrian `n` does not belong to a pedestrian social group.
#'
#' @param p_pred Numeric matrix with shape Nx2 (x & y) containing predicted positions
#' of all pedestrians.
#' @param centers Numeric matrix with shape (33x2) containing (x & y) for each cell centre.
#' @param nped Numeric integer indicating number of people in pedestrian `n`'s social group. 
#'
#' @return Numeric vector `cell_dist` with distances from each cell center to the group centroid. 
#'
#' @export 
get_mean_group_centroid <- function(p_pred, centers, nped) {
   
    # First need to identify whether a pedestrian belongs to a social group
    
    if (nped == 0) {
        return(NULL)    
    }

    # All of the positions of all in-group pedestrians need to be averaged
    # Which represents the group centroid
    centroid <- c(mean(p_pred[,1]), mean(p_pred[,2]))

    # Euclidean distance for pedestrian_i is calculated for each cell
    return(m4ma::dist1_rcpp(centroid, centers))
}

#' Distances to Group Centroid
#'
#' Get distances to group centroid for pedestrian `n`
#'
#' @details Group centroid is calculated using the median predicted (x & y)
#'          coordinates of pedestrians belonging to the group of pedestrian `n`.
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
get_median_group_centroid <- function(agent_idx, p_pred, agent_group, centers) {
   
    # First need to identify whether a pedestrian belongs to a social group
    inGroup <- agent_group[-agent_idx] == agent_group[agent_idx]
    p_pred <- p_pred[inGroup, , drop = FALSE]
    nped <- dim(p_pred)[1]
    
    if (nped == 0) {
        return(NULL)    
    }

    # All of the positions of all in-group pedestrians need to be averaged
    # Which represents the group centroid
    centroid <- c(median(p_pred[,1]), median(p_pred[,2]))

    # Euclidean distance for pedestrian_i is calculated for each cell
    return(m4ma::dist1_rcpp(centroid, centers))
}

#' Group Centroid Utility
#'
#' @details Returns `empty numeric` if the pedestrian `n` does not belong to a pedestrian social group.
#'
#' @param a_gc Numeric scalar power parameter
#' @param b_gc Numeric scalar steepness parameter
#' @param radius Numeric double indicating the radius of pedestrian `n`.
#' @param cell_dist Numeric vector with the distance from the cell to the predicted group centroid.
#' @param stop_utility Numeric double with the disutility of 34th stopping cell.
#' @param nped Numeric integer indicating number of people in pedestrian `n`'s social group. 
#' 
#' @return Numeric vector `centroid_util` with group centroid utility for each cell.
#' @export
#'
gc_utility <- function(a_gc, b_gc, radius, cell_dist, stop_utility, nped) {
    
    if (is.null(cell_dist)) {
        return(numeric(33))
    }

    optimal_distance <- 1.5 * nped * radius
    
    # Calculate centroid 
    centroid_util <- -sapply(cell_dist, 
                                \(x) ifelse(x < optimal_distance, 0, b_gc * (x - optimal_distance)^a_gc))

    if (all(centroid_util < stop_utility)) {
        return(numeric(33))
    }
    
    # return the utility
    return(centroid_util)
}

