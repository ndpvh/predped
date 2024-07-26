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
#' @param orientation Numeric indicating the angle of the current pedestrian.
#' @param current_pos Numeric matrix with shape (1x2) indicating the current pedestrian position.
#'
#' @return  List: 
#'  \describe{
#'   \item{cell_dist}{Numeric matrix with distances 
#'     from each cell centre (columns) to group centoid.}
#'   \item{ang_diff}{Numeric matrix with angular heading difference
#'     between current pedestrian orientation and the cell orientation.}
#'
#'  }
#'
#' @export 
get_group_centroid <- function(agent_idx, p_pred, agent_group, centers, orientation, current_pos) {
   
    # First need to identify whether a pedestrian belongs to a social group
    inGroup <- agent_group[-agent_idx] == agent_group[agent_idx]
    p_pred <- p_pred[inGroup, , drop = FALSE]
    nped <- dim(p_pred)[1]
    
    if (nped == 0) {
        return(NULL)    
    }

    if (nped > 1) {
    # All of the positions of all in-group pedestrians need to be averaged
    # Which represents the group centroid
    centroid <- c(mean(p_pred[,1]), mean(p_pred[,2]))

    # Euclidean distance for pedestrian_i is calculated for each cell
    cell_names <- names(centers)
    cell_dist <- matrix(m4ma::dist1_rcpp(centroid, centers), ncol = 33)
    colnames(cell_dist) <- cell_names

    # Anglular Heading Difference of Pedestrian_i 
    current_orient <- orientation
    current_pos <- current_pos
    ang_diff <- matrix(m4ma::minAngle_rcpp(current_orient, m4ma::angle2_rcpp(current_pos, centers)), ncol = 33)
    colnames(ang_diff) <- cell_names

    # List containing matrices of Euclidean distance & Heading difference
    dists_angles <- list(cell_dist, ang_diff)

    }
    return(dists_angles)
}

#' Group Centroid Utility
#'
#' @details Returns `empty numeric` if the pedestrian `n` does not belong to a pedestrian social group.
#'
#' @param a_gc Numeric scalar power parameter
#' @param b_gc Numeric scalar steepness parameter
#' @param d_gc Numeric scalar steepness penalty parameter
#' @param gc List of two numeric matrices:
#'  \describe{
#'      \item{cell_dist}{Matrix with cells as columns, each column containing the distance from the cell
#'                   to the predicted group centroid.}
#'      \item{ang_diff}{Matrix with the cells as colums, each column containing the angular difference
#'                  in orientation between the cell and current orientation of pedestrian `n`}
#' }
#' 
#' @return Numeric vector with group centroid utilities for each cell.
#' @export
#'
gc_utility <- function(a_gc, b_gc, d_gc, gc) {
    
    if (is.null(gc)) {
        return(numeric(33))
    }

    # Weighted b that takes the angular difference into account
    # Normal b for cells that minimise the angular difference
    b <- ifelse(gc$ang_diff < 5, b_gc, b_gc + d_gc)
    
    # 
    centroid_util <- -sapply(1:length(b), function(i) {
                            b[i] * (gc$cell_dist[, i] - 0.45)^a_gc
    })

    # return the utility
    return(centroid_util)
}

