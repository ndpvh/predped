#' Nearest Group Member of Pedestrian `n` in the visual of Pedestrian `n`.
#'
#' Find cells that minimise the absolute difference between heading angle of pedestrian `n` 
#' and angle from cell centre to predicted position of nearest group member (in radians).
#'
#' @details Returns `Null` if pedestrian `n` does not belong to a social group.
#'
#' @param agent_idx Integer scalar indexing the pedestrian in the state.
#' @param agent_group Numeric vector with group membership indices of all pedestrians.
#' @param p_pred Numeric matrix with shape Nx2 (x & y) containing predicted positions
#' of all pedestrians.
#' @param orientation Numeric scalar indicating heading angle pedestrian `n`.
#' @param centers Numeric matrix with shape (33x2) containing (x & y) for each cell centre.
#' @param position Numeric vector indicating position (x & y) pedestrian `n`.
#'
#' @return Numeric vector with absolute difference between heading angle and angle from cell centre
#'         to predicted position of nearest group member of pedestrian `n` (in radians).
#' @export 
#'
get_angles_closest_buddy <- function (agent_idx, agent_group, p_pred, orientation, centers, position) {
    
    # First need to identify whether a pedestrian belongs to a social group
    inGroup <- agent_group[-agent_idx] == agent_group[agent_idx]
    p_pred <- p_pred[-agent_idx]
    p_pred <- p_pred[inGroup, , drop = FALSE]
    nped <- dim(p_pred)[1]
    
    if (nped == 0) {
        return(NULL)    
    }
    
    # Find out which pedestrian will be closest to pedestrian `n` at the next iteration.
    # This means extracting the current_position and calculating the distance 
    # to other pedestrians in the group at the next time step.
    nearest_ped <- m4ma::dist1_rcpp(position, p_pred)
    nearest_ped <- p_pred[which.min(nearest_ped),]
    orientations <- atan2(centers[,2] - positions[2], centers[,1] - position[1])

    # Get angle from cell centers of pedestrian `n`
    # to predicted position of the nearest group member.
    angles <- atan2(nearest_ped[2] - centers[,2], nearest_ped[1] - centers[,1])
    
    # Get relative angle from cell 
    rel_angles <- angles - orientations
    rel_angles <- ifelse(rel_angles < 0, rel_angles + 2*pi, rel_angles)

    # Return the difference between cell centers' orientation angle
    # and angle to position randomly selected group member (in radians).

    return(rel_angles)
}

#' Find any pedestrian belonging to the group of pedestrian `n`,
#' that is in the extended visual field of pedestrian `n`.
#'
#' @details Returns `NULL` if pedestrian `n` does not belong to a group.
#'
#' @param agent_idx Integer scalar indexing the pedestrian in the state.
#' @param agent_group Numeric vector with group membership indices of all pedestrians.
#' @param p_pred Numeric matrix with shape Nx2 (x & y) containing predicted positions
#' of all pedestrians.
#' @param orientation Numeric scalar indicating heading angle pedestrian `n`.
#' @param centers Numeric matrix with shape (33x2) containing (x & y) for each cell centre.
#' @param position Numeric vector indicating position (x & y) pedestrian `n`.
#'
#' @return Numeric vector with absolute difference between heading angle and angle from cell centre
#'         to predicted position of randomly sampled group member (in radians).
#'
#' @export
#'
get_angles_any_buddy <- function(agent_idx, agent_group, p_pred, orientation, centers, position) {
    
    # First need to identify whether a pedestrian belongs to a social group
    inGroup <- agent_group[-agent_idx] == agent_group[agent_idx]
    p_pred <- p_pred[-agent_idx]
    p_pred <- p_pred[inGroup, , drop = FALSE]
    nped <- dim(p_pred)[1]
    
    if (nped == 0) {
        return(NULL)    
    }

    # Get orientations of the cells in for pedestrian `n`.
    orientations <- atan2(centers[,2] - position[2], centers[,1] - position[1])

    # Get angles from cell centers to positions of other pedestrians
    # belonging to the group of pedestrian `n`.
    rel_angles <- lapply(seq_len(nrow(p_pred)),
                     \(i) atan2(p_pred[i, 2] - centers[,2], p_pred[i, 1] - centers[,1]) - orientations) 
    rel_angles <- do.call(cbind, rel_angles)
    
    # Get location of angle on unit circle
    rel_angles <- ifelse(rel_angles < 0, rel_angles + 2*pi, rel_angles)
    
    # Index which angles are maximal
    idx <- numeric(nrow(rel_angles))
    for (i in seq_len(nrow(rel_angles))) {
        max_values <- max(cos(rel_angles[i,]))
        idx[i] <- max_values[1]
    }

    return(rel_angles[, max(idx)])
}


#' Continuous Visual Field Utility
#'
#' @details Returns `empty numeric` if pedestrian `n` does not belong to a pedestrian social group
#'
#' @param b_vf Numeric scalar vertex x-coordinate cosine function.
#' @param rel_angles Numeric vector with relative angle from cell center 
#'                   to predicted position group member of pedestrian `n` (in radians).
#'
#' @return Numeric vector `visual_field_utility` with visual field utilities for each pedestrian `n`.
#'
#' @export
#'
vf_utility_cosine_continuous <- function(b_vf, rel_angles) {
     
     if (is.null(rel_angles)) {
        return(numeric(33))
    }

    # Calculate visual field utility
    visual_field_utility <- -sapply(rel_anlges, 
                                    \(x) b_vf * cos(x))

    # Return the visual field utility
    return(visual_field_utility)
}

#' Continuous Visual Field Utility 90 Degrees
#'
#' @details Returns `empty numeric` if pedestrian `n` does not belong to a pedestrian social group
#'
#' @param b_vf Numeric scalar steepness paramter.
#' @param rel_angles Numeric vector with relative angle from cell center 
#'                   to predicted position group member of pedestrian `n` (in radians).
#'
#' @return Numeric vector `visual_field_utility` with visual field utilities for each pedestrian `n`.
#'
#' @export
#'
vf_utility_sine_continuous <- function(b_vf, rel_angles) {
     
     if (is.null(rel_angles)) {
        return(numeric(33))
    }

    # Calculate visual field utility
    visual_field_utility <- -sapply(rel_anlges, 
                                    \(x) b_vf * abs(sin(x)))

    # Return the visual field utility
    return(visual_field_utility)
}

#' Discrete Visual Field Utility
#'
#' @details Returns `empty numeric` if pedestrian `n` does not belong to a pedestrian social group
#'
#' @param b_vf Numeric scalar steepness parameter.
#' @param rel_angles Numeric vector with relative angle from cell center 
#'                   to predicted position group member of pedestrian `n` (in radians).
#'
#' @return Numeric vector `visual_field_utility` with visual field utilities for each cell of pedestrian `n`.
#'
#' @export
#'
vf_utility_discrete <- function(b_vf, rel_angles) {
     
     if (is.null(rel_angles)) {
        return(numeric(33))
    }

    # Calculate visual field utility
    # If in visual field: disutility = 0
    # If not in visual field: disutility = b*-1
    visual_field_utility <- -sapply(rel_angle, 
                                    \(x) ifelse(x > 3*pi/4 & x < 5*pi/4, b_vf * 1, 0))

    # Return the visual field utility
    return(visual_field_utility)
}
