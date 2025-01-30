likelihood_dummy <- agent(center = c(0, 0), radius = 1)
likelihood_dummy <- parameters(likelihood_dummy)
likelihood_dummy[1, ] <- 0

#' Compute the min-log-likelihood
#' 
#' Use data to compute the min-log-likelihood of choosing a given observed cell  
#' given a set of parameters. Should allow the user to estimate the parameters 
#' of the model.
#' 
#' @param data Data.frame containing at least "id", "time", "x", "y", "goal_x",
#' "goal_y", and "goal_id". If it does not have the utility variables yet, these
#' will add them to the data.frame.
#' @param parameters Numeric vector or matrix containing the parameters to be 
#' used. Should be specified in the same order as specified in 
#' \code{"parameter_names"}. If a matrix, each row should contain parameters to 
#' be estimated for each instance of "id" separately.
#' @param parameter_names Character vector containing the parameters that you 
#' want to estimate. Defaults to all parameters defined in
#' \code{\link[predped]{params_from_csv}}. Whenever not all parameters are used,
#' the excluded parameters are assumed to have a value of 0.
#' @param ... Additional arguments passed on to \code{\link[predped]{add_motion_variables}}.
#' In a typical estimation situation, these motion variables should already be 
#' in \code{data}.
#' 
#' @return Min-log-likelihood per person in the dataset.
#' 
#' @export 
mll <- function(data, 
                parameters,
                parameter_names = colnames(likelihood_dummy),
                ...) {
    
    browser()

    # Check whether the utility variables are in there. Just checked for one and 
    # assumed that the others are there as well
    if(!("ps_speed" %in% colnames(data))) {
        data <- add_motion_variables(data, ...)
    }

    # For each agent, loop over the unique participant id's 
    ids <- unique(data$id)
    MLL <- sapply(seq_along(ids), 
                  function(i) {
                      # Select the data for which the utilities should be computed
                      selection <- data[data$id == ids[i], ]
  
                      # Adjust the parameters to those that were provided
                      likelihood_dummy[, parameter_names] <- parameters[i, ]
  
                      # Get the utilities for each cell based on the provided 
                      # results
                      browser()
                      V <- utility(selection, likelihood_dummy)
                  })


}