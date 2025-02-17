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
#' @param transform Logical denoting whether to transform the provided parameters
#' from the real axis to the bounded scales imposed on the parameters within 
#' \code{predped}. Defaults to \code{TRUE}.
#' @param bounds Matrix containing the lower and upper bounds of the parameters
#' in its first and second column respectively. Additionally, rownames should 
#' denote for which parameter a certain pair represents the bounds. Only used 
#' when \code{transform = TRUE}. Defaults to the default bounds of \code{predped}.
#' @param ... Additional arguments passed on to \code{\link[predped]{add_motion_variables}}.
#' In a typical estimation situation, these motion variables should already be 
#' in \code{data}.
#' 
#' @return Min-log-likelihood per person in the dataset.
#' 
#' @export 
mll <- function(data, 
                parameters,
                parameter_names = colnames(params_from_csv[["params_archetypes"]])[-c(1, 2)],
                transform = TRUE,
                bounds = params_from_csv[["params_bounds"]],
                cpp = TRUE,
                ...) {

    # Check whether the utility variables are in there. Just checked for one and 
    # assumed that the others are there as well
    if(!("ps_speed" %in% colnames(data))) {
        data <- add_motion_variables(data, ...)

        # Delete those instances in which a person is not moving around
        data <- data[!is.na(data$ps_speed), ]
    }

    # If transform is TRUE, then we need to transform the parameters from the 
    # real scale to the bounded scale.
    if(transform) {
        parameters <- to_bounded(parameters, bounds)
    }

    # Retrieve each person's identifier
    ids <- unique(data$id)

    # Check whether the provided parameters conform to data.frame format. If not, 
    # transform.
    #
    # Two cases: Either parameters is a vector or a matrix (first case), or 
    # parameters is a dataframe of insufficient length. Ensure they fit the bill
    # or the Rcpp code will throw errors
    if(is.null(dim(parameters)) | is.matrix(parameters)) {
        parameters <- matrix(parameters, 
                             nrow = length(ids),
                             ncol = length(parameters),
                             byrow = TRUE) |>
            as.data.frame() |>
            setNames(parameter_names)

    } else if(is.data.frame(parameters) & nrow(parameters) < length(ids)) {
        idx <- rep_len(1:nrow(parameters), length(ids))
        parameters <- parameters[idx, ]
    }

    if(cpp) {
        return(mll_rcpp(data, 
                        parameters, 
                        parameter_names = names(parameters), 
                        ids = ids))
    }

    # For each agent, loop over the unique participant id's 
    MLL <- sapply(seq_along(ids), 
                  function(i) {
                      # Select the data for which the utilities should be computed
                      selection <- data[data$id == ids[i], ]
  
                      # Get the utilities for each cell based on the provided 
                      # results
                      L <- sapply(1:nrow(selection), 
                                  function(j) {
                                      V <- utility(selection[j, ], parameters[i, ], cpp = FALSE)

                                      V <- V - max(V)
                                      exp_V <- exp(V)
                                      return(exp_V[selection$cell[j] + 1] / sum(exp_V))
                                  })
                      
                      # Convert likelihoods to min-log-likelihood. 1 was added
                      # to each likelihood to ensure that 0 probability will 
                      # not lead to -Inf min-log-likelihood.
                      return(-sum(log(1 + L)))
                  })

    names(MLL) <- ids 
    return(MLL)
}