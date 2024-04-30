# Vectors that define the parameters by name. Used in the functions defined here.
# Not exported because not needed anywhere else.
normal_parameters <- c("radius",
                       "slowing_time",
                       "preferred_speed",
                       "reroute",
                       "b_current_direction",
                       "a_current_direction",
                       "blr_current_direction",
                       "b_goal_direction",
                       "a_goal_direction",
                       "b_blocked",
                       "a_blocked",
                       "b_interpersonal",
                       "a_interpersonal",
                       "d_interpersonal",
                       "b_preferred_speed",
                       "a_preferred_speed",
                       "b_leader",
                       "a_leader",
                       "d_leader",
                       "b_buddy",
                       "a_buddy")

log_parameters <- c("randomness",
                    "stop_utility")

utility_parameters <- c(normal_parameters, 
                        log_parameters)

nest_parameters <- c("central", 
                     "non_central", 
                     "acceleration", 
                     "constant_speed", 
                     "deceleration")

#' Draw parameters
#' 
#' Use the mean parameter values and their standard deviations to draw generate
#' parameters to be used in simulation.
#' 
#' @param parameters A named list containing the parameters for a given agent.
#' @param individual_differences Logical denoting whether to use the standard 
#' deviations in the parameter list to create some variation in the parameters.
#' Defaults to `TRUE`.
#' 
#' @return A names list containing the drawn parameters. Standard deviations are
#' ommitted from this list 
#' 
#' @export  
draw_parameters <- function(parameters, 
                            individual_differences = TRUE) {

    # Delete standard deviations from parameters and return if individual 
    # differences are not allowed
    if(!individual_differences) {
        return(parameters[c(utility_parameters, nest_parameters)])
    }

    # If individual differences are allowed, draw random parameters from the
    # distribution specified for the agent
    for(i in normal_parameters) {
        parameters[[i]] <- rnorm(1, 
                                 parameters[[i]], 
                                 parameters[[paste0("sd_", i)]])
    }

    for(i in log_parameters) {
        parameters[[i]] <- rlnorm(1, 
                                  log(parameters[[i]]),
                                  parameters[[paste0("sd_", i)]])        
    }

    return(parameters[c(utility_parameters, nest_parameters)])
}

#' Transform nest association to precision (mu)
#'
#' @param parameters A named list containing the parameters for a given agent
#'
#' @return A named list containing the transformed parameters
#'
#' @export transform_mu
#
# Original function `getmuM`
transform_mu <- function(parameters) {
    parameter_names <- nest_parameters

    for(i in parameter_names) {
        parameters[[i]] <- (1 - parameters[[i]])^(-1)
    }

    return(parameters)
}

#' Exponentiate the parameters
#'
#' @param parameters A named list containing the parameters for a given agent
#'
#' @return A named list containing the transformed parameters
#'
#' @export transform_exponentiate
#
# TO DO:
#  - parameter "bS" should be changed, as an exponentiation with a very high
#    number becomes Inf (see tests)
#
# Original function `toNatural`
transform_exponentiate <- function(parameters) {
    for(i in utility_parameters){
        parameters[[i]] <- exp(parameters[[i]])
    }

    for(i in nest_parameters){
        parameters[[i]] <- pnorm(parameters[[i]])
    }

    return(parameters)

    # Original implementation
    #   gt1p was empty
    #   negp was empty
    #   posp contained `utility_parameters`
    #   pp contained `nest_parameters`
    #
    # p[gt1p] <- exp(p[gt1p]) + 1
    # p[negp] <- -exp(p[negp])
    # p[posp] <- exp(p[posp])
    # p[pp] <- pnorm(p[pp])
    # p[!is.na(p)]
}

#' Take the logarithm of the parameters
#'
#' @param parameters A named list containing the parameters for a given agent
#'
#' @return A named list containing the transformed parameters
#'
#' @export transform_logarithmic
#
# Original function `toReal`
transform_logarithmic <- function(parameters) {
    for(i in utility_parameters){
        parameters[[i]] <- log(parameters[[i]])
    }

    for(i in nest_parameters){
        parameters[[i]] <- qnorm(parameters[[i]])
    }

    return(parameters)

    # Original implementation
    #   gt1p was empty
    #   negp was empty
    #   posp contained `utility_parameters`
    #   pp contained `nest_parameters`
    #
    # p[gt1p] <-  log(p[gt1p] - 1)
    # p[negp] <-  log(-p[negp])
    # p[posp] <-  log(p[posp])
    # p[pp] <- qnorm(p[pp])
    # p[!is.na(p)]
}

