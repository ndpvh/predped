#' Transform nest association to precision (mu)  
#' 
#' @param parameters A named list containing the parameters for a given agent
#' 
#' @return A named list containing the transformed parameters
#' 
#' @export 
#' 
#' Original function `getmuM`
transform_mu <- function(parameters) {
    parameter_names <- c("Central", "NonCentral", "acc", "const", "dec")

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
#' @export 
#' 
#' Original function `toNatural`
transform_exponentiate <- function(parameters) {
    utility_parameters <- c("rU",                     # utility randomness
                            "bS",                     # stand still threshold
                            "bWB", "aWB",             # walk beside
                            "bFL", "aFL", "dFL",      # follow the leader
                            "bCA", "bCAlr", "aCA",    # current direction
                            "bBA", "aBA",             # blocked angle
                            "bGA", "aGA",             # goal angle
                            "bPS", "aPS", 
                            "sPref", "sSlow",         # preferred velocity
                            "bID", "aID", "dID")      # interpersonal distance
    nest_parameters <- c("Central", "NonCentral", "acc", "const", "dec")
    
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
#' @export 
#' 
#' Original function `toReal`
transform_logarithmic <- function(parameters) {
    utility_parameters <- c("rU",                     # utility randomness
                            "bS",                     # stand still threshold
                            "bWB", "aWB",             # walk beside
                            "bFL", "aFL", "dFL",      # follow the leader
                            "bCA", "bCAlr", "aCA",    # current direction
                            "bBA", "aBA",             # blocked angle
                            "bGA", "aGA",             # goal angle
                            "bPS", "aPS", 
                            "sPref", "sSlow",         # preferred velocity
                            "bID", "aID", "dID")      # interpersonal distance
    nest_parameters <- c("Central", "NonCentral", "acc", "const", "dec")
    
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

