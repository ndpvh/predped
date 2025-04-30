################################################################################
# VALUES
################################################################################

#' Default Parameter List 
#' 
#' This parameter list contains the default mean parameters under slot 
#' \code{params_archetypes}, their standard deviations under slot 
#' \code{params_sigma}, and their bounds under \code{params_bounds}. These 
#' parameters automatically come with the package.
#' 
#' @details 
#' # Content of the slots
#' 
#' Values under slots \code{params_archetypes} and \code{params_sigma} contain 
#' some variation as imposed through what we call the "archetypes", which for 
#' \code{params_archetypes} can be found in the column \code{name} and for 
#' \code{params_sigma} in the names of this list. These archetypes represent 
#' parameter sets that have been created to display a given type of behavior, 
#' such as rushing to get to the goals ("Rushed") or making very random 
#' moment-to-moment decisions ("DrunkAussie"). These archetypes thus represent 
#' a part of the individual variability that \code{predped} allows.
#' 
#' Another aspect of this variability is controlled by the values under 
#' \code{params_sigma}. For each of the archetypes a covariance matrix is 
#' defined in the list \code{params_sigma} that allows for variation around the 
#' values found in \code{params_archetypes}. However, do not mistake the matrices
#' in \code{params_sigma} to be covariance matrices: Instead, these matrices 
#' have standard deviations on the diagonal and correlations between the 
#' parameters on the off-diagonal, allowing for users to more intuitively set 
#' up these matrices themselves. Under the hood, the covariance matrix COV is 
#' computed through the provided matrix X by defining:
#' 
#' \eqn{SD = diag(X) ,}
#' 
#' and by creating the matrix COR which consists of X with its diagonal turned 
#' to 1. We can then compute the covariance matrix by multplying both matrices:
#' 
#' \eqn{COV = COR * SD * SD^T .}
#' 
#' Importantly, the standard deviation and correlations should be defined between
#' each of the parameters. We furthermore note that the standard deviations can 
#' also be equal to 0, allowing no variation in the selected parameters.
#' 
#' # Parameters
#' 
#' Each of the parameters in \code{params_archetypes} controls an aspect of the 
#' decisions pedestrians make when walking around in an environment, namely:
#' \itemize{
#'     \item{\code{radius}:}{the radius of the agent}
#'     \item{\code{slowing_time}:}{the number of seconds the agent needs to slow
#'                                down when approaching a goal}
#'     \item{\code{preferred_speed}:}{the speed at which the agent is comfortable
#'                                   walking}
#'     \item{\code{randomness}:}{the temperature parameter that controls the 
#'                              overall unpredictability of the nex decision an 
#'                              agent will make. Larger values make movement 
#'                              more deterministic (i.e., strongly determined 
#'                              by the utility functions).}
#'     \item{\code{stop_utility}:}{utility value of stopping instead of moving}
#'     \item{\code{reroute}:}{number of pedestrians that should be in the way for
#'                           an agent to consider rerouting 50\% of the time}
#'     \item{\code{b_current_direction}:}{slope that scales the utility of 
#'                                       continuing waking in the current 
#'                                       direction}
#'     \item{\code{a_current_direction}:}{exponent that determines the power to 
#'                                       which the difference of not walking in 
#'                                       the current direction is taken}
#'     \item{\code{blr_current_direction}:}{scales the preference for walking to  
#'                                         the left or right when heading in a
#'                                         given direction. Done in such a way
#'                                         that \code{b_current_direction} 
#'                                         defines the slope for the left side 
#'                                         and is divided by 
#'                                         \code{blr_current_direction} for the 
#'                                         right side, meaning that the slope 
#'                                         for the right side increases when 
#'                                         \code{blr_current_direction < 1} and 
#'                                         decreases when 
#'                                         \code{blr_current_direction > 1}}
#'     \item{\code{b_goal_direction}:}{slope that scales the utility of heading 
#'                                    in the direction of your current goal}
#'     \item{\code{a_goal_direction}:}{exponent that determines the power to 
#'                                    which the utility of not heading towards 
#'                                    the current goal is taken}
#'     \item{\code{b_blocked}:}{slope that scales the extent to which agents will
#'                             avoid directions that in the long run will lead 
#'                             to blockage (e.g., because of other agents)}
#'     \item{\code{a_blocked}:}{exponent that determines the power of the function
#'                             that determines the utility for avoiding 
#'                             directions that will lead to blockage}     
#'     \item{\code{b_interpersonal}:}{slope that scales the steepness of the 
#'                                   utility for keeping an interpersonal 
#'                                   distance}
#'     \item{\code{a_interpersonal}:}{exponent that determines the power to which
#'                                   the interpersonal distance is taken. Note 
#'                                   that -- in contrast to the other exponents
#'                                   -- the exponent here concerns the exponent
#'                                   of a hyperbolic function, rather than a 
#'                                   power function.} 
#'     \item{\code{d_interpersonal}:}{increment added to \code{b_interpersonal} 
#'                                   when pedestrians close to the agent are of 
#'                                   a different social group, effectively 
#'                                   increasing the interpersonal distance}
#'     \item{\code{b_preferred_speed}:}{slope that scales the effect of trying to
#'                                     walk at your preferred speed} 
#'     \item{\code{a_preferred_speed}:}{exponent that determines the power to 
#'                                     which the difference of not walking at 
#'                                     your preferred speed is taken} 
#'     \item{\code{b_leader}:}{slope that scales the effect of selecting and 
#'                            following in a leader's footsteps}
#'     \item{\code{a_leader}:}{exponent that determines the power of the function
#'                            for the follow-the-leader effect} 
#'     \item{\code{d_leader}:}{increment added to \code{b_interpersonal} when the
#'                            leader is of the same social group as the agent, 
#'                            effectively increasing the tendency for the agent
#'                            to follow this leader} 
#'     \item{\code{b_buddy}:}{slope that scales the effect of selecting and 
#'                            walking besides a buddy} 
#'     \item{\code{b_group_centroid}:}{slope that scales the effect of trying to 
#'                                     maintain a small distance between an 
#'                                     agent and their group members}
#'     \item{\code{a_group_centroid}:}{exponent that determines the power with 
#'                                     which the distance of the agent to their
#'                                     group members is taken}
#'     \item{\code{b_visual_field}:}{slope that scales the effect of maintaining
#'                                   group members within the visual field}
#' } 
#' 
#' @seealso 
#' \code{\link[predped]{predped-class}},
#' \code{\link[predped]{generate_parameters}},
#' \code{\link[predped]{load_parameters}},
#' \code{\link[predped]{utility}}
#' 
#' @rdname params_from_csv
#' 
#' @export
params_from_csv <- list("params_archetypes" = read.csv(file.path("archetypes.csv")),
                        "params_sigma" = readRDS(file.path("archetypes_sigma.Rds")),
                        "params_bounds" = readRDS(file.path("archetypes_bounds.Rds")))

#' Load parameters
#' 
#' Read in the parameters from either a database or from the predped-provided
#' csv-files. Only reads in data from database (a) when a database is provided, 
#' (b) when that database exists and (c) when that database contains the required 
#' table. If either of these conditions is not satisfied, we read in the 
#' csv-files that are present in predped.
#' 
#' @details 
#' Note that reading in from a database is currently only supported for the 
#' mean values or \code{params_archetypes}, not for the standard deviations in 
#' \code{params_sigma} and bounds in \code{params_bounds}.
#' 
#' @param x Character denoting the path to a file containing parameters. 
#' Defaults to \code{NULL}, triggering reading in the csv-files that come with 
#' predped. 
#' @param sep Character denoting the separator in case \code{x} is a delimited 
#' file. Defaults to \code{","}.
#' 
#' @return List of parameter values contained in \code{params_archetypes} (means), 
#' \code{params_sigma} (standard deviations), and \code{params_bounds} (the 
#' bounds of the parameters).
#' 
#' @examples 
#' # Read in the datasets from the predped-provided csv-files
#' parameters <- load_parameters()
#' head(parameters)
#' 
#' @seealso
#' \code{\link[predped]{params_from_csv}}
#' 
#' @rdname load_parameters
#' 
#' @export
#
# TO DO: 
#   - At some point the GUI should be integrated with predped itself or hosted
#     on a server so that the location of the database doesn't depend on the 
#     user.
#   - Test this function extensively, including cases where (e.g.) a list contains
#     other irrelevant information
load_parameters <- function(x = NULL, 
                            sep = ",") {

    # If the provided database is NULL, we can return the default parameters of 
    # predped immediately
    if(is.null(x)) {
        return(params_from_csv)
    }

    
    # Check what type of file you are dealing with. Cases are (a) a database,
    # (b) R-related files, or (c) csv-files
    #
    # Case: Database
    if(grepl(".sqlite", x, fixed = TRUE)) {
        # Create the connection
        con <- DBI::dbConnect(RSQLite::SQLite(), dbname = x)

        # Check whether the necessary table can be retrieved from the location
        if("PedestrianDefinition" %in% DBI::dbListTables(con)) {
            params <- dplyr::tbl(con, "PedestrianDefinition") |>
                dplyr::collect()
        } else {
            # If the table cannot be retrieved, we will just return the 
            # default parameters and throw a message indicating this
            message(paste0("Could not read in the parameters from the database. ", 
                           "Returning default parameters instead."))
            return(params_from_csv)
        }

    # Case: R-files
    } else if(grepl(".Rda", x, fixed = TRUE) | grepl(".Rds", x, fixed = TRUE) | grepl(".RDS", x, fixed = TRUE) | grepl(".RDA", x, fixed = TRUE)) {
        # Use the readRDS function to read in the parameters
        params <- readRDS(x)

    # Case: Other delimited files        
    } else {
        # Use read.table for this purpose
        params <- utils::read.table(x, 
                                    header = TRUE, 
                                    sep = sep,
                                    comment.char = "@")
    }

    # When parameters have been read in, we should check what their type is. 
    # Specifically, we should check whether we only read in the mean values of 
    # the parameters ("params_archetypes") or whether we read in a complete 
    # parameter structure (like for "params_from_csv"). We do this by checking 
    # the names and whether they belong to the expected list. If not, then we 
    # know that we can only change the "params_archetypes" slot of 
    # params_from_csv.
    if(any(names(params) %in% c("params_archetypes", "params_sigma", "params_bounds"))) {
        # Change the slots of params_from_csv that are contained in params 
        # itself. Doing it this way allows for people to only specify one or 
        # more specific parts of the parameters and predped still retaining all
        # the needed information.
        for(i in names(params)) {
            # Only use information that is necessary to contain here
            if(!(i %in% c("params_archetypes", "params_sigma", "params_bounds"))) {
                next
            }

            params_from_csv[[i]] <- params[[i]]
        }

    # Check if all the default column names can be found in the retrieved ones.
    # If so, then we know that we just loaded in the mean parameters, so we can 
    # change the "params_archetypes" slot
    } else if(all(names(params_from_csv[["params_archetypes"]]) %in% names(params))) {
        params_from_csv[["params_archetypes"]] <- params

    # Check whether the names of the parameters are contained within the names
    # of the "params_archetypes" slot. If so, then we know that it is 
    # "params_sigma" that has been read in, and which we can change here
    } else if(any(names(params) %in% params_from_csv[["params_archetypes"]]$name)) {
        # Only retain those archetypes in the list that are also in the 
        # dataframe.
        params <- params[names(params) %in% params_from_csv[["params_archetypes"]]$name]
        params_from_csv[["params_sigma"]] <- params

    # Check whether the rownames of the parameters are contained within the 
    # column names of the mean parameters. This would indicate whether we are 
    # talking about the parameter bounds
    } else if(all(rownames(params) %in% colnames(params_from_csv[["params_archetypes"]]))) {
        params_from_csv[["params_bounds"]] <- params

    # Give a message if things are not clear
    } else {
        message(paste0("Unable to determine which aspect of the parameters has been provided. ", 
                       "Returning default parameters instead."))
    }
    
    return(params_from_csv)
}

# Silent function that extracts the names of the utility_parameters. Is needed
# for the other functions in this file to work. Takes in the `params_archetypes`
utility_parameters <- function(x) {
    nest_parameters <- c("central", 
                         "non_central", 
                         "acceleration", 
                         "constant_speed", 
                         "deceleration")
    
    params <- colnames(x)
    return(params[!(params %in% c("name", "color", "id", "weight"))])
}





################################################################################
# FUNCTIONS
################################################################################

#' Generate parameters
#' 
#' Use the mean parameter values and their standard deviations to generate
#' parameters to be used in simulation.
#' 
#' @details 
#' When \code{individual_differences = FALSE}, generating \code{n} parameters 
#' for a given \code{archetype} comes down to repeating the same parameters 
#' under \code{params_archetypes} \code{n} times.
#' 
#' When \code{individual_differences = TRUE}, we use the following steps to 
#' generate the data. First, we transform the mean values under the name 
#' \code{"params_archetypes"} in the parameter list or provided through the 
#' \code{mean} argument to a value between 0 and 1 by using the bounds under
#' \code{"params_bounds"} in the parameter list or through those provided 
#' through the \code{bounds} argument. Then, we transform this bounded value 
#' to an unbounded value drawn from a normal distribution through the 
#' \code{qnorm} function. This transformation is done through the
#' \code{\link[predped]{to_unbounded}} function.
#' 
#' Once we have unbounded values for the parameters, we transform the provided 
#' matrix under \code{"params_sigma"} in the parameter list or provided through 
#' the \code{Sigma} argument. Note that this transformation is only done 
#' if \code{transform_covariance = TRUE}. This transformation is done through 
#' the \code{\link[predped]{to_covariance}}.
#' 
#' Having both the unbounded means and the covariance matrix for the parameters
#' now allows us to draw random values for these parameters through a 
#' multivariate normal distribution. For this, we rely on the \code{MASS} 
#' package. Once the parameters have been sampled, we transform them back to 
#' their original bounds through the \code{\link[predped]{to_bounded}}
#' function.
#' 
#' @param n Integer denoting the number of parameters to generate. Defaults to 
#' \code{1}.
#' @param filename Character denoting the path to a file containing parameters. 
#' Defaults to \code{NULL}, triggering reading in the csv-files that come with 
#' predped. 
#' @param sep Character denoting the separator in case \code{x} is a delimited 
#' file. Defaults to \code{","}.
#' @param mean Dataframe containing the means for each of the parameters for 
#' a given agent. Defaults to \code{NULL}, triggering reading in the data 
#' instead.
#' @param Sigma Either a covariance matrix that defines the individual differences
#' on each of the parameters (when \code{transform_covariance = FALSE}), or a 
#' matrix containing standard deviations for each of the parameters on its 
#' diagonal and correlations between the parameters on its off-diagonal (when 
#' \code{transform_covariance = TRUE}; see \code{\link[predped]{params_from_csv}}). 
#' Default covariance matrices exist for each of the archetypes in 
#' \code{\link[predped]{params_from_csv}} and thus changes with the value of 
#' \code{archetype}. Defaults to \code{NULL}, triggering reading in the data
#' (but only if \code{individual_differences = TRUE}).
#' @param bounds Named numeric matrix containing the bounds for each of the 
#' parameters. Usually provided in the parameter-list under name 
#' \code{"params_bounds"}. Defaults to \code{NULL}, triggering reading in the 
#' data (but only if \code{individual_differences = TRUE}).
#' @param archetype String denoting the archetype to be used for the covariance
#' matrix. Ignored if \code{Sigma} is provided. Defaults to \code{"BaselineEuropean"}.
#' @param individual_differences Logical denoting whether to use the standard 
#' deviations in the parameter list to create some variation in the parameters.
#' Defaults to \code{TRUE}.
#' @param transform_covariance Logical denoting whether to transform \code{Sigma}
#' to a proper covariance matrix or not. Defaults to \code{TRUE}.
#' 
#' @return Data.frame containing the generated parameter values.
#' 
#' @examples 
#' # Generate multiple instances of the BaselineEuropean
#' parameters <- generate_parameters(5)
#' head(parameters)
#' 
#' # Note that you can turn individual differences off
#' parameters <- generate_parameters(5, individual_differences = FALSE)
#' head(parameters)
#' 
#' @seealso 
#' \code{\link[predped]{load_parameters}},
#' \code{\link[predped]{params_to_csv}},
#' \code{\link[predped]{plot_distribution}},
#' \code{\link[predped]{to_bounded}},
#' \code{\link[predped]{to_covariance}},
#' \code{\link[predped]{to_unbounded}}
#' 
#' @rdname generate_parameters
#' 
#' @export  
generate_parameters <- function(n = 1,
                                filename = NULL,
                                sep = ",",
                                archetype = "BaselineEuropean",
                                mean = NULL,
                                Sigma = NULL,
                                bounds = NULL,
                                individual_differences = TRUE,
                                transform_covariance = TRUE) {

    # If the parameters are already defined, then we can just use those instead 
    # of reading in the complete database. Use-cases are different. The 
    # arguments `mean`, `Sigma`, and `bounds` will typically be provided within 
    # a simulation, while the argument `database` will typically be used 
    # whenever one is trying to visualize the distributions of potential 
    # parameters.
    #
    # Let's check some cases here and fill up the missing parameter specifications
    # with the one's of `archetype` (but only consider those cases if not all 
    # of them are missing, as we can then assume they just want to read in the 
    # parameters through the filename)
    params <- load_parameters(x = filename, sep = sep)
    if(is.null(mean) & is.null(Sigma) & is.null(bounds)) {
        mean <- params[["params_archetypes"]] |>
            dplyr::filter(name %in% archetype)
        Sigma <- params[["params_sigma"]][[archetype]]
        bounds <- params[["params_bounds"]]

    } else {
        if(is.null(mean)) {
            warning(paste0("Mean parameters are NULL. Using those of the ", 
                           archetype, 
                           "instead."))
            mean <- params[["params_archetypes"]] |>
                dplyr::filter(name %in% archetype)
        }
    
        if(is.null(Sigma) & individual_differences) {
            warning(paste0("Sigma parameters are NULL. Using those of the ", 
                           archetype, 
                           "instead."))
            Sigma <- params[["params_sigma"]][[archetype]]
        }

        if(is.null(bounds) & individual_differences) {
            warning(paste0("Bounds of parameters are NULL. Using those of the ", 
                           archetype, 
                           "instead."))
            bounds <- params[["params_bounds"]]
        }
    }   

    # Extract the names of the utility parameters
    u_params <- utility_parameters(mean)

    # Delete standard deviations from parameters and return if individual 
    # differences are not allowed
    if(!individual_differences) {
        return(mean[u_params])
    }

    # Make sure the order of the variables in Sigma is the same as for mean
    mean <- mean[u_params]
    Sigma <- Sigma[u_params, u_params]

    # Check whether the user already provided an actual covariance matrix, or 
    # whether the user input corresponds to a mixed standard deviation - 
    # correlation matrix
    if(transform_covariance) {
        Sigma <- to_covariance(Sigma)
    }

    # If individual differences are allowed, draw random parameters from the
    # distribution specified for the agent. Happens in the following steps
    #
    #   - Transform the parameters to the normal scale
    #   - Draw a random parameter from a multivariate normal with as mean the
    #     transformed parameters and as standard deviation those provided by 
    #     the user.
    #   - Transform the drawn parameters back to their usual scale
    mean <- to_unbounded(mean, bounds) |>
        t()
    params <- MASS::mvrnorm(n, mean, Sigma) |>
        matrix(ncol = length(u_params)) |>
        as.data.frame() |>
        setNames(u_params)
    params <- to_bounded(params, bounds)
    

    return(params[u_params])
}

#' Transform to real axis
#' 
#' Use the probit (positive, bounded) definition of the parameters and transform
#' them to a real (normal, unbounded) scale.
#'
#' @param parameters Dataframe or named list containing values for the 
#' parameters of M4MA.
#' @param bounds Named numeric matrix containing the bounds for the parameters 
#' of M4MA.
#'
#' @return Dataframe or named list containing the transformed parameters
#' 
#' @seealso 
#' \code{\link[predped]{generate_parameters}},
#' \code{\link[predped]{plot_distribution}},
#' \code{\link[predped]{to_bounded}},
#' \code{\link[predped]{to_covariance}}
#' 
#' @rdname to_unbounded
#'
#' @export
#
# Original function `toReal`
to_unbounded <- function(parameters, 
                         bounds) {

    # Extract the utility parameters
    u_params <- utility_parameters(parameters)

    # Loop over them
    for(i in u_params){
        # Transform to 0 - 1 (probit) range
        parameters[[i]] <- (parameters[[i]] - bounds[i,1]) / diff(as.numeric(bounds[i,]))

        # Check whether the parameter indeed falls within this range. If not, 
        # throw an error
        if(parameters[[i]] < 0 | parameters[[i]] > 1) {
            stop(paste0("Parameter ", 
                        i,
                        " does not fall within its bounds."))
        }

        # If the parameters fall exactly on the bounds, we need to change them 
        # by an arbitrarily small number to make sure there are no infinities in 
        # our generated parameters
        if(parameters[[i]] == 0) {
            parameters[[i]] <- parameters[[i]]
        }

        if(parameters[[i]] == 1) {
            parameters[[i]] <- parameters[[i]]
        }

        # Transform to a value of a normal distribution
        parameters[[i]] <- qnorm(parameters[[i]])
    }

    return(parameters[u_params])
}

#' Transform to positive axis
#' 
#' Use the real (normal, unbounded) definition of the parameters and transform
#' them to a positive (probit, bounded) scale.
#'
#' @param parameters Dataframe or named list containing values for the 
#' parameters of M4MA.
#' @param bounds Named numeric matrix containing the bounds for the parameters 
#' of M4MA.
#'
#' @return Dataframe or named list containing the transformed parameters
#' 
#' @seealso 
#' \code{\link[predped]{generate_parameters}},
#' \code{\link[predped]{plot_distribution}},
#' \code{\link[predped]{to_unbounded}},
#' \code{\link[predped]{to_covariance}}
#' 
#' @rdname to_bounded
#'
#' @export
#
# TO DO:
#  - parameter "bS" should be changed, as an exponentiation with a very high
#    number becomes Inf (see tests)
#
# Original function `toNatural`
to_bounded <- function(parameters, 
                       bounds) {

    # If the parameters are numeric, transpose them to a matrix. Not needed 
    # anymore due to check in generate_parameters
    # if(!any(class(parameters) %in% "matrix")) {
    #     parameters <- t(parameters)
    # }

    # Extract the utility parameters
    u_params <- utility_parameters(parameters)

    # Loop over the parameters
    params <- list()
    for(i in u_params){
        # Transform to the 0 - 1 scale
        params[[i]] <- pnorm(parameters[,i])

        # Transform to the original bounds of the parameters
        params[[i]] <- diff(as.numeric(bounds[i,])) * params[[i]] + bounds[i, 1]
    }

    return(data.frame(params[u_params]))
}

#' Transform user-provided matrix to covariance matrix
#' 
#' Use the inputted matrix to construct the covariance matrix for the parameters.
#' 
#' @param X A d x d matrix containing the standard deviations of the parameters
#' on the diagonal and the correlations between parameters off the diagonal.
#' 
#' @return Covariance matrix
#' 
#' @seealso 
#' \code{\link[predped]{generate_parameters}},
#' \code{\link[predped]{plot_distribution}},
#' \code{\link[predped]{to_bounded}},
#' \code{\link[predped]{to_unbounded}}
#' 
#' @rdname to_covariance
#' 
#' @export 
to_covariance <- function(X) {
    d <- nrow(X)

    # Compute the covariance matrix in the following way:
    #   - Create a diagonal matrix containing the standard deviations
    #   - Adjust X so that it contains 1's on its diagonal
    #   - Multiply X with these the diagonal matrices so that two matrices where
    #     COV(x, y) = SD(x) * COR(x, y) * SD(y)
    SD <- diag(X) |>
        diag()
    diag(X) <- 1

    return(SD %*% X %*% SD)
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
    parameter_names <- c("central", 
                         "non_central", 
                         "acceleration", 
                         "constant_speed", 
                         "deceleration")

    for(i in parameter_names) {
        parameters[[i]] <- (1 - parameters[[i]])^(-1)
    }

    return(parameters)
}

#' Plot prior parameter distributions
#' 
#' Wrapper of the \code{\link[predped]{generate_parameters}} function that uses
#' the provided arguments to visualize the distribution of parameter values 
#' under the current specifications. 
#' 
#' @param ... Arguments provided to the \code{\link[predped]{generate_parameters}}. 
#' 
#' @return Plotted histograms of the prior distribution of parameters under the 
#' provided specifications.
#' 
#' @seealso 
#' \code{\link[predped]{generate_parameters}},
#' \code{\link[predped]{load_parameters}},
#' \code{\link[predped]{params_to_csv}},
#' \code{\link[predped]{to_bounded}},
#' \code{\link[predped]{to_covariance}},
#' \code{\link[predped]{to_unbounded}}
#' 
#' @rdname plot_distribution
#' 
#' @export
plot_distribution <- function(...) {

    # Simulate several parameters
    parameters <- generate_parameters(...)

    # Loop over each of the parameters and create histograms of the distribution
    # of these parameters
    plt <- list()
    suppressMessages(for(i in colnames(parameters)) {
        # Transform parameter data to specific plotting data
        plot_data <- parameters[,i] |>
            as.data.frame() |>
            setNames("X")

        # Compute how much variation there is in the parameter
        interval <- diff(range(plot_data$X))

        # Create the general ggplot
        plt[[i]] <- ggplot2::ggplot(data = plot_data,
                                    ggplot2::aes(x = as.numeric(X))) +
            ggplot2::geom_histogram(fill = "gray",
                                    color = "black",
                                    binwidth = ifelse(interval != 0, 
                                                      interval / 16,
                                                      1e-3)) +
            ggplot2::labs(title = i,
                          x = "",
                          y = "Frequency") +
            ggplot2::scale_x_continuous(labels = \(x) sprintf("%.2f", x)) +
            # ggplot2::scale_x_continuous(labels = scales::scientific) +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, 
                                                               hjust = 0))

        # If there is no variation in the parameters, we change the limits so 
        # that it is nicer for the eye.
        if(interval == 0) {
            plt[[i]] <- plt[[i]] +
                ggplot2::scale_x_continuous(limits = plot_data$X[1] + c(-1e-2, 1e-2),
                                            #labels = scales::scientific,
                                            labels = \(x) sprintf("%.2f", x))
        }
    })

    # Bind plots together and return
    return(ggpubr::ggarrange(plotlist = plt, 
                             nrow = ceiling(sqrt(length(plt))),
                             ncol = ceiling(sqrt(length(plt)))))
}
