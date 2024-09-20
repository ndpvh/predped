################################################################################
# VALUES
################################################################################

# setwd('/Users/nielsvanhasbroeck/Broncode/predpedgui/build/Qt_6_7_2_for_macOS-Debug');
# Read in the parameters from the database instead of from local csv files. 
# Currently only supported for the mean values, not for the standard deviations.
#
# Importantly, at some point the GUI should be integrated with predped itself or 
# hosted on a server so that the location of the database doesn't depend on the 
# user.
#
# We do two checks here. First, we make sure that the database exists. If not, 
# then we cannot read the connection anyway. Afterwards, we also check whether 
# the required table exists. If either of these conditions is not satisfied, we 
# read in the csv-files that are present in the current project.
database_location <- file.path("..", "predpedgui", "build", "Qt_6_7_2_for_macOS-Debug", "predped.sqlite")
if(file.exists(database_location)) {
    # Create the connection
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = database_location)

    # Check whether the necessary table can be retrieved from the location
    if("PedestrianDefinition" %in% DBI::dbListTables(con)) {
        db_loaded_parameters <- dplyr::tbl(con, "PedestrianDefinition") |>
            dplyr::collect()
    } 
}

if(!exists("db_loaded_parameters")) {
    warning(paste0("Could not read in the parameters from the database. ", 
                   "Reading them in from a csv-file."))
    db_loaded_parameters <- read.csv(file.path("archetypes.csv"))
}

#' Typical Archetypes used for Simulation
#' 
#' @export
params_archetypes <- db_loaded_parameters
params_sigma <- readRDS(file.path("archetypes_sigma.Rds"))
params_bounds <- read.csv(file.path("parameter_bounds.csv"), 
                          row.names = 1)

# Vectors that define the parameters by name. Used in the functions defined here.
# Not exported because not needed anywhere else.
nest_parameters <- c("central", 
                     "non_central", 
                     "acceleration", 
                     "constant_speed", 
                     "deceleration")

utility_parameters <- colnames(params_archetypes)
utility_parameters <- utility_parameters[!(utility_parameters %in% c("name", "color"))]





################################################################################
# FUNCTIONS
################################################################################

#' Draw parameters
#' 
#' Use the mean parameter values and their standard deviations to draw generate
#' parameters to be used in simulation.
#' 
#' @param n Integer denoting the number of parameters to draw. Defaults to `1`.
#' @param mean A named list containing the mean for each of the parameters for 
#' a given agent.
#' @param Sigma Either a covariance matrix that defines the individual differences
#' on each of the parameters (when `transform_covariance == FALSE`), or a matrix
#' containing standard deviations for each of the parameters on its diagonal and
#' correlations between the parameters on its off-diagonal (when 
#' `transform_covariance == TRUE`). Default covariance matrices exist for each
#' of the archetypes in `params_archetypes` and thus changes with the value of 
#' `archetype`.
#' @param archetype String denoting the archetype to be used for the covariance
#' matrix. Ignored if `Sigma` is provided. Defaults to `BaselineEuropean`.
#' @param individual_differences Logical denoting whether to use the standard 
#' deviations in the parameter list to create some variation in the parameters.
#' Defaults to `TRUE`.
#' @param transform_covariance Logical denoting whether to transform `Sigma` to 
#' a proper covariance matrix or not. Defaults to `TRUE`.
#' 
#' @return A names list containing the drawn parameters. Standard deviations are
#' ommitted from this list 
#' 
#' @export  
draw_parameters <- function(n = 1,
                            mean = params_archetypes[params_archetypes$name == archetype,],
                            Sigma = params_sigma[[archetype]],
                            archetype = "BaselineEuropean",
                            individual_differences = TRUE,
                            transform_covariance = TRUE) {

    # Delete standard deviations from parameters and return if individual 
    # differences are not allowed
    if(!individual_differences) {
        return(mean[utility_parameters])
    }

    # Make sure the order of the variables in Sigma is the same as for mean
    mean <- mean[utility_parameters]
    Sigma <- Sigma[utility_parameters, utility_parameters]

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
    mean <- to_unbounded(mean) |>
        t()
    params <- MASS::mvrnorm(n, mean, Sigma)
    params <- to_bounded(params)

    return(params[utility_parameters])
}

#' Transform to real axis
#' 
#' Use the probit (positive, bounded) definition of the parameters and transform
#' them to a real (normal, unbounded) scale.
#'
#' @param parameters A named list containing the parameters for a given agent
#'
#' @return A named list containing the transformed parameters
#'
#' @export
#
# Original function `toReal`
to_unbounded <- function(parameters) {
    for(i in utility_parameters){
        # Transform to 0 - 1 (probit) range
        parameters[[i]] <- (parameters[[i]] - params_bounds[i,1]) / diff(as.numeric(params_bounds[i,]))

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
            parameters[[i]] <- parameters[[i]] + 1e-5
        }

        if(parameters[[i]] == 1) {
            parameters[[i]] <- parameters[[i]] - 1e-5
        }

        # Transform to a value of a normal distribution
        parameters[[i]] <- qnorm(parameters[[i]])
    }

    return(parameters[utility_parameters])
}

#' Transform to positive axis
#' 
#' Use the real (normal, unbounded) definition of the parameters and transform
#' them to a positive (probit, bounded) scale.
#'
#' @param parameters A named list containing the parameters for a given agent
#'
#' @return A named list containing the transformed parameters
#'
#' @export
#
# TO DO:
#  - parameter "bS" should be changed, as an exponentiation with a very high
#    number becomes Inf (see tests)
#
# Original function `toNatural`
to_bounded <- function(parameters) {
    # If the parameters are numeric, transpose them to a matrix
    if(!any(class(parameters) %in% "matrix")) {
        parameters <- t(parameters)
    }

    params <- list()
    for(i in utility_parameters){
        # Transform to the 0 - 1 scale
        params[[i]] <- pnorm(parameters[,i])

        # Transform to the original bounds of the parameters
        params[[i]] <- diff(as.numeric(params_bounds[i,])) * params[[i]] + params_bounds[i, 1]
    }

    return(data.frame(params[utility_parameters]))
}

#' Transform user-inputted matrix to covariance matrix
#' 
#' Use the inputted matrix to construct the covariance matrix for the parameters.
#' 
#' @param X A d x d matrix containing the standard deviations of the parameters
#' on the diagonal and the correlations between parameters off the diagonal.
#' 
#' @return Covariance matrix of the parameters
#' 
#' @export 
to_covariance <- function(X) {
    d <- nrow(X)

    # Compute the covariance matrix in the following way:
    #   - Create two matrices containing all standard deviations of a given 
    #     dimension in their rows or columns.
    #   - Adjust X so that it contains 1's on its diagonal
    #   - Multiply X with these two matrices in an element-wise way, so that each
    #     cell contains the covariance: COV(x, y) = COR(x, y) * SD(x) * SD(y)
    SD <- diag(X) |>
        rep(times = d) |>
        matrix(nrow = d, ncol = d)
    diag(X) <- 1

    return(X * SD * t(SD))
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

#' Plot prior parameter distributions
#' 
#' Use the provided means and covariance matrix to visualize the distribution of
#' parameter values.
#' 
#' @param n Integer denoting the number of parameters to draw. Defaults to `1`.
#' @param mean A named list containing the mean for each of the parameters for 
#' a given agent.
#' @param Sigma Either a covariance matrix that defines the individual differences
#' on each of the parameters (when `transform_covariance == FALSE`), or a matrix
#' containing standard deviations for each of the parameters on its diagonal and
#' correlations between the parameters on its off-diagonal (when 
#' `transform_covariance == TRUE`). Default covariance matrices exist for each
#' of the archetypes in `params_archetypes` and thus changes with the value of 
#' `archetype`.
#' @param archetype String denoting the archetype to be used for the covariance
#' matrix. Ignored if `Sigma` is provided. Defaults to `BaselineEuropean`.
#' @param individual_differences Logical denoting whether to use the standard 
#' deviations in the parameter list to create some variation in the parameters.
#' Defaults to `TRUE`.
#' @param transform_covariance Logical denoting whether to transform `Sigma` to 
#' a proper covariance matrix or not. Defaults to `TRUE`.
#' 
#' @return Printed histogram of the distribution of parameters under the 
#' specifications.
#' 
#' @export
plot_distribution <- function(...) {

    # Simulate several parameters
    parameters <- draw_parameters(...)

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
