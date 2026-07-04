# TO DO:
#   - I changed convention in this file due to the nestedness of the lists. 
#     consider changing the convention for all files or for this file

#' Benchmark the package
#' 
#' Function that allows users to benchmark predped on their own device. Times are 
#' measured through repeated assessments with \code{Sys.time()}, providing us 
#' with a measure of the user-based time for the functions. Importantly, this is
#' different from pure execution time of the function, which may be less than 
#' the user time. 
#' 
#' @param x Character or character vector containing the names of the functions 
#' to include in the benchmark. Functions should be part of the package. 
#' Defaults to \code{NULL}, denoting that all functions will be tested. 
#' @param iterations Integer denoting the number of times to repeat each 
#' function's benchmark case. The higher this number, the longer the benchmark 
#' will take but also the more accurate the distribution of times will be per 
#' function. Defaults to \code{100}.
#' @param summarize Logical denoting whether to summarize the results of the 
#' benchmark. If \code{TRUE}, the function will output an HTML file containing 
#' the summarized results of the benchmark (mean, standard deviation, and 
#' 2.5% and 97.5% quantiles of the times in seconds). If \code{FALSE}, the output
#' will be a list of lists containing the raw execution times per function and 
#' per benchmark case for the function. Defaults to \code{TRUE}.
#' @param digits Integer denoting the decimal points to which to round to. 
#' Ignored if \code{summarize = FALSE}. Defaults to \code{2}.
#' @param progress Logical denoting whether to add a progress bar for the 
#' benchmarks. Defaults to \code{TRUE}.
#' 
#' @return Either a list of lists containing the raw execution times per 
#' benchmark case per function (if \code{summarize = FALSE}) or an HTML file 
#' summarizing the results of the benchmark (if \code{summarize = TRUE})
#' 
#' @examples
#' \dontrun{
#' # Run benchmark for in_object with 100 iterations
#' benchmark(
#'     x = "in_object",
#'     iterations = 100
#' )
#' 
#' # Run benchmark for all functions with 10 iterations
#' benchmark(
#'    iterations = 10
#' )
#' }
#' 
#' @concept helper
#' 
#' @export 
benchmark <- function(x = NULL, 
                      iterations = 100,
                      summarize = TRUE,
                      digits = 2,
                      progress = TRUE) {

    # Load all tests used in the benchmark
    benchmark_test <- load_benchmark()
    specs <- load_benchmark_specifications()

    caption <- specs$caption 
    hierarchy <- specs$hierarchy

    # Define the functions to include in the benchmark. If x is NULL, then we 
    # include all functions in the benchmark
    if(is.null(x)) {
        x <- names(benchmark_test)
    }

    # Check whether the provided functions have a benchmark case. If not, then 
    # we have to provide a warning and select only those functions that do
    idx <- x %in% names(benchmark_test)
    if(!all(idx)) {
        warning("Not all functions have a benchmark case. Only performing benchmarks for those that do.")
        x <- x[idx]
    }

    # Perform the benchmark and print progress if asked for
    if(progress) {
        cat("\n")
        progress_bar(0, length(x))
    }

    results <- lapply(seq_along(x), 
                      function(i) {
                          # Select all cases for this function
                          fx_test <- benchmark_test[[x[i]]]

                          # Perform benchmark per case
                          case_result <- lapply(names(fx_test), 
                                                function(case) {
                                                    return(test_times(fx_test[[case]], 
                                                                      iterations = iterations))
                                                })
                          names(case_result) <- names(fx_test)

                          # If progress is asked for, adjust
                          if(progress) {
                              progress_bar(i, length(x))
                          }

                          return(case_result)
                      })
    names(results) <- x

    if(progress) {
        cat("\n")
    }

    # Return result like this if asked for raw results
    if(!summarize) {
        return(results)
    }

    # Otherwise, create an HTML file that summarizes the results. Start with the 
    # preamble.
    cat(paste0("---\n",
               "title: \"Benchmarks for predped\"\n",
               "author: \"Niels Vanhasbroeck\"\n",
               "date: '`r Sys.Date()`'\n",
               "output: html_document\n",
               "---\n\n",
               "\`\`\`{r setup, include=FALSE}\n",
               "knitr::opts_chunk$set(echo = TRUE)\n",
               "\`\`\`\n\n",
               "\`\`\`{css, echo=FALSE}\n",
               ".table caption {font-style: italic; font-size: 25px}\n",
               "\`\`\`\n\n"), 
        file = "benchmark.Rmd")

    # Loop over all R-files that make up predped
    for(i in names(hierarchy)) {
        # Check whether there are any functions tested for that specific file. 
        # If so, we will add them to the Rmd file.
        if(any(names(results) %in% hierarchy[[i]])) {
            # Add a title for this file
            cat(paste0("## ", i, "\n\n"), 
                file = "benchmark.Rmd",
                append = TRUE)

            # Loop over the functions that have been benchmarked
            benchmarked <- results[names(results) %in% hierarchy[[i]]]
            for(j in seq_along(benchmarked)) {
                # Add a title for this function
                fx <- names(benchmarked)[j]
                cat(paste0("### ", fx, "\n\n"),
                    file = "benchmark.Rmd", 
                    append = TRUE)

                # Create the table for this function
                tbl <- knitr_table(benchmarked[[fx]],
                                   caption = caption[[fx]],
                                   digits = digits)

                cat(paste0(tbl, "\n\n"), 
                    file = "benchmark.Rmd",
                    append = TRUE)
            }
            
        }
    }

    # Render the markdown and provide a message telling the user what to look for.
    suppressMessages(capture.output(rmarkdown::render("benchmark.Rmd")))
    message("Results have been saved to 'benchmark.html'.")

    # Remove the Rmd
    file.remove("benchmark.Rmd")

    return(NULL)
}

# Function that times the functions user-time
#
# @param fx Function that takes in no arguments. In our case a benchmark case 
# function defining what to do for the benchmark.
# @param iterations Integer denoting the number of times to run the benchmark 
# case. Defaults to \code{100}.
# 
# @return Integer vector of seconds it took for the function to execute at each
# iteration
test_times <- function(fx, 
                       iterations = 100,
                       variables = list()) {

    # Allocate memory to a vector and loop over each of the iterations
    times <- numeric(iterations)
    for(i in seq_along(times)) {
        # Get start time
        start_time <- Sys.time()

        # Perform function and capture result
        suppressMessages(fx())

        # Get stop time
        stop_time <- Sys.time()

        # Compare both and save in the vector
        times[i] <- stop_time - start_time
    }

    return(times)
}

# Create a progress bar for the benchmarks
#
# @param iteration The iteration number you're currently on
# @param max The maximal iteration number you can be on
# @param width The number of vertical bars that make up the progress bar
#
# @return NULL
progress_bar <- function(iteration,
                         max,
                         width = 20) {
    
    n_bars <- round(width * iteration / max)
    cat(
        paste0(
            "\rRunning benchmark: |", 
            paste(rep("|", n_bars), collapse = ""), 
            paste(rep(" ", width - n_bars), collapse = ""), 
            "| ",
            round(100 * iteration / max), 
            "%"
        )
    )

    return(NULL)
}





################################################################################
# UTILITY FUNCTIONS FOR CREATING THE HTML

# Create table
#
# Use the input of a function list to create a table as defined through 
# knitr.
#
# @param x List containing the entries of the benchmark
# @param caption Caption to include in the table
# @param digits Integer denoting the number to which to round the results
#
# @return String containing the table as defined by knitr
knitr_table <- function(x, 
                        caption = "",
                        digits = 2) {

    if(!requireNamespace("knitr") | !requireNamespace("kableExtra")) {
        stop(
            paste(
                "The `knitr` and `kableExtra` packages are required to benchmark `predped`.",
                "Please install."
            )
        )
    }

    rnd <- function(y) {
        return(format(y, scientific = TRUE, digits = digits))
    }

    # Define the number of levels for the table
    levels <- strsplit(names(x)[1], " | ", fixed = TRUE)[[1]]

    # Create different cases:
    #   - No variable was manipulated
    #   - 1 or more variables were manipulated
    n <- length(levels)
    if(n == 1 & levels[1] == "") {
        x <- x[[1]]

        tbl <- cbind(M = rnd(mean(x)),
                     SD = rnd(sd(x)),
                     Q025 = rnd(quantile(x, prob = 0.025)),
                     Q975 = rnd(quantile(x, prob = 0.975)))
        rownames(tbl) <- NULL

        tbl <- knitr::kable(tbl,
                            format = "html",
                            align = "cccc",
                            caption = paste(
                                caption,
                                "Table displays mean (M), standard deviation (SD),",
                                "and 2.5\\% and 97.5\\% quantiles (Q) of the",
                                "execution times as measured in seconds." 
                            ),
                            longtable = FALSE,
                            booktabs = TRUE,
                            escape = FALSE,
                            position = "h")

    } else if(n == 1) {
        levels <- names(x)

        tbl <- matrix(" ", nrow = 1, ncol = length(levels))
        for(i in seq_along(levels)) {
            tbl[, i] <- paste0(rnd(mean(x[[levels[i]]])), 
                               "  [",
                               rnd(quantile(x[[levels[i]]], prob = 0.025)),
                               ", ",
                               rnd(quantile(x[[levels[i]]], prob = 0.975)),
                               "]")
        }
        colnames(tbl) <- levels
        rownames(tbl) <- NULL

        tbl <- knitr::kable(tbl,
                            format = "html",
                            align = paste(rep("c", ncol(tbl)), collapse = ""),
                            caption = paste(
                                caption,
                                "Table displays mean and 2.5\\% and 97.5\\%",
                                "quantiles of the execution times as measured",
                                "in seconds." 
                            ), 
                            longtable = FALSE,
                            booktabs = TRUE,
                            escape = FALSE,
                            position = "h")

        
    } else if(n > 1) {
        # Get all levels in a data.frame
        levels <- lapply(names(x),
                         \(y) matrix(strsplit(y, " | ", fixed = TRUE)[[1]], nrow = 1))
        levels <- do.call("rbind", levels)

        # Get unique levels for all parts
        levels <- lapply(seq_len(ncol(levels)),
                         \(i) unique(levels[, i]))

        # Create a matrix of the correct size
        tbl <- matrix(" ", 
                      nrow = prod(sapply(levels[2:length(levels)], length)), 
                      ncol = length(levels[[1]]) + length(levels) - 1)

        # Fill first columns of the matrix with info on the levels that appear 
        # in the rows
        by <- nrow(tbl)
        for(i in seq_len(length(levels) - 1)) {
            # Get info on the level of choice here
            info <- levels[[i + 1]]

            # Define the indices at which we should put the info
            by <- by / length(info)
            idx <- seq(1, nrow(tbl), by = by)
            
            # Add the info the the table
            tbl[idx, i] <- info
        }

        # Double loop: Each of the rows and each of the columns
        rows <- expand.grid(levels[2:length(levels)], stringsAsFactors = FALSE)
        for(i in 1:nrow(rows)) {
            for(j in seq_along(levels[[1]])) {
                # Find out where the info for this particular cell is held
                idx <- sapply(c(levels[[1]][j], rows[i, ]),
                              \(y) grepl(y, names(x), fixed = TRUE))
                idx <- which(sapply(1:nrow(idx), \(k) all(idx[k, ])))

                if(length(idx) > 1) {
                    browser()
                }

                # Add that info to the table
                tbl[i, j + length(levels) - 1] <- paste0(rnd(mean(x[[idx]])), 
                                                         "  [",
                                                         rnd(quantile(x[[idx]], prob = 0.025)),
                                                         ", ",
                                                         rnd(quantile(x[[idx]], prob = 0.975)),
                                                         "]")
            }
        }
        colnames(tbl) <- c(rep(" ", length(levels) - 1), levels[[1]])
        rownames(tbl) <- NULL

        tbl <- knitr::kable(tbl,
                            format = "html",
                            align = paste(c(rep("l", length(levels) - 1), 
                                            rep("c", length(levels[[1]]))), 
                                          collapse = ""),
                            caption = paste(
                                caption,
                                "Table displays mean and 2.5\\% and 97.5\\%",
                                "quantiles of the execution times as measured",
                                "in seconds." 
                            ), 
                            longtable = FALSE,
                            booktabs = TRUE,
                            escape = FALSE,
                            position = "h")

    }

    tbl <- kableExtra::kable_styling(tbl,
                                     full_width = TRUE, 
                                     html_font = "Cambria",
                                     font_size = 20,
                                     position = "left",
                                     bootstrap_options = c("striped", "hover"))

    return(tbl)
}



################################################################################
# DATA RELEVANT TO THE BENCHMARKS

# Load all data so that it's available to the benchmark functions. Would've 
# loved to see this done another way, but couldn't immediately find one that 
# would work with changing paths in tests vs internal functions.
load(file.path("data", "supermarket.rda"))
load(file.path("data", "benchmark_few_edges.rda"))
load(file.path("data", "benchmark_many_edges.rda"))
load(file.path("data", "benchmark_few_edges_uneval.rda"))
load(file.path("data", "benchmark_many_edges_uneval.rda"))
load(file.path("data", "benchmark_data.rda"))
load(file.path("data", "benchmark_inx.rda"))
load(file.path("data", "benchmark_inx_data.rda"))
load(file.path("data", "benchmark_trace.rda"))
load(file.path("data", "benchmark_params_bounded.rda"))
load(file.path("data", "benchmark_params_real.rda"))

#' Default supermarket
#' 
#' This variable contains the default supermarket used in the benchmarks and 
#' and tests, as well as some substantive studies. The supermarket is coded as 
#' an instance of the \code{\link[predped]{background-class}} and is based on 
#' the blueprints of an Albert Heijn supermarket based in Amsterdam.
#' 
#' @format Instance of the \code{\link[predped]{background-class}}
#' 
#' @export
"supermarket"

#' Edges of an environment (\code{many_nodes = FALSE})
#' 
#' This variable contains the result of running \code{compute_edges} in a 
#' particular environment while setting the option \code{many_nodes = FALSE}.
#' It mainly serves as a variable used to benchmark some of the functions related
#' to routing.
#' 
#' @format Named list with three slots:
#' \describe{
#'   \item{\code{"nodes"}}{data.frame with 3 columns containing the identifier 
#'                         of the node along with its coordinates.}
#'   \item{\code{"edges"}}{data.frame with 3 columns denoting the identifier of 
#'                         the starting and end node of the edge, as well as the 
#'                         distance between them}
#'   \item{\code{"edges_with_coords"}}{data.frame containing the same information 
#'                                     as in \code{"edges"}, but now accompanied
#'                                     with the coordinates of each node in a 
#'                                     pair.}
#'}
#' 
#' @export
"benchmark_few_edges"

#' Edges of an environment (\code{many_nodes = TRUE})
#' 
#' This variable contains the result of running \code{compute_edges} in a 
#' particular environment while setting the option \code{many_nodes = TRUE}.
#' It mainly serves as a variable used to benchmark some of the functions related
#' to routing.
#' 
#' @format Named list with three slots:
#' \describe{
#'   \item{\code{"nodes"}}{data.frame with 3 columns containing the identifier 
#'                         of the node along with its coordinates.}
#'   \item{\code{"edges"}}{data.frame with 3 columns denoting the identifier of 
#'                         the starting and end node of the edge, as well as the 
#'                         distance between them}
#'   \item{\code{"edges_with_coords"}}{data.frame containing the same information 
#'                                     as in \code{"edges"}, but now accompanied
#'                                     with the coordinates of each node in a 
#'                                     pair.}
#'}
#' 
#' @export
"benchmark_many_edges"

#' Raw edges of an environment (\code{many_nodes = FALSE})
#' 
#' This variable contains the result of running \code{create_edges} in a 
#' particular environment while setting the option \code{many_nodes = FALSE} and 
#' without evaluating which edges cannot serve as navigation lines due to objects
#' being in the way. It mainly serves as a variable used to benchmark some of the 
#' functions related to routing.
#' 
#' @format Named list with three slots:
#' \describe{
#'   \item{\code{"nodes"}}{data.frame with 3 columns containing the identifier 
#'                         of the node along with its coordinates.}
#'   \item{\code{"edges"}}{data.frame with 3 columns denoting the identifier of 
#'                         the starting and end node of the edge, as well as the 
#'                         distance between them}
#'   \item{\code{"edges_with_coords"}}{data.frame containing the same information 
#'                                     as in \code{"edges"}, but now accompanied
#'                                     with the coordinates of each node in a 
#'                                     pair.}
#'}
#' 
#' @export
"benchmark_few_edges_uneval"

#' Raw edges of an environment (\code{many_nodes = TRUE})
#' 
#' This variable contains the result of running \code{create_edges} in a 
#' particular environment while setting the option \code{many_nodes = TRUE} and 
#' without evaluating which edges cannot serve as navigation lines due to objects
#' being in the way. It mainly serves as a variable used to benchmark some of the 
#' functions related to routing.
#' 
#' @format Named list with three slots:
#' \describe{
#'   \item{\code{"nodes"}}{data.frame with 3 columns containing the identifier 
#'                         of the node along with its coordinates.}
#'   \item{\code{"edges"}}{data.frame with 3 columns denoting the identifier of 
#'                         the starting and end node of the edge, as well as the 
#'                         distance between them}
#'   \item{\code{"edges_with_coords"}}{data.frame containing the same information 
#'                                     as in \code{"edges"}, but now accompanied
#'                                     with the coordinates of each node in a 
#'                                     pair.}
#'}
#' 
#' @export
"benchmark_many_edges_uneval"

#' Benchmark data
#' 
#' This data.frame contains data of a long-corridor type of simulation, letting 
#' several people walk from one end of a seemingly infinite corridor to the other 
#' side. This is used to benchmark and evaluate some of the functions related 
#' to data handling, as well as the likelihood function.
#' 
#' @format A data frame with 1000 rows and 30 variables:
#' \describe{
#'   \item{\code{iteration}}{Integer denoting the iteration number}
#'   \item{\code{time}}{Numeric denoting the time of the measurement}
#'   \item{\code{id}}{Character denoting the identifier of an agent}
#'   \item{\code{x}, \code{y}}{Numeric denoting the position of the agent}
#'   \item{\code{speed}}{Numeric denoting the speed of the agent}
#'   \item{\code{orientation}}{Numeric denoting the orientation of the agent}
#'   \item{\code{cell}}{Integer denoting the cell the agent moved to on this 
#'                      iteration}
#'   \item{\code{group}}{Numeric denoting group the agent belongs to}
#'   \item{\code{status}}{Character denoting what the agent was doing on this 
#'                        iteration}
#'   \item{\code{goal_id}}{Character denoting the identifier of the agent's 
#'                         current goal}
#'   \item{\code{goal_x}, \code{goal_y}}{Numeric denoting the position of the
#'                                       agent's current goal}
#'   \item{\code{radius}}{Numeric denoting agent's size}
#'   \item{\code{agent_idx}}{Integer denoting the index of the agent in the 
#'                           shared specification list (utility-related)}
#'   \item{\code{check}}{List containing a logical matrix denoting the locations 
#'                       the agent can and cannot move to (utility-related)}
#'   \item{\code{ps_speed}, \code{ps_distance}}{Numeric denoting the speed of 
#'                                              the agent and the distance from 
#'                                              the current goal, used to 
#'                                              evaluate the preferred speed 
#'                                              utility function (utility-related)}
#'   \item{\code{gd_angle}}{List containing a numeric matrix of size 
#'                          \eqn{1 \times 11}} containing the relative angles to 
#'                          the goals for each cone the agent potentially may 
#'                          have moved to
#'   \item{\code{id_distance}, \code{id_check}, \code{id_ingroup}}{
#'         List containing a numeric matrix of size \eqn{A \times 33} containing
#'         the distance to other agents from each cell, a logical matrix of size
#'         \eqn{11 \times 3} containing an update of cells the agent can and 
#'         cannot move to when taking agents into account, and a logical vector
#'         denoting whether the other agents belong to the current agents ingroup.
#'         This information is used to evaluate the interpersonal distance 
#'         utility function (utility-related).}
#'   \item{\code{ba_angle}, \code{ba_cones}}{}
#'   \item{\code{fl_leaders}, \code{wb_buddies}}{List containing the leaders and
#'         buddies needed to evaluate the follow-the-leader and walk-beside 
#'         utility functions (utility-related).}
#'   \item{\code{gc_distance}, \code{gc_radius}, \code{gc_nped}}{List containing 
#'         an integer denoting the number of group members, a numeric vector 
#'         of size \eqn{33} containing the distance of a particular cell to the
#'         predicted position of the group centroid and the radius of the agent
#'         itself. Used to evaluate the group-centroid utility function (utility-
#'         based).}
#'   \item{\code{vf_angles}}{List containing a numeric vector of size \eqn{33} 
#'                           containing the relative angles of the agent's 
#'                           group members towards the agent for each cell. Used
#'                           to evaluate the visual-field utility function 
#'                           (utility-based).}
#' }
#' 
#' @export
"benchmark_data"

#' Initial condition used in benchmarks
#' 
#' This variable contains an instance of the \code{\link[predped]{state-class}}
#' that serves as an initial condition in the benchmarks.
#' 
#' @format Instance of the \code{\link[predped]{state-class}}
#' 
#' @export
"benchmark_inx"

#' Initial condition used in benchmarks (data)
#' 
#' This data.frame contains an initial condition that is used in the benchmarks.
#' 
#' @format A data frame with 70 rows and 30 variables:
#' \describe{
#'   \item{\code{iteration}}{Integer denoting the iteration number}
#'   \item{\code{time}}{Numeric denoting the time of the measurement}
#'   \item{\code{id}}{Character denoting the identifier of an agent}
#'   \item{\code{x}, \code{y}}{Numeric denoting the position of the agent}
#'   \item{\code{speed}}{Numeric denoting the speed of the agent}
#'   \item{\code{orientation}}{Numeric denoting the orientation of the agent}
#'   \item{\code{cell}}{Integer denoting the cell the agent moved to on this 
#'                      iteration}
#'   \item{\code{group}}{Numeric denoting group the agent belongs to}
#'   \item{\code{status}}{Character denoting what the agent was doing on this 
#'                        iteration}
#'   \item{\code{goal_id}}{Character denoting the identifier of the agent's 
#'                         current goal}
#'   \item{\code{goal_x}, \code{goal_y}}{Numeric denoting the position of the
#'                                       agent's current goal}
#'   \item{\code{radius}}{Numeric denoting agent's size}
#'   \item{\code{agent_idx}}{Integer denoting the index of the agent in the 
#'                           shared specification list (utility-related)}
#'   \item{\code{check}}{List containing a logical matrix denoting the locations 
#'                       the agent can and cannot move to (utility-related)}
#'   \item{\code{ps_speed}, \code{ps_distance}}{Numeric denoting the speed of 
#'                                              the agent and the distance from 
#'                                              the current goal, used to 
#'                                              evaluate the preferred speed 
#'                                              utility function (utility-related)}
#'   \item{\code{gd_angle}}{List containing a numeric matrix of size 
#'                          \eqn{1 \times 11}} containing the relative angles to 
#'                          the goals for each cone the agent potentially may 
#'                          have moved to
#'   \item{\code{id_distance}, \code{id_check}, \code{id_ingroup}}{
#'         List containing a numeric matrix of size \eqn{A \times 33} containing
#'         the distance to other agents from each cell, a logical matrix of size
#'         \eqn{11 \times 3} containing an update of cells the agent can and 
#'         cannot move to when taking agents into account, and a logical vector
#'         denoting whether the other agents belong to the current agents ingroup.
#'         This information is used to evaluate the interpersonal distance 
#'         utility function (utility-related).}
#'   \item{\code{ba_angle}, \code{ba_cones}}{}
#'   \item{\code{fl_leaders}, \code{wb_buddies}}{List containing the leaders and
#'         buddies needed to evaluate the follow-the-leader and walk-beside 
#'         utility functions (utility-related).}
#'   \item{\code{gc_distance}, \code{gc_radius}, \code{gc_nped}}{List containing 
#'         an integer denoting the number of group members, a numeric vector 
#'         of size \eqn{33} containing the distance of a particular cell to the
#'         predicted position of the group centroid and the radius of the agent
#'         itself. Used to evaluate the group-centroid utility function (utility-
#'         based).}
#'   \item{\code{vf_angles}}{List containing a numeric vector of size \eqn{33} 
#'                           containing the relative angles of the agent's 
#'                           group members towards the agent for each cell. Used
#'                           to evaluate the visual-field utility function 
#'                           (utility-based).}
#' }
#' 
#' @export
"benchmark_inx_data"

#' Trace used in benchmarks
#' 
#' This variable contains an instance of the \code{\link[predped]{trace-class}}
#' that serves in the benchmarks for testing data handling functions.
#' 
#' @format Instance of the \code{\link[predped]{trace-class}}
#' 
#' @export
"benchmark_trace"

#' Benchmarked parameters (bounded)
#' 
#' This data.frame contains a set of bounded parameters used to benchmark the 
#' parameter-related functions.
#' 
#' @format A data.frame with the rougly the same columns as the archetypes 
#' datasets, see \code{\link[predped]{load_parameters}}
#' 
#' @export
"benchmark_params_bounded"

#' Benchmarked parameters (unbounded)
#' 
#' This data.frame contains a set of unbounded parameters used to benchmark the 
#' parameter-related functions.
#' 
#' @format A data.frame with the rougly the same columns as the archetypes 
#' datasets, see \code{\link[predped]{load_parameters}}
#' 
#' @export
"benchmark_params_real"



################################################################################
# SPECIFICATIONS OF THE BENCHMARK

#' Load the arguments for the benchmark
#' 
#' This function serves as a wrapper around a named list defining the arguments
#' provided to the functions that should be benchmarked. 
#' 
#' @return Returns a named list containing the arguments for the benchmarks.
#' 
#' @noRd
load_benchmark_arguments <- function() {    
    # Create and return the list of arguments needed for benchmarking
    return(
        list(
            # background.R
            "compute_limited_access" = list(
                segment(from = c(0, 0), to = c(1, 1))
            ),
            "background" = list(
                rectangle(center = c(0, 0), size = c(2, 2)),
                lapply(1:10, \(x) rectangle(center = c(0, 0), size = c(1, 1))),
                lapply(1:10, \(x) segment(from = c(-1, -1), to = c(1, 1)))
            ),
            "limit_access" = list(
                background(
                    shape = rectangle(center = c(0, 0), size = c(2, 2)),
                    objects = lapply(1:10, \(x) rectangle(center = c(0, 0), size = c(1, 1))),
                    limited_access = lapply(1:10, \(x) segment(from = c(-1, -1), to = c(1, 1)))
                ),
                c(0, -0.5),
                agent(center = c(0, -0.5), radius = 0.25)
            ),

            # data.R
            "time_series" = list(benchmark_trace),
            "to_trace" = list(
                benchmark_data[benchmark_data$iteration <= 14, ],
                supermarket
            ),
            "unpack_trace" = list(benchmark_trace),

            # general.R
            "line_line_intersection" = list(
                cbind(rep(1, 100), rep(1, 100), rep(20, 100), rep(20, 100)),
                cbind(rep(20, 100), rep(1, 100), rep(1, 100), rep(20, 100))
            ),
            "perpendicular_orientation" = list(
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(1, 2), size = c(1, 1)),
                circle(center = c(1, 2), radius = 1), 
                c(0.1, 0.2)
            ),

            # goals.R
            "add_goal" = list(
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(0, 0), size = c(1, 1)), 
                circle(center = c(0, 0), radius = 1)
            ),
            "change" = list(
                goal(id = "goal exit", position = c(0, 13), counter = 10)
            ),
            "find_path" = list(
                goal(id = "goal exit", position = c(0, 13)),
                agent(center = c(0.3, 0.5), radius = 0.25)
            ),
            "goal_stack" = list(
                10,
                10
            ),
            "goal" = list(
                c(1, 2)
            ),
            "interact" = list(
                goal(id = "goal exit", position = c(0, 13), counter = 10)
            ),

            # likelihood.R
            "mll" = list(
                benchmark_data,
                benchmark_params_bounded,
                benchmark_params_real
            ),

            # moving_options.R
            "agents_between_goal" = list(
                agent(
                    center = c(1, 1), 
                    radius = 0.25, 
                    current_goal = goal(
                        position = c(0, 13), 
                        path = matrix(c(0, 13), nrow = 1)
                    )
                ),
                state(
                    iteration = 0,
                    setting = supermarket,
                    agents = append(
                        agent(
                            center = c(1, 1), 
                            radius = 0.25, 
                            current_goal = goal(
                                position = c(0, 13), 
                                path = matrix(c(0, 13), nrow = 1)
                            )
                        ),
                        lapply(1:10, \(x) agent(center = runif(2, 1, 20), radius = 0.25))
                    )
                )
            ),
            "compute_centers" = list(
                agent(
                    center = c(1, 1), 
                    radius = 0.25, 
                    current_goal = goal(
                        position = c(0, 13), 
                        path = matrix(c(0, 13), nrow = 1)
                    )
                )
            ),
            "moving_options" = list(
                agent(
                    center = c(1, 1), 
                    radius = 0.25, 
                    current_goal = goal(
                        position = c(0, 13), 
                        path = matrix(c(0, 13), nrow = 1)
                    )
                ),
                state(
                    iteration = 0,
                    setting = supermarket,
                    agents = append(
                        agent(
                            center = c(1, 1), 
                            radius = 0.25, 
                            current_goal = goal(
                                position = c(0, 13), 
                                path = matrix(c(0, 13), nrow = 1)
                            )
                        ),
                        lapply(1:10, \(x) agent(center = runif(2, 1, 20), radius = 0.25))
                    )
                ),
                compute_centers(
                    agent(
                        center = c(1, 1), 
                        radius = 0.25, 
                        current_goal = goal(
                            position = c(0, 13), 
                            path = matrix(c(0, 13), nrow = 1)
                        )
                    ),
                    cpp = FALSE
                )
            ),
            "overlap_with_objects" = list(
                agent(
                    center = c(1, 1), 
                    radius = 0.25, 
                    current_goal = goal(
                        position = c(0, 13), 
                        path = matrix(c(0, 13), nrow = 1)
                    )
                ),
                compute_centers(
                    agent(
                        center = c(1, 1), 
                        radius = 0.25, 
                        current_goal = goal(
                            position = c(0, 13), 
                            path = matrix(c(0, 13), nrow = 1)
                        )
                    ),
                    cpp = FALSE
                ),
                matrix(TRUE, nrow = 11, ncol = 3)
            ),

            # objects.R
            "add_nodes" = list(
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(1, 2), size = c(1, 1)),
                circle(center = c(1, 2), radius = 1)
            ),
            "enlarge" = list(
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(1, 2), size = c(1, 1)),
                circle(center = c(1, 2), radius = 1)
            ),
            "in_object" = list(
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(1, 2), size = c(1, 1)),
                circle(center = c(1, 2), radius = 1),
                cbind(rep(1, 100), rep(2, 100))
            ),
            "polygon" = list(
                rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))
            ),
            "rectangle" = list(
                c(1, 2),
                c(1, 1)
            ),
            "circle" = list(
                c(1, 2),
                1
            ),
            "intersects" = list(
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(1, 2), size = c(1, 1)),
                circle(center = c(1, 2), radius = 1)        
            ),
            "line_intersection" = list(
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(1, 2), size = c(1, 1)),
                circle(center = c(1, 2), radius = 1),
                cbind(rep(1, 100), rep(1, 100), rep(20, 100), rep(20, 100))
            ),
            "nodes_on_circumference" = list(
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(1, 2), size = c(1, 1)),
                circle(center = c(1, 2), radius = 1)
            ),
            "out_object" = list(
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(1, 2), size = c(1, 1)),
                circle(center = c(1, 2), radius = 1),
                cbind(rep(1, 100), rep(2, 100))
            ),
            "rng_point" = list(
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(0, 0), size = c(1, 1)), 
                circle(center = c(0, 0), radius = 1)
            ),

            # plot.R
            "plot" = list(
                agent(
                    center = c(0, 0), 
                    radius = 0.25, 
                    current_goal = goal(position = c(10, 0))
                ),
                polygon(points = rbind(c(1, 1), c(1, -1), c(-1, -1), c(-1, 1))),
                rectangle(center = c(0, 0), size = c(1, 1)), 
                circle(center = c(0, 0), radius = 1),
                benchmark_inx
            ),

            # routing.R
            "adjust_edges" = list(
                c(0.3, 0.5), 
                c(0, 13),
                lapply(1:10, \(x) agent(center = c(2, 1), radius = 0.25))
            ),
            "combine_nodes" = list(
                data.frame(
                    node_ID = paste0("node_", 1:100), 
                    X = 1:100, 
                    Y = 1:100
                )
            ),
            "compute_edges" = list(
                background(
                    shape = shape(supermarket),
                    objects = objects(supermarket),
                    limited_access = lapply(1:10, \(x) segment(from = c(0, 0), to = c(40, 25)))
                )
            ),
            "create_edges" = list(
                c(0.3, 0.5), 
                c(0, 13),
                background(
                    shape = shape(supermarket),
                    objects = objects(supermarket),
                    limited_access = lapply(1:10, \(x) segment(from = c(0, 0), to = c(40, 25)))
                )
            ),
            "create_nodes" = list(
                c(0.3, 0.5), 
                c(0, 13)
            ),
            "evaluate_edges" = list(
                benchmark_few_edges_uneval, 
                benchmark_many_edges_uneval,
                background(
                    shape = shape(supermarket),
                    objects = objects(supermarket),
                    limited_access = lapply(1:10, \(x) segment(from = c(0, 0), to = c(40, 25)))
                )
            ),
            "prune_edges" = list(
                objects(supermarket),
                cbind(rep(1, 100), rep(1, 100), rep(20, 100), rep(20, 100))
            ),

            # simulate.R
            "add_agent" = list(
                predped(
                    id = "benchmark",
                    setting = supermarket,
                    archetypes = "BaselineEuropean"
                )
            ),
            "create_initial_condition" = list(
                10, 
                predped(
                    id = "benchmark",
                    setting = supermarket,
                    archetypes = "BaselineEuropean"
                ),
                10
            ),
            "simulate" = list(
                predped(
                    id = "benchmark",
                    setting = supermarket, 
                    archetypes = "BaselineEuropean"
                ),
                1,
                benchmark_inx@agents
            ),

            # update-R
            "create_agent_specifications" = list(
                lapply(1:100, \(x) agent(center = c(-1, 0), radius = 0.2, speed = 2, orientation = 0))
            ),
            "predict_movement" = list(
                agent(center = c(-1, 0), radius = 0.2, speed = 2, orientation = 0)
            ),

            # utility.R
            "compute_utility_variables" = list(
                benchmark_inx,
                create_agent_specifications(benchmark_inx@agents, cpp = FALSE),
                matrix(1, nrow = 33, ncol = 2),
                matrix(TRUE, nrow = 11, ncol = 3)
            ),
            "utility" = list(
                benchmark_inx_data[benchmark_inx_data$status == "move", ],
                benchmark_params_bounded,
                benchmark_inx,
                create_agent_specifications(benchmark_inx@agents, cpp = FALSE),
                matrix(1, nrow = 33, ncol = 2),
                matrix(TRUE, nrow = 11, ncol = 3)
            )
        )
    )
}



#' Load the tests for the benchmark
#' 
#' This function serves as a wrapper around a named list defining the benchmarks
#' used for each function.
#' 
#' @return Returns a named list containing the tests for the benchmarks together
#' with an indicator defining the arguments that have been varied across 
#' different benchmarks.
#' 
#' @noRd
load_benchmark <- function() { 
    # Load the arguments that go into the tests
    args <- load_benchmark_arguments()

    # Create and return the list containing the tests
    return(
        list(
            # background.R
            "compute_limited_access" = list(
                " | " = function() {
                    return(
                        compute_limited_access(
                            args[["compute_limited_access"]][[1]]
                        )
                    )
                }
            ),
            "background" = list(
                "no limited_access" = function() {
                    return(
                        background(
                            args[["background"]][[1]],
                            args[["background"]][[2]]
                        )
                    )
                },
                "limited_access" = function() {
                    return(
                        background(
                            args[["background"]][[1]],
                            args[["background"]][[2]],
                            args[["background"]][[3]]
                        )
                    )
                }
            ),
            "limit_access" = list(
                "coordinate" = function() {
                    return(
                        limit_access(
                            args[["limit_access"]][[1]], 
                            args[["limit_access"]][[2]]
                        )
                    )
                },
                "agent" = function() {
                    return(
                        limit_access(
                            args[["limit_access"]][[1]], 
                            args[["limit_access"]][[3]]
                        )
                    )
                }
            ),

            # data.R
            "time_series" = list(
                "cpp = FALSE" = function() {
                    return(
                        time_series(
                            args[["time_series"]][[1]], 
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        time_series(
                            args[["time_series"]][[1]], 
                            cpp = TRUE
                        )
                    )
                }
            ),
            "to_trace" = list(
                "cpp = FALSE" = function() {
                    return(
                        to_trace(
                            args[["to_trace"]][[1]],
                            args[["to_trace"]][[2]], 
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        to_trace(
                            args[["to_trace"]][[1]],
                            args[["to_trace"]][[2]], 
                            cpp = TRUE
                        )
                    )
                }
            ),
            "unpack_trace" = list(
                "cpp = FALSE" = function() {
                    return(
                        unpack_trace(
                            args[["unpack_trace"]][[1]], 
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        unpack_trace(
                            args[["unpack_trace"]][[1]], 
                            cpp = TRUE
                        )
                    )
                }
            ),

            # general.R
            "line_line_intersection" = list(
                "cpp = FALSE" = function() {
                    return(
                        line_line_intersection(
                            args[["line_line_intersection"]][[1]],
                            args[["line_line_intersection"]][[2]],
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        line_line_intersection(
                            args[["line_line_intersection"]][[1]],
                            args[["line_line_intersection"]][[2]],
                            cpp = TRUE
                        )
                    )
                }
            ),
            "perpendicular_orientation" = list(
                "polygon" = function() {
                    return(
                        perpendicular_orientation(
                            args[["perpendicular_orientation"]][[1]],
                            args[["perpendicular_orientation"]][[4]]
                        )
                    )
                },
                "rectangle" = function() {
                    return(
                        perpendicular_orientation(
                            args[["perpendicular_orientation"]][[2]],
                            args[["perpendicular_orientation"]][[4]]
                        )
                    )
                },
                "circle" = function() {
                    return(
                        perpendicular_orientation(
                            args[["perpendicular_orientation"]][[3]],
                            args[["perpendicular_orientation"]][[4]]
                        )
                    )
                }
            ),

            # goals.R
            "add_goal" = list(
                "polygon | middle_edge = FALSE" = function() {
                    return(
                        add_goal(
                            args[["add_goal"]][[1]],
                            supermarket,
                            middle_edge = FALSE
                        )
                    )
                },
                "rectangle | middle_edge = FALSE" = function() {
                    return(
                        add_goal(
                            args[["add_goal"]][[2]],
                            supermarket,
                            middle_edge = FALSE
                        )
                    )
                },
                "circle | middle_edge = FALSE" = function() {
                    return(
                        add_goal(
                            args[["add_goal"]][[3]],
                            supermarket
                        )
                    )
                },
                "polygon | middle_edge = TRUE" = function() {
                    return(
                        add_goal(
                            args[["add_goal"]][[1]],
                            supermarket,
                            middle_edge = FALSE
                        )
                    )
                },
                "rectangle | middle_edge = TRUE" = function() {
                    return(
                        add_goal(
                            args[["add_goal"]][[2]],
                            supermarket,
                            middle_edge = FALSE
                        )
                    )
                },
                "circle | middle_edge = TRUE" = function() {
                    return(
                        add_goal(
                            args[["add_goal"]][[3]],
                            supermarket
                        )
                    )
                }
            ),
            "change" = list(
                " | " = function() {
                    return(
                        change(
                            args[["change"]][[1]], 
                            supermarket, 
                            counter = 5
                        )
                    )
                }
            ),
            "find_path" = list(
                "precomputed | many_nodes = FALSE | reevaluate = FALSE" = function() {
                    return(
                        find_path(
                            args[["find_path"]][[1]],
                            args[["find_path"]][[2]],
                            supermarket,
                            precomputed_edges = benchmark_few_edges,
                            reevaluate = FALSE
                        )
                    )
                },
                "precomputed | many_nodes = TRUE | reevaluate = FALSE" = function() {
                    return(
                        find_path(
                            args[["find_path"]][[1]],
                            args[["find_path"]][[2]],
                            supermarket,
                            precomputed_edges = benchmark_many_edges,
                            reevaluate = FALSE
                        )
                    )
                },
                "precomputed | many_nodes = FALSE | reevaluate = TRUE" = function() {
                    return(
                        find_path(
                            args[["find_path"]][[1]],
                            args[["find_path"]][[2]],
                            supermarket,
                            precomputed_edges = benchmark_few_edges,
                            reevaluate = TRUE
                        )
                    )
                },
                "precomputed | many_nodes = TRUE | reevaluate = TRUE" = function() {
                    return(
                        find_path(
                            args[["find_path"]][[1]],
                            args[["find_path"]][[2]],
                            supermarket,
                            precomputed_edges = benchmark_many_edges,
                            reevaluate = TRUE
                        )
                    )
                },
                "not precomputed | many_nodes = FALSE | reevaluate = FALSE" = function() {
                    return(
                        find_path(
                            args[["find_path"]][[1]],
                            args[["find_path"]][[2]],
                            supermarket,
                            precomputed_edges = NULL,
                            many_nodes = FALSE
                        )
                    )
                },
                "not precomputed | many_nodes = TRUE | reevaluate = FALSE" = function() {
                    return(
                        find_path(
                            args[["find_path"]][[1]],
                            args[["find_path"]][[2]],
                            supermarket,
                            precomputed_edges = NULL,
                            many_nodes = TRUE
                        )
                    )
                },
                "not precomputed | many_nodes = FALSE | reevaluate = TRUE" = function() {
                    return(NA)
                },
                "not precomputed | many_nodes = TRUE | reevaluate = TRUE" = function() {
                    return(NA)
                }
            ),
            "goal_stack" = list(
                " | " = function() {
                    return(
                        goal_stack(
                            args[["goal_stack"]][[1]],
                            supermarket,
                            counter = args[["goal_stack"]][[2]]
                        )
                    )
                }
            ),
            "goal" = list(
                " | " = function() {
                    return(
                        goal(
                            position = args[["goal"]][[1]]
                        )
                    )
                }
            ),
            "interact" = list(
                " | " = function() {
                    return(
                        interact(
                            args[["interact"]][[1]]
                        )
                    )
                }
            ),

            # likelihood.R
            "mll" = list(
                "transform = FALSE | cpp = FALSE | summed = FALSE" = function() {
                    return(
                        mll(
                            args[["mll"]][[1]],
                            args[["mll"]][[2]],
                            transform = FALSE,
                            cpp = FALSE,
                            summed = FALSE
                        )
                    )
                },
                "transform = TRUE | cpp = FALSE | summed = FALSE" = function() {
                    return(
                        mll(
                            args[["mll"]][[1]],
                            args[["mll"]][[3]],
                            transform = TRUE,
                            cpp = FALSE,
                            summed = FALSE
                        )
                    )
                },
                "transform = FALSE | cpp = TRUE | summed = FALSE" = function() {
                    return(
                        mll(
                            args[["mll"]][[1]],
                            args[["mll"]][[2]],
                            transform = FALSE,
                            cpp = TRUE,
                            summed = FALSE
                        )
                    )
                },
                "transform = TRUE | cpp = TRUE | summed = FALSE" = function() {
                    return(
                        mll(
                            args[["mll"]][[1]],
                            args[["mll"]][[3]],
                            transform = TRUE,
                            cpp = TRUE,
                            summed = FALSE
                        )
                    )
                },
                "transform = FALSE | cpp = FALSE | summed = TRUE" = function() {
                    return(
                        mll(
                            args[["mll"]][[1]],
                            args[["mll"]][[2]],
                            transform = FALSE,
                            cpp = FALSE,
                            summed = TRUE
                        )
                    )
                },
                "transform = TRUE | cpp = FALSE | summed = TRUE" = function() {
                    return(
                        mll(
                            args[["mll"]][[1]],
                            args[["mll"]][[3]],
                            transform = TRUE,
                            cpp = FALSE,
                            summed = TRUE
                        )
                    )
                },
                "transform = FALSE | cpp = TRUE | summed = TRUE" = function() {
                    return(
                        mll(
                            args[["mll"]][[1]],
                            args[["mll"]][[2]],
                            transform = FALSE,
                            cpp = TRUE,
                            summed = TRUE
                        )
                    )
                },
                "transform = TRUE | cpp = TRUE | summed = TRUE" = function() {
                    return(
                        mll(
                            args[["mll"]][[1]],
                            args[["mll"]][[3]],
                            transform = TRUE,
                            cpp = TRUE,
                            summed = TRUE
                        )
                    )
                }
            ),

            # moving_options.R
            "agents_between_goal" = list(
                " | " = function() {
                    return(
                        agents_between_goal(
                            args[["agents_between_goal"]][[1]], 
                            args[["agents_between_goal"]][[2]]
                        )
                    )
                }
            ),
            "compute_centers" = list(
                "cpp = FALSE" = function() {
                    return(
                        compute_centers(
                            args[["compute_centers"]][[1]],
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        compute_centers(
                            args[["compute_centers"]][[1]],
                            cpp = TRUE
                        )
                    )
                }
            ),
            "moving_options" = list(
                "cpp = FALSE" = function() {
                    return(
                        moving_options(
                            args[["moving_options"]][[1]],
                            args[["moving_options"]][[2]],
                            supermarket,
                            args[["moving_options"]][[3]],
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        moving_options(
                            args[["moving_options"]][[1]],
                            args[["moving_options"]][[2]],
                            supermarket,
                            args[["moving_options"]][[3]],
                            cpp = TRUE
                        )
                    )
                }
            ),
            "overlap_with_objects" = list(
                "cpp = FALSE" = function() {
                    return(
                        overlap_with_objects(
                            args[["overlap_with_objects"]][[1]],
                            supermarket,
                            args[["overlap_with_objects"]][[2]],
                            args[["overlap_with_objects"]][[3]],
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        overlap_with_objects(
                            args[["overlap_with_objects"]][[1]],
                            supermarket,
                            args[["overlap_with_objects"]][[2]],
                            args[["overlap_with_objects"]][[3]],
                            cpp = TRUE
                        )
                    )
                }
            ),

            # objects.R
            "add_nodes" = list(
                "polygon | only_corners = FALSE" = function() {
                    return(
                        add_nodes(
                            args[["add_nodes"]][[1]],
                            space_between = 0.5,
                            only_corners = FALSE
                        )
                    )
                },
                "rectangle | only_corners = FALSE" = function() {
                    return(
                        add_nodes(
                            args[["add_nodes"]][[2]],
                            space_between = 0.5,
                            only_corners = FALSE
                        )
                    )
                },
                "circle | only_corners = FALSE" = function() {
                    return(
                        add_nodes(
                            args[["add_nodes"]][[3]],
                            space_between = 0.5,
                            only_corners = FALSE
                        )
                    )
                },
                "polygon | only_corners = TRUE" = function() {
                    return(
                        add_nodes(
                            args[["add_nodes"]][[1]],
                            space_between = 0.5,
                            only_corners = TRUE
                        )
                    )
                },
                "rectangle | only_corners = TRUE" = function() {
                    return(
                        add_nodes(
                            args[["add_nodes"]][[2]],
                            space_between = 0.5,
                            only_corners = TRUE
                        )
                    )
                },
                "circle | only_corners = TRUE" = function() {
                    return(
                        add_nodes(
                            args[["add_nodes"]][[3]],
                            space_between = 0.5,
                            only_corners = TRUE
                        )
                    )
                }
            ),
            "enlarge" = list(
                "polygon" = function() {
                    return(
                        enlarge(
                            args[["enlarge"]][[1]],
                            0.5
                        )
                    )
                },
                "rectangle" = function() {
                    return(
                        enlarge(
                            args[["enlarge"]][[2]],
                            0.5
                        )
                    )
                },
                "circle" = function() {
                    return(
                        enlarge(
                            args[["enlarge"]][[3]],
                            0.5
                        )
                    )
                }
            ),
            "in_object" = list(
                "polygon | cpp = FALSE" = function() {
                    return(
                        in_object(
                            args[["in_object"]][[1]],
                            args[["in_object"]][[4]],
                            cpp = FALSE
                        )
                    )
                },
                "rectangle | cpp = FALSE" = function() {
                    return(
                        in_object(
                            args[["in_object"]][[2]],
                            args[["in_object"]][[4]],
                            cpp = FALSE
                        )
                    )
                },
                "circle | cpp = FALSE" = function() {
                    return(
                        in_object(
                            args[["in_object"]][[3]],
                            args[["in_object"]][[4]],
                            cpp = FALSE
                        )
                    )
                },
                "polygon | cpp = TRUE" = function() {
                    return(
                        in_object(
                            args[["in_object"]][[1]],
                            args[["in_object"]][[4]],
                            cpp = TRUE
                        )
                    )
                },
                "rectangle | cpp = TRUE" = function() {
                    return(
                        in_object(
                            args[["in_object"]][[2]],
                            args[["in_object"]][[4]],
                            cpp = TRUE
                        )
                    )
                },
                "circle | cpp = TRUE" = function() {
                    return(
                        in_object(
                            args[["in_object"]][[3]],
                            args[["in_object"]][[4]],
                            cpp = TRUE
                        )
                    )
                }
            ),
            "polygon" = list(
                " | " = function() {
                    return(
                        polygon(
                            points = args[["polygon"]][[1]]
                        )
                    )
                }
            ),
            "rectangle" = list(
                " | " = function() {
                    return(
                        rectangle(
                            center = args[["rectangle"]][[1]],
                            size = args[["rectangle"]][[2]]
                        )
                    )
                }
            ),
            "circle" = list(
                " | " = function() {
                    return(
                        circle(
                            center = args[["circle"]][[1]],
                            radius = args[["circle"]][[2]]
                        )
                    )
                }
            ),
            "intersects" = list(
                "polygon | polygon" = function() {
                    return(
                        intersects(
                            args[["intersects"]][[1]],
                            args[["intersects"]][[1]]
                        )
                    )
                },
                "polygon | rectangle" = function() {
                    return(
                        intersects(
                            args[["intersects"]][[1]],
                            args[["intersects"]][[2]]
                        )
                    )
                },
                "polygon | circle" = function() {
                    return(
                        intersects(
                            args[["intersects"]][[1]],
                            args[["intersects"]][[3]]
                        )
                    )
                },
                "rectangle | polygon" = function() {
                    return(
                        intersects(
                            args[["intersects"]][[2]],
                            args[["intersects"]][[1]]
                        )
                    )
                },
                "rectangle | rectangle" = function() {
                    return(
                        intersects(
                            args[["intersects"]][[2]],
                            args[["intersects"]][[2]]
                        )
                    )
                },
                "rectangle | circle" = function() {
                    return(
                        intersects(
                            args[["intersects"]][[2]],
                            args[["intersects"]][[3]]
                        )
                    )
                },
                "circle | polygon" = function() {
                    return(
                        intersects(
                            args[["intersects"]][[3]],
                            args[["intersects"]][[1]]
                        )
                    )
                },
                "circle | rectangle" = function() {
                    return(
                        intersects(
                            args[["intersects"]][[3]],
                            args[["intersects"]][[2]]
                        )
                    )
                },
                "circle | circle" = function() {
                    return(
                        intersects(
                            args[["intersects"]][[3]],
                            args[["intersects"]][[3]]
                        )
                    )
                }
            ),
            "line_intersection" = list(
                "polygon | cpp = FALSE" = function() {
                    return(
                        line_intersection(
                            args[["line_intersection"]][[1]],
                            args[["line_intersection"]][[4]],
                            cpp = FALSE
                        )
                    )
                },
                "rectangle | cpp = FALSE" = function() {
                    return(
                        line_intersection(
                            args[["line_intersection"]][[2]],
                            args[["line_intersection"]][[4]],
                            cpp = FALSE
                        )
                    )
                },
                "circle | cpp = FALSE" = function() {
                    return(
                        line_intersection(
                            args[["line_intersection"]][[3]],
                            args[["line_intersection"]][[4]],
                            cpp = FALSE
                        )
                    )
                },
                "polygon | cpp = TRUE" = function() {
                    return(
                        line_intersection(
                            args[["line_intersection"]][[1]],
                            args[["line_intersection"]][[4]],
                            cpp = TRUE
                        )
                    )
                },
                "rectangle | cpp = TRUE" = function() {
                    return(
                        line_intersection(
                            args[["line_intersection"]][[2]],
                            args[["line_intersection"]][[4]],
                            cpp = TRUE
                        )
                    )
                },
                "circle | cpp = TRUE" = function() {
                    return(
                        line_intersection(
                            args[["line_intersection"]][[3]],
                            args[["line_intersection"]][[4]],
                            cpp = TRUE
                        )
                    )
                }
            ),
            "nodes_on_circumference" = list(
                "polygon | cpp = FALSE" = function() {
                    return(
                        nodes_on_circumference(
                            args[["nodes_on_circumference"]][[1]],
                            cpp = FALSE
                        )
                    )
                },
                "rectangle | cpp = FALSE" = function() {
                    return(
                        nodes_on_circumference(
                            args[["nodes_on_circumference"]][[2]],
                            cpp = FALSE
                        )
                    )
                },
                "circle | cpp = FALSE" = function() {
                    return(
                        nodes_on_circumference(
                            args[["nodes_on_circumference"]][[3]],
                            cpp = FALSE
                        )
                    )
                },
                "polygon | cpp = TRUE" = function() {
                    return(
                        nodes_on_circumference(
                            args[["nodes_on_circumference"]][[1]],
                            cpp = TRUE
                        )
                    )
                },
                "rectangle | cpp = TRUE" = function() {
                    return(
                        nodes_on_circumference(
                            args[["nodes_on_circumference"]][[2]],
                            cpp = TRUE
                        )
                    )
                },
                "circle | cpp = TRUE" = function() {
                    return(
                        nodes_on_circumference(
                            args[["nodes_on_circumference"]][[3]],
                            cpp = TRUE
                        )
                    )
                }
            ),
            "out_object" = list(
                "polygon | cpp = FALSE" = function() {
                    return(
                        out_object(
                            args[["out_object"]][[1]],
                            args[["out_object"]][[4]],
                            cpp = FALSE
                        )
                    )
                },
                "rectangle | cpp = FALSE" = function() {
                    return(
                        out_object(
                            args[["out_object"]][[2]],
                            args[["out_object"]][[4]],
                            cpp = FALSE
                        )
                    )
                },
                "circle | cpp = FALSE" = function() {
                    return(
                        out_object(
                            args[["out_object"]][[3]],
                            args[["out_object"]][[4]],
                            cpp = FALSE
                        )
                    )
                },
                "polygon | cpp = TRUE" = function() {
                    return(
                        out_object(
                            args[["out_object"]][[1]],
                            args[["out_object"]][[4]],
                            cpp = TRUE
                        )
                    )
                },
                "rectangle | cpp = TRUE" = function() {
                    return(
                        out_object(
                            args[["out_object"]][[2]],
                            args[["out_object"]][[4]],
                            cpp = TRUE
                        )
                    )
                },
                "circle | cpp = TRUE" = function() {
                    return(
                        out_object(
                            args[["out_object"]][[3]],
                            args[["out_object"]][[4]],
                            cpp = TRUE
                        )
                    )
                }
            ),
            "rng_point" = list(
                "polygon | middle_edge = FALSE" = function() {
                    return(
                        rng_point(
                            args[["rng_point"]][[1]],
                            middle_edge = FALSE
                        )
                    )
                },
                "rectangle | middle_edge = FALSE" = function() {
                    return(
                        rng_point(
                            args[["rng_point"]][[2]],
                            middle_edge = FALSE
                        )
                    )
                },
                "circle | middle_edge = FALSE" = function() {
                    return(
                        rng_point(
                            args[["rng_point"]][[3]],
                            middle_edge = FALSE
                        )
                    )
                },
                "polygon | middle_edge = TRUE" = function() {
                    return(
                        rng_point(
                            args[["rng_point"]][[1]],
                            middle_edge = TRUE
                        )
                    )
                },
                "rectangle | middle_edge = TRUE" = function() {
                    return(
                        rng_point(
                            args[["rng_point"]][[2]],
                            middle_edge = TRUE
                        )
                    )
                },
                "circle | middle_edge = TRUE" = function() {
                    return(
                        rng_point(
                            args[["rng_point"]][[3]],
                            middle_edge = TRUE
                        )
                    )
                }
            ),

            # plot.R
            "plot" = list(
                "agent | optimize = FALSE" = function() {
                    return(
                        plot(
                            args[["plot"]][[1]]
                        )
                    )
                },
                "background | optimize = FALSE" = function() {
                    return(
                        plot(
                            supermarket,
                            optimize = FALSE
                        )
                    )
                },
                "polygon | optimize = FALSE" = function() {
                    return(
                        plot(
                            args[["plot"]][[2]]
                        )
                    )
                },
                "rectangle | optimize = FALSE" = function() {
                    return(
                        plot(
                            args[["plot"]][[3]]
                        )
                    )
                },
                "circle | optimize = FALSE" = function() {
                    return(
                        plot(
                            args[["plot"]][[4]]
                        )
                    )
                },
                "state | optimize = FALSE" = function() {
                    return(
                        plot(
                            args[["plot"]][[5]],
                            optimize = FALSE
                        )
                    )
                },
                "agent | optimize = TRUE" = function() {
                    return(
                        plot(
                            args[["plot"]][[1]]
                        )
                    )
                },
                "background | optimize = TRUE" = function() {
                    return(
                        plot(
                            supermarket,
                            optimize = TRUE
                        )
                    )
                },
                "polygon | optimize = TRUE" = function() {
                    return(
                        plot(
                            args[["plot"]][[2]]
                        )
                    )
                },
                "rectangle | optimize = TRUE" = function() {
                    return(
                        plot(
                            args[["plot"]][[3]]
                        )
                    )
                },
                "circle | optimize = TRUE" = function() {
                    return(
                        plot(
                            args[["plot"]][[4]]
                        )
                    )
                },
                "state | optimize = TRUE" = function() {
                    return(
                        plot(
                            args[["plot"]][[5]],
                            optimize = TRUE
                        )
                    )
                }
            ),

            # routing.R
            "adjust_edges" = list(
                "few edges | reevaluate = FALSE" = function() {
                    return(
                        adjust_edges(
                            args[["adjust_edges"]][[1]],
                            args[["adjust_edges"]][[2]],
                            supermarket, 
                            new_objects = args[["adjust_edges"]][[3]],
                            precomputed_edges = benchmark_few_edges,
                            reevaluate = FALSE
                        )
                    )
                },
                "many edges | reevaluate = FALSE" = function() {
                    return(
                        adjust_edges(
                            args[["adjust_edges"]][[1]],
                            args[["adjust_edges"]][[2]],
                            supermarket, 
                            new_objects = args[["adjust_edges"]][[3]],
                            precomputed_edges = benchmark_many_edges,
                            reevaluate = FALSE
                        )
                    )
                },
                "few edges | reevaluate = TRUE" = function() {
                    return(
                        adjust_edges(
                            args[["adjust_edges"]][[1]],
                            args[["adjust_edges"]][[2]],
                            supermarket, 
                            new_objects = args[["adjust_edges"]][[3]],
                            precomputed_edges = benchmark_few_edges,
                            reevaluate = TRUE
                        )
                    )
                },
                "many edges | reevaluate = TRUE" = function() {
                    return(
                        adjust_edges(
                            args[["adjust_edges"]][[1]],
                            args[["adjust_edges"]][[2]],
                            supermarket, 
                            new_objects = args[["adjust_edges"]][[3]],
                            precomputed_edges = benchmark_many_edges,
                            reevaluate = TRUE
                        )
                    )
                }
            ),
            "combine_nodes" = list(
                "single" = function() {
                    return(
                        combine_nodes(
                            args[["combine_nodes"]][[1]]
                        )
                    )
                },
                "pair" = function() {
                    return(
                        combine_nodes(
                            args[["combine_nodes"]][[1]],
                            args[["combine_nodes"]][[1]]
                        )
                    )
                }
            ),
            "compute_edges" = list(
                "unlimited | many_nodes = FALSE" = function() {
                    return(
                        compute_edges(
                            supermarket, 
                            many_nodes = FALSE
                        )
                    )
                },
                "limited access | many_nodes = FALSE" = function() {
                    return(
                        compute_edges(
                            args[["compute_edges"]][[1]], 
                            many_nodes = FALSE
                        )
                    )
                },
                "unlimited | many_nodes = TRUE" = function() {
                    return(
                        compute_edges(
                            supermarket, 
                            many_nodes = TRUE
                        )
                    )
                },
                "limited access | many_nodes = TRUE" = function() {
                    return(
                        compute_edges(
                            args[["compute_edges"]][[1]], 
                            many_nodes = TRUE
                        )
                    )
                }
            ),
            "create_edges" = list(
                "unlimited | many_nodes = FALSE" = function() {
                    return(
                        create_edges(
                            args[["create_edges"]][[1]],
                            args[["create_edges"]][[2]],
                            supermarket, 
                            many_nodes = FALSE
                        )
                    )
                },
                "limited access | many_nodes = FALSE" = function() {
                    return(
                        create_edges(
                            args[["create_edges"]][[1]],
                            args[["create_edges"]][[2]],
                            args[["create_edges"]][[3]], 
                            many_nodes = FALSE
                        )
                    )
                },
                "unlimited | many_nodes = TRUE" = function() {
                    return(
                        create_edges(
                            args[["create_edges"]][[1]],
                            args[["create_edges"]][[2]],
                            supermarket, 
                            many_nodes = TRUE
                        )
                    )
                },
                "limited access | many_nodes = TRUE" = function() {
                    return(
                        create_edges(
                            args[["create_edges"]][[1]],
                            args[["create_edges"]][[2]],
                            args[["create_edges"]][[3]], 
                            many_nodes = TRUE
                        )
                    )
                }
            ),
            "create_nodes" = list(
                "many_nodes = FALSE" = function() {
                    return(
                        create_nodes(
                            args[["create_nodes"]][[1]],
                            args[["create_nodes"]][[2]],
                            supermarket, 
                            many_nodes = FALSE
                        )
                    )
                },
                "many_nodes = TRUE" = function() {
                    return(
                        create_nodes(
                            args[["create_nodes"]][[1]],
                            args[["create_nodes"]][[2]],
                            supermarket, 
                            many_nodes = TRUE
                        )
                    )
                }
            ),
            "evaluate_edges" = list(
                "unlimited | few nodes" = function() {
                    return(
                        evaluate_edges(
                            args[["evaluate_edges"]][[1]],
                            supermarket,
                            0.5
                        )
                    )
                },
                "limited access | few nodes" = function() {
                    return(
                        evaluate_edges(
                            args[["evaluate_edges"]][[1]],
                            args[["evaluate_edges"]][[3]],
                            0.5
                        )
                    )
                },
                "unlimited | many nodes" = function() {
                    return(
                        evaluate_edges(
                            args[["evaluate_edges"]][[2]],
                            supermarket,
                            0.5
                        )
                    )
                },
                "limited access | many nodes" = function() {
                    return(
                        evaluate_edges(
                            args[["evaluate_edges"]][[2]],
                            args[["evaluate_edges"]][[3]],
                            0.5
                        )
                    )
                }
            ),
            "prune_edges" = list(
                "cpp = FALSE" = function() {
                    return(
                        prune_edges(
                            args[["prune_edges"]][[1]],
                            args[["prune_edges"]][[2]],
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        prune_edges(
                            args[["prune_edges"]][[1]],
                            args[["prune_edges"]][[2]],
                            cpp = TRUE
                        )
                    )
                }
            ),

            # simulate.R
            "add_agent" = list(
                "sort_goals = FALSE | individual_differences = FALSE" = function() {
                    return(
                        add_agent(
                            args[["add_agent"]][[1]],
                            sort_goals = FALSE,
                            individual_differences = FALSE
                        )
                    )
                },
                "sort_goals = TRUE | individual_differences = FALSE" = function() {
                    return(
                        add_agent(
                            args[["add_agent"]][[1]],
                            sort_goals = TRUE,
                            individual_differences = FALSE
                        )
                    )
                },
                "sort_goals = FALSE | individual_differences = TRUE" = function() {
                    return(
                        add_agent(
                            args[["add_agent"]][[1]],
                            sort_goals = FALSE,
                            individual_differences = TRUE
                        )
                    )
                },
                "sort_goals = TRUE | individual_differences = TRUE" = function() {
                    return(
                        add_agent(
                            args[["add_agent"]][[1]],
                            sort_goals = TRUE,
                            individual_differences = TRUE
                        )
                    )
                }
            ),
            "create_initial_condition" = list(
                " | " = function() {
                    return(
                        create_initial_condition(
                            args[["create_initial_condition"]][[1]],
                            args[["create_initial_condition"]][[2]],
                            goal_number = args[["create_initial_condition"]][[3]]
                        )
                    )
                }
            ),
            "simulate" = list(
                "precompute_edges = FALSE | many_nodes = FALSE" = function() {
                    return(
                        capture.output(
                            simulate(
                                args[["simulate"]][[1]],
                                iterations = args[["simulate"]][[2]],
                                max_agents = 70,
                                initial_agents = args[["simulate"]][[3]],
                                precompute_edges = FALSE,
                                many_nodes = FALSE
                            )
                        )
                    )
                },
                "precompute_edges = TRUE | many_nodes = FALSE" = function() {
                    return(
                        capture.output(
                            simulate(
                                args[["simulate"]][[1]],
                                iterations = args[["simulate"]][[2]],
                                max_agents = 70,
                                initial_agents = args[["simulate"]][[3]],
                                precompute_edges = TRUE,
                                many_nodes = FALSE
                            )
                        )
                    )
                },
                "precompute_edges = FALSE | many_nodes = TRUE" = function() {
                    return(
                        capture.output(
                            simulate(
                                args[["simulate"]][[1]],
                                iterations = args[["simulate"]][[2]],
                                max_agents = 70,
                                initial_agents = args[["simulate"]][[3]],
                                precompute_edges = FALSE,
                                many_nodes = TRUE
                            )
                        )
                    )
                },
                "precompute_edges = TRUE | many_nodes = TRUE" = function() {
                    return(
                        capture.output(
                            simulate(
                                args[["simulate"]][[1]],
                                iterations = args[["simulate"]][[2]],
                                max_agents = 70,
                                initial_agents = args[["simulate"]][[3]],
                                precompute_edges = TRUE,
                                many_nodes = TRUE
                            )
                        )
                    )
                }
            ),

            # update-R
            "create_agent_specifications" = list(
                "cpp = FALSE" = function() {
                    return(
                        create_agent_specifications(
                            args[["create_agent_specifications"]][[1]],
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        create_agent_specifications(
                            args[["create_agent_specifications"]][[1]],
                            cpp = TRUE
                        )
                    )
                }
            ),
            "predict_movement" = list(
                "cpp = FALSE" = function() {
                    return(
                        predict_movement(
                            args[["predict_movement"]][[1]],
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        predict_movement(
                            args[["predict_movement"]][[1]],
                            cpp = TRUE
                        )
                    )
                }
            ),
            
            # utility.R
            "compute_utility_variables" = list(
                "cpp = FALSE" = function() {
                    return(
                        compute_utility_variables(
                            args[["compute_utility_variables"]][[1]]@agents[[3]],
                            args[["compute_utility_variables"]][[1]],
                            args[["compute_utility_variables"]][[1]]@setting,
                            args[["compute_utility_variables"]][[2]],
                            args[["compute_utility_variables"]][[3]],
                            args[["compute_utility_variables"]][[4]],
                            cpp = FALSE
                        )
                    )
                },
                "cpp = TRUE" = function() {
                    return(
                        compute_utility_variables(
                            args[["compute_utility_variables"]][[1]]@agents[[3]],
                            args[["compute_utility_variables"]][[1]],
                            args[["compute_utility_variables"]][[1]]@setting,
                            args[["compute_utility_variables"]][[2]],
                            args[["compute_utility_variables"]][[3]],
                            args[["compute_utility_variables"]][[4]],
                            cpp = TRUE
                        )
                    )
                }
            ),
            "utility" = list(
                "data | cpp = FALSE" = function() {
                    return(
                        utility(
                            args[["utility"]][[1]][1, ],
                            args[["utility"]][[2]],
                            cpp = FALSE
                        )
                    )
                },
                "trace | cpp = FALSE" = function() {
                    return(
                        utility(
                            args[["utility"]][[3]]@agents[[1]],
                            args[["utility"]][[3]],
                            args[["utility"]][[3]]@setting,
                            args[["utility"]][[4]],
                            args[["utility"]][[5]],
                            args[["utility"]][[6]],
                            cpp = FALSE
                        )
                    )
                },
                "data | cpp = TRUE" = function() {
                    return(
                        utility(
                            args[["utility"]][[1]][1, ],
                            args[["utility"]][[2]],
                            cpp = TRUE
                        )
                    )
                },
                "trace | cpp = TRUE" = function() {
                    return(
                        utility(
                            args[["utility"]][[3]]@agents[[1]],
                            args[["utility"]][[3]],
                            args[["utility"]][[3]]@setting,
                            args[["utility"]][[4]],
                            args[["utility"]][[5]],
                            args[["utility"]][[6]],
                            cpp = TRUE
                        )
                    )
                }
            )
        )
    )
}
                           
#' Load the hierarchy of test for the benchmark
#' 
#' This function serves as a wrapper around a named list defining the hierarchy
#' for the tests in the benchmark, immediately providing us with an idea of where
#' a particular function can be found in the R-folder.
#' 
#' @return Returns a named list containing the hierarchy for the benchmarks 
#' under the \code{"hierarchy"} label and accompanying captions explaining the 
#' test under the \code{"caption"} label.
#' 
#' @noRd
load_benchmark_specifications <- function() { 
    return(
        list(
            "hierarchy" = list(
                "background.R" = c(
                    "compute_limited_access", 
                    "background",
                    "limit_access"
                ),

                "data.R" = c(
                    "time_series",
                    "to_trace",
                    "unpack_trace"
                ),
                
                "general.R" = c(
                    "line_line_intersection",
                    "perpendicular_orientation"
                ),

                "goals.R" = c(
                    "add_goal",
                    "change",
                    "find_path",
                    "goal_stack",
                    "goal",
                    "interact"
                ),

                "likelihood.R" = c("mll"),

                "moving_options.R" = c(
                    "agents_between_goal",
                    "compute_centers",
                    "moving_options",
                    "overlap_with_objects"
                ),

                "objects.R" = c(
                    "add_nodes",
                    "enlarge",
                    "in_object",
                    "polygon",
                    "rectangle",
                    "circle",
                    "intersects",
                    "line_intersection",
                    "nodes_on_circumference",
                    "out_object",
                    "rng_point"
                ),

                "plot.R" = c("plot"),

                "routing.R" = c(
                    "adjust_edges",
                    "combine_nodes",
                    "compute_edges",
                    "create_edges",
                    "create_nodes",
                    "evaluate_edges",
                    "prune_edges"
                ),
                
                "simulate.R" = c(
                    "add_agent",
                    "create_initial_condition",
                    "simulate"
                ),

                "update-R" = c(
                    "create_agent_specifications",
                    "predict_movement"
                ),

                "utility.R" = c(
                    "compute_utility_variables",
                    "utility"
                )
            ),
            "caption" = list(
                "compute_limited_access" = "Benchmark for the compute\\_limited\\_access function, evaluated for a single segment.", 
                "background" = paste(
                    "Benchmark for the initialization function for background.",
                    "The background has a rectangular shape and consists of 10 rectangular objects as well as", 
                    "10 segments that limit access."
                ),
                "limit_access" = paste(
                    "Benchmark for the limit\\_access function for background.",
                    "The background has a rectangular shape and consists of 10 rectangular objects as well as", 
                    "10 segments that limit access.",
                    "We placed an agent or coordinate at a location so that all segments block access."
                ),
                "time_series" = paste(
                    "Benchmark for the time\\_series function.",
                    "A trace of length 10 and containing 3 agents is transformed to a dataset.",
                    "We distinguish between the R and the Rcpp version of this function."
                ),
                "to_trace" = paste(
                    "Benchmark for the to\\_trace function.", 
                    "Transformed data consist of 10 datapoints of 10 different agents within the default supermarket",
                    "environment.",
                    "We distinguish between the R and Rcpp version of this function."
                ),
                "unpack_trace" = paste(
                    "Benchmark for the unpack\\_trace function.",
                    "A trace of length 10 and containing 3 agents is transformed to a dataset containing",
                    "all information that is contained within the trace.", 
                    "We distinguish between the R and the Rcpp version of this function."
                ),   
                "line_line_intersection" = paste(
                    "Benchmark for the line\\_line\\_intersection function.",
                    "Intersections between two matrices of 100 segments are evaluated."
                ),
                "perpendicular_orientation" = paste(
                    "Benchmark for the perpendicular\\_orientation function.",
                    "The perpendicular orientation is evaluated for a single coordinate across",
                    "the different objects that are exported by predped."
                ),
                "add_goal" = paste(
                    "Benchmark for the add\\_goal function with a single goal being added to an object.", 
                    "We distinguish between adding a goal for each object that is exported by predped.",
                    "We additionally distinguish between adding a goal at a random location on the object's",
                    "circumference (middle\\_edge = FALSE) or at predefined locations (middle\\_edge = TRUE)."
                ),
                "change" = "Benchmark for the change function, changing a single goal for another one.",
                "find_path" = paste(
                    "Benchmark for the find\\_path function.", 
                    "A distinction is made between finding a path using only a few or many edges along which the",
                    "agent can walk (many\\_nodes = FALSE or TRUE), whether the edges are precomputed or not",
                    "(precomputed\\_edges is either NULL or not), and whether to reevaluate the validity of the",
                    "provided edges (reevaluate = FALSE or TRUE).",
                    "Note that if precomputed\\_edge is NULL, reevaluate is not used as an argument."
                ),
                "goal_stack" = "Benchmark for the goal\\_stack function, used to generate a list of 10 goals.",
                "goal" = "Benchmark for the initialization function for goal using its default settings.",
                "interact" = "Benchmark for the interact function, allowing the interaction for only a single goal.",
                "mll" = paste(
                    "Benchmark for the mll function.",
                    "The function evaluates the likelihood of 10 datapoints of 10 different agents.",
                    "We distinguish between providing parameters on the real or the bounded scale",
                    "(transform = TRUE or FALSE) and between using the R or Rcpp version of this function", 
                    "(cpp = FALSE or TRUE)."
                ),
                "agents_between_goal" = "Benchmark for the agents\\_between\\_goal function with 10 other agents in the room.", 
                "compute_centers" = paste(
                    "Benchmark for the compute\\_centers function.", 
                    "We distinguish between the R and Rcpp versions of this function (cpp = FALSE or TRUE)."
                ), 
                "moving_options" = paste(
                    "Benchmark for the moving\\_options function. ", 
                    "The environment consisted of the default supermarket containing 10 other agents.",
                    "We distinguish between the R and Rcpp versions of this function (cpp = FALSE or TRUE)."
                ),
                "overlap_with_objects" = paste(
                    "Benchmark for the overlap\\_with\\_objects function.", 
                    "Objects consisted of those in the default supermarket.",
                    "We distinguish between the R and Rcpp versions of this function (cpp = FALSE or TRUE)."
                ),
                "add_nodes" = paste(
                    "Benchmark for the add\\_nodes function.",
                    "We distinguish between the different types of objects that nodes can be added to as well as",
                    "between having either only nodes on the corners or on the complete circumference of the object",
                    "(only\\_corners = TRUE or FALSE)."
                ),
                "enlarge" = paste(
                    "Benchmark for the enlarge function.",
                    "We distinguish between the different types of objects that can be enlarged."
                ),
                "in_object" = paste(
                    "Benchmark for the in\\_object function.",
                    "For the benchmark, we evaluate whether one or more of 100 points fall inside of an object.",
                    "We distinguish between the different types of objects that are exported by predped as well as",
                    "between whether we use the R or Rcpp version of this function (cpp = FALSE or TRUE)."
                ),
                "polygon" = "Benchmark for the initialization function of the polygon, providing it with four points.",
                "rectangle" = "Benchmark for the initialization function of the rectangle, providing it with a center and a size.",
                "circle" = "Benchmark for the initialization function of the circle, providing it with a center and a radius.",
                "intersects" = paste(
                    "Benchmark for the intersects function.",
                    "We distinguish between different combinations of objects that can intersect."
                ),
                "line_intersection" = paste(
                    "Benchmark for the line\\_intersection function.", 
                    "We evaluate whether one or more of 100 segments intersects with the provided object.",
                    "We distinguish between the different objects that are exported by predped."
                ),
                "nodes_on_circumference" = paste(
                    "Benchmark for the nodes\\_on\\_circumference function using the default space_between.",
                    "We distinguish between adding nodes on the circumference of the different",
                    "objects that are exported by predped as well as between whether we use the R or",
                    "Rcpp version of this function (cpp = FALSE or TRUE)."
                ),
                "out_object" = paste(
                    "Benchmark for the out\\_object function.",
                    "For the benchmark, we evaluate whether one or more of 100 points fall outside of an object.",
                    "We distinguish between the different types of objects that are exported by predped as well as",
                    "between whether we use the R or Rcpp version of this function (cpp = FALSE or TRUE)."
                ),
                "rng_point" = paste(
                    "Benchmark for the rng\\_point function. ", 
                    "We distinguish between the different types of objects that are exported by predped as well as",
                    "between whether a point is generated at a random location on the object's circumference or at",
                    "one of the prespecified locations (middle\\_edge = FALSE or TRUE)."
                ),
                "plot" = paste(
                    "Benchmark for the plot function.",
                    "We distinguish between all different objects that can be plotted - using the default supermarket for",
                    "plotting a background and the same supermarket with 70 agents for plotting a state - and between ",
                    "whether we optimize the plotting or not (optimize = TRUE or FALSE)."
                ),
                "adjust_edges" = paste(
                    "Benchmark for the adjust\\_edges function.", 
                    "We use the default supermarket with 10 agents for this benchmark.",
                    "We distinguish between few or many edges that need adjusting as well as between",
                    "whether these edges are reevaluated as a whole (reevaluate = TRUE or FALSE)."
                ),
                "combine_nodes" = paste(
                    "Benchmark for the combine\\_nodes function.", 
                    "Input consisted of one or two matrices of 100 nodes, depending on whether we wanted to",
                    "combine nodes within a single matrix or across two matrices (single or pair)."
                ),
                "compute_edges" = paste(
                    "Benchmark for the compute\\_edges function using the default supermarket environment.", 
                    "We distinguish between computing few or many edges (many_nodes = FALSE or TRUE) and between", 
                    "the presence of 10 segments that limit access in the environment."
                ), 
                "create_edges" = paste(
                    "Benchmark for the create\\_edges function using the default supermarket environment.", 
                    "We distinguish between computing few or many edges (many_nodes = FALSE or TRUE) and between",
                    "the presence of 10 segments that limit access in the environment."
                ), 
                "create_nodes" = paste(
                    "Benchmark for the create\\_nodes function using the default supermarket environment.", 
                    "We distinguish between computing few or many nodes (many_nodes = FALSE or TRUE)."
                ),
                "evaluate_edges" = paste(
                    "Benchmark for the evaluate\\_edges function using the default supermarket environment.", 
                    "We distinguish between computing few or many edges (many_nodes = FALSE or TRUE) and between",
                    "the presence of 10 segments that limit access in the environment."
                ),
                "prune_edges" = paste(
                    "Benchmark for the prune\\_edges function using the objects in the default supermarket environment.",
                    "We evaluated or pruned 100 segments provided in a matrix."
                ),
                "add_agent" = paste(
                    "Benchmark for the add\\_agent function.",
                    "A BaselineEuropean was added in the default supermarket environment.",
                    "We distinguish between goals being sorted according to distance to one another or being", 
                    "scattered randomly across the environment (sort\\_goals = TRUE or FALSE) and between parameters", 
                    "being drawn from a stochastic distribution or provided as is (individual\\_differences = TRUE", 
                    "or FALSE)."
                ),
                "create_initial_condition" = paste(
                    "Benchmark for the create\\_initial\\_condition function.",
                    "The supermarket environment is filled with 10 agents with 10 goals each."
                ),
                "simulate" = paste(
                    "Benchmark for the simulate function.",
                    "We run the simulation for 1 iteration, having 70 agents in the default supermarket",
                    "environment.",
                    "We distinguish between asking for precomputation of the edges (precompute_edges = TRUE or FALSE)",
                    "and between these edges making use of few or many nodes (many_nodes = FALSE or TRUE)."
                ),
                "create_agent_specifications" = paste(
                    "Benchmark for the create\\_agent\\_specifications function.",
                    "Function computes the simulation-relevant specifications of 100 agents.",
                    "We distinguish between using the R and Rcpp version of this function", 
                    "(cpp = FALSE or TRUE)."
                ),
                "predict_movement" = paste(
                    "Benchmark for the predict\\_movement function.",
                    "Movement is predicted for a single agent.",
                    "We distinguish between using the R and Rcpp version of this function (cpp = FALSE or TRUE)."
                ),
                "compute_utility_variables" = paste(
                    "Benchmark for the compute\\_utility\\_variables function.", 
                    "Does so for a single agent in the default supermarket environment, which", 
                    "contains 70 agents in total.", 
                    "We distinguish between using the R and Rcpp version of this function", 
                    "(cpp = FALSE or TRUE)."
                ),
                "utility" = paste(
                    "Benchmark for the utility function.",
                    "Does so for a single agent in the default supermarket environment, which contains 70 agents in total.",
                    "We distinguish between using a dataset or a trace for the computation as well as between",
                    "using the R or Rcpp verion of this function (cpp = FALSE or TRUE)."
                )
            )
        )
    )
}
