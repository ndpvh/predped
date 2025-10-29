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
#' 
#' @return Either a list of lists containing the raw execution times per 
#' benchmark case per function (if \code{summarize = FALSE}) or an HTML file 
#' summarizing the results of the benchmark (if \code{summarize = TRUE})
#' 
#' @example 
#' # Run benchmark for simulate with 1000 iterations
#' benchmark(
#'     x = "simulate",
#'     iterations = 1000
#' )
#' 
#' # Run benchmark for all functions with 10 iterations
#' benchmark(
#'    iterations = 10
#' )
#' 
#' @export 
benchmark <- function(x = NULL, 
                      iterations = 100,
                      summarize = TRUE,
                      progress = TRUE) {

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

    # Otherwise, create an HTML file that summarizes the results
    browser()
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
# SPECIFICATIONS OF THE BENCHMARK

# Load in the supermarket environment and its precomputed edges that serve as 
# input to some of these benchmarks
supermarket <- readRDS(file.path("tests", "testthat", "data", "supermarket.Rds"))
few_edges <- readRDS(file.path("tests", "testthat", "data", "few_edges_bench.Rds"))
many_edges <- readRDS(file.path("tests", "testthat", "data", "many_edges_bench.Rds"))

# Do the same for a dataset and parameters that will serve as input to some 
# benchmarks
params_bounded <- params_from_csv[["params_archetypes"]][1, -c(1:2)]
params_real <- to_unbounded(params_bounded, params_from_csv[["params_bounds"]])

data_1 <- readRDS(file.path("tests", "testthat", "data", "data_mll_benchmark.Rds"))
data_1 <- data_1[data_1$id == "eqdyv", ][1:100, ]
data_bench <- lapply(
    1:10, 
    function(i) {
        data_1$id <- paste0("person_", i)
        return(data_1)
    }
)
data_bench <- do.call("rbind", data_bench)

# Finally, load the initial condition for the simulate benchmark
inx <- readRDS(file.path("tests", "testthat", "data", "benchmark_inx.Rds"))
data_inx <- unpack_trace(list(inx), cpp = FALSE)

# Create a variable that provides arguments to the tests. These are precomputed,
# meaning that they should not influence the speed of the tests themselves
benchmark_args <- list(
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
    "time_series" = list(
        readRDS(file.path("tests", "testthat", "data", "trace_mll_bench.Rds"))[1:10]
    ),
    "unpack_trace" = list(
        readRDS(file.path("tests", "testthat", "data", "trace_mll_bench.Rds"))[1:10]
    ),

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
        data_bench,
        params_bounded,
        params_real
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
        inx
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
        subset(few_edges$edges_with_coords, select = -cost),
        subset(many_edges$edges_with_coords, select = -cost),
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
        inx@agents
    ),

    # update.R
    "create_agent_specifications" = list(
        lapply(1:100, \(x) agent(center = c(-1, 0), radius = 0.2, speed = 2, orientation = 0))
    ),
    "predict_movement" = list(
        agent(center = c(-1, 0), radius = 0.2, speed = 2, orientation = 0)
    ),

    # utility.R
    "compute_utility_variables" = list(
        inx,
        create_agent_specifications(inx@agents, cpp = FALSE),
        matrix(1, nrow = 33, ncol = 2),
        matrix(TRUE, nrow = 11, ncol = 3)
    ),
    "utility" = list(
        data_inx[data_inx$status == "move", ],
        params_bounded,
        inx,
        create_agent_specifications(inx@agents, cpp = FALSE),
        matrix(1, nrow = 33, ncol = 2),
        matrix(TRUE, nrow = 11, ncol = 3)
    )
)

# Create a variable with the benchmark tests in it
benchmark_test <- list(
    # background.R
    "compute_limited_access" = list(
        " | " = function() {
            return(
                compute_limited_access(
                    benchmark_args[["compute_limited_access"]][[1]]
                )
            )
        }
    ),
    "background" = list(
        "no limited_access" = function() {
            return(
                background(
                    shape = benchmark_args[["background"]][[1]],
                    objects = benchmark_args[["background"]][[2]]
                )
            )
        },
        "limited_access" = function() {
            return(
                background(
                    shape = benchmark_args[["background"]][[1]],
                    objects = benchmark_args[["background"]][[2]],
                    limited_access = benchmark_args[["background"]][[3]]
                )
            )
        }
    ),
    "limit_access" = list(
        "coordinate" = function() {
            return(
                limit_access(
                    benchmark_args[["limit_access"]][[1]], 
                    benchmark_args[["limit_access"]][[2]]
                )
            )
        },
        "agent" = function() {
            return(
                limit_access(
                    benchmark_args[["limit_access"]][[1]], 
                    benchmark_args[["limit_access"]][[3]]
                )
            )
        }
    ),

    # data.R
    "time_series" = list(
        "cpp = FALSE" = function() {
            return(
                time_series(
                    benchmark_args[["time_series"]][[1]], 
                    cpp = FALSE
                )
            )
        },
        "cpp = TRUE" = function() {
            return(
                time_series(
                    benchmark_args[["time_series"]][[1]], 
                    cpp = TRUE
                )
            )
        }
    ),
    "unpack_trace" = list(
        "cpp = FALSE" = function() {
            return(
                unpack_trace(
                    benchmark_args[["unpack_trace"]][[1]], 
                    cpp = FALSE
                )
            )
        },
        "cpp = TRUE" = function() {
            return(
                unpack_trace(
                    benchmark_args[["unpack_trace"]][[1]], 
                    cpp = TRUE
                )
            )
        }
    ),

    # general.R
    "line_line_intersection" = list(
        " | " = function() {
            return(
                line_line_intersection(
                    benchmark_args[["line_line_intersection"]][[1]],
                    benchmark_args[["line_line_intersection"]][[2]]
                )
            )
        }
    ),
    "perpendicular_orientation" = list(
        "polygon" = function() {
            return(
                perpendicular_orientation(
                    benchmark_args[["perpendicular_orientation"]][[1]],
                    benchmark_args[["perpendicular_orientation"]][[4]]
                )
            )
        },
        "rectangle" = function() {
            return(
                perpendicular_orientation(
                    benchmark_args[["perpendicular_orientation"]][[2]],
                    benchmark_args[["perpendicular_orientation"]][[4]]
                )
            )
        },
        "circle" = function() {
            return(
                perpendicular_orientation(
                    benchmark_args[["perpendicular_orientation"]][[3]],
                    benchmark_args[["perpendicular_orientation"]][[4]]
                )
            )
        }
    ),

    # goals.R
    "add_goal" = list(
        "polygon | middle_edge = FALSE" = function() {
            return(
                add_goal(
                    benchmark_args[["add_goal"]][[1]],
                    supermarket,
                    middle_edge = FALSE
                )
            )
        },
        "rectangle | middle_edge = FALSE" = function() {
            return(
                add_goal(
                    benchmark_args[["add_goal"]][[2]],
                    supermarket,
                    middle_edge = FALSE
                )
            )
        },
        "circle | middle_edge = FALSE" = function() {
            return(
                add_goal(
                    benchmark_args[["add_goal"]][[3]],
                    supermarket
                )
            )
        },
        "polygon | middle_edge = TRUE" = function() {
            return(
                add_goal(
                    benchmark_args[["add_goal"]][[1]],
                    supermarket,
                    middle_edge = FALSE
                )
            )
        },
        "rectangle | middle_edge = TRUE" = function() {
            return(
                add_goal(
                    benchmark_args[["add_goal"]][[2]],
                    supermarket,
                    middle_edge = FALSE
                )
            )
        },
        "circle | middle_edge = TRUE" = function() {
            return(
                add_goal(
                    benchmark_args[["add_goal"]][[3]],
                    supermarket
                )
            )
        }
    ),
    "change" = list(
        " | " = function() {
            return(
                change(
                    benchmark_args[["change"]][[1]], 
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
                    benchmark_args[["find_path"]][[1]],
                    benchmark_args[["find_path"]][[2]],
                    supermarket,
                    precomputed_edges = few_edges,
                    reevaluate = FALSE
                )
            )
        },
        "precomputed | many_nodes = TRUE | reevaluate = FALSE" = function() {
            return(
                find_path(
                    benchmark_args[["find_path"]][[1]],
                    benchmark_args[["find_path"]][[2]],
                    supermarket,
                    precomputed_edges = many_edges,
                    reevaluate = FALSE
                )
            )
        },
        "precomputed | many_nodes = FALSE | reevaluate = TRUE" = function() {
            return(
                find_path(
                    benchmark_args[["find_path"]][[1]],
                    benchmark_args[["find_path"]][[2]],
                    supermarket,
                    precomputed_edges = few_edges,
                    reevaluate = TRUE
                )
            )
        },
        "precomputed | many_nodes = TRUE | reevaluate = TRUE" = function() {
            return(
                find_path(
                    benchmark_args[["find_path"]][[1]],
                    benchmark_args[["find_path"]][[2]],
                    supermarket,
                    precomputed_edges = many_edges,
                    reevaluate = TRUE
                )
            )
        },
        "not precomputed | many_nodes = FALSE | " = function() {
            return(
                find_path(
                    benchmark_args[["find_path"]][[1]],
                    benchmark_args[["find_path"]][[2]],
                    supermarket,
                    precomputed_edges = NULL,
                    many_nodes = FALSE
                )
            )
        },
        "not precomputed | many_nodes = TRUE | " = function() {
            return(
                find_path(
                    benchmark_args[["find_path"]][[1]],
                    benchmark_args[["find_path"]][[2]],
                    supermarket,
                    precomputed_edges = NULL,
                    many_nodes = TRUE
                )
            )
        }
    ),
    "goal_stack" = list(
        " | " = function() {
            return(
                goal_stack(
                    benchmark_args[["goal_stack"]][[1]],
                    supermarket,
                    counter = benchmark_args[["goal_stack"]][[2]]
                )
            )
        }
    ),
    "goal" = list(
        " | " = function() {
            return(
                goal(
                    position = benchmark_args[["goal"]][[1]]
                )
            )
        }
    ),
    "interact" = list(
        " | " = function() {
            return(
                interact(
                    benchmark_args[["interact"]][[1]]
                )
            )
        }
    ),

    # likelihood.R
    "mll" = list(
        "transform = FALSE | cpp = FALSE | summed = FALSE" = function() {
            return(
                mll(
                    benchmark_args[["mll"]][[1]],
                    benchmark_args[["mll"]][[2]],
                    transform = FALSE,
                    cpp = FALSE,
                    summed = FALSE
                )
            )
        },
        "transform = TRUE | cpp = FALSE | summed = FALSE" = function() {
            return(
                mll(
                    benchmark_args[["mll"]][[1]],
                    benchmark_args[["mll"]][[3]],
                    transform = TRUE,
                    cpp = FALSE,
                    summed = FALSE
                )
            )
        },
        "transform = FALSE | cpp = TRUE | summed = FALSE" = function() {
            return(
                mll(
                    benchmark_args[["mll"]][[1]],
                    benchmark_args[["mll"]][[2]],
                    transform = FALSE,
                    cpp = TRUE,
                    summed = FALSE
                )
            )
        },
        "transform = TRUE | cpp = TRUE | summed = FALSE" = function() {
            return(
                mll(
                    benchmark_args[["mll"]][[1]],
                    benchmark_args[["mll"]][[3]],
                    transform = TRUE,
                    cpp = TRUE,
                    summed = FALSE
                )
            )
        },
        "transform = FALSE | cpp = FALSE | summed = TRUE" = function() {
            return(
                mll(
                    benchmark_args[["mll"]][[1]],
                    benchmark_args[["mll"]][[2]],
                    transform = FALSE,
                    cpp = FALSE,
                    summed = TRUE
                )
            )
        },
        "transform = TRUE | cpp = FALSE | summed = TRUE" = function() {
            return(
                mll(
                    benchmark_args[["mll"]][[1]],
                    benchmark_args[["mll"]][[3]],
                    transform = TRUE,
                    cpp = FALSE,
                    summed = TRUE
                )
            )
        },
        "transform = FALSE | cpp = TRUE | summed = TRUE" = function() {
            return(
                mll(
                    benchmark_args[["mll"]][[1]],
                    benchmark_args[["mll"]][[2]],
                    transform = FALSE,
                    cpp = TRUE,
                    summed = TRUE
                )
            )
        },
        "transform = TRUE | cpp = TRUE | summed = TRUE" = function() {
            return(
                mll(
                    benchmark_args[["mll"]][[1]],
                    benchmark_args[["mll"]][[3]],
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
                    benchmark_args[["agents_between_goal"]][[1]], 
                    benchmark_args[["agents_between_goal"]][[2]]
                )
            )
        }
    ),
    "compute_centers" = list(
        "cpp = FALSE" = function() {
            return(
                compute_centers(
                    benchmark_args[["compute_centers"]][[1]],
                    cpp = FALSE
                )
            )
        },
        "cpp = TRUE" = function() {
            return(
                compute_centers(
                    benchmark_args[["compute_centers"]][[1]],
                    cpp = TRUE
                )
            )
        }
    ),
    "moving_options" = list(
        "cpp = FALSE" = function() {
            return(
                moving_options(
                    benchmark_args[["moving_options"]][[1]],
                    benchmark_args[["moving_options"]][[2]],
                    supermarket,
                    benchmark_args[["moving_options"]][[3]],
                    cpp = FALSE
                )
            )
        },
        "cpp = TRUE" = function() {
            return(
                moving_options(
                    benchmark_args[["moving_options"]][[1]],
                    benchmark_args[["moving_options"]][[2]],
                    supermarket,
                    benchmark_args[["moving_options"]][[3]],
                    cpp = TRUE
                )
            )
        }
    ),
    "overlap_with_objects" = list(
        "cpp = FALSE" = function() {
            return(
                overlap_with_objects(
                    benchmark_args[["overlap_with_objects"]][[1]],
                    supermarket,
                    benchmark_args[["overlap_with_objects"]][[2]],
                    benchmark_args[["overlap_with_objects"]][[3]],
                    cpp = FALSE
                )
            )
        },
        "cpp = TRUE" = function() {
            return(
                overlap_with_objects(
                    benchmark_args[["overlap_with_objects"]][[1]],
                    supermarket,
                    benchmark_args[["overlap_with_objects"]][[2]],
                    benchmark_args[["overlap_with_objects"]][[3]],
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
                    benchmark_args[["add_nodes"]][[1]],
                    space_between = 0.5,
                    only_corners = FALSE
                )
            )
        },
        "rectangle | only_corners = FALSE" = function() {
            return(
                add_nodes(
                    benchmark_args[["add_nodes"]][[2]],
                    space_between = 0.5,
                    only_corners = FALSE
                )
            )
        },
        "circle | only_corners = FALSE" = function() {
            return(
                add_nodes(
                    benchmark_args[["add_nodes"]][[3]],
                    space_between = 0.5,
                    only_corners = FALSE
                )
            )
        },
        "polygon | only_corners = TRUE" = function() {
            return(
                add_nodes(
                    benchmark_args[["add_nodes"]][[1]],
                    space_between = 0.5,
                    only_corners = TRUE
                )
            )
        },
        "rectangle | only_corners = TRUE" = function() {
            return(
                add_nodes(
                    benchmark_args[["add_nodes"]][[2]],
                    space_between = 0.5,
                    only_corners = TRUE
                )
            )
        },
        "circle | only_corners = TRUE" = function() {
            return(
                add_nodes(
                    benchmark_args[["add_nodes"]][[3]],
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
                    benchmark_args[["enlarge"]][[1]],
                    0.5
                )
            )
        },
        "rectangle" = function() {
            return(
                enlarge(
                    benchmark_args[["enlarge"]][[2]],
                    0.5
                )
            )
        },
        "circle" = function() {
            return(
                enlarge(
                    benchmark_args[["enlarge"]][[3]],
                    0.5
                )
            )
        }
    ),
    "in_object" = list(
        "polygon | cpp = FALSE" = function() {
             return(
                in_object(
                    benchmark_args[["in_object"]][[1]],
                    benchmark_args[["in_object"]][[4]],
                    cpp = FALSE
                )
             )
        },
        "rectangle | cpp = FALSE" = function() {
             return(
                in_object(
                    benchmark_args[["in_object"]][[2]],
                    benchmark_args[["in_object"]][[4]],
                    cpp = FALSE
                )
             )
        },
        "circle | cpp = FALSE" = function() {
             return(
                in_object(
                    benchmark_args[["in_object"]][[3]],
                    benchmark_args[["in_object"]][[4]],
                    cpp = FALSE
                )
             )
        },
        "polygon | cpp = TRUE" = function() {
             return(
                in_object(
                    benchmark_args[["in_object"]][[1]],
                    benchmark_args[["in_object"]][[4]],
                    cpp = TRUE
                )
             )
        },
        "rectangle | cpp = TRUE" = function() {
             return(
                in_object(
                    benchmark_args[["in_object"]][[2]],
                    benchmark_args[["in_object"]][[4]],
                    cpp = TRUE
                )
             )
        },
        "circle | cpp = TRUE" = function() {
             return(
                in_object(
                    benchmark_args[["in_object"]][[3]],
                    benchmark_args[["in_object"]][[4]],
                    cpp = TRUE
                )
             )
        }
    ),
    "polygon" = list(
        " | " = function() {
            return(
                polygon(
                    points = benchmark_args[["polygon"]][[1]]
                )
            )
        }
    ),
    "rectangle" = list(
        " | " = function() {
            return(
                rectangle(
                    center = benchmark_args[["rectangle"]][[1]],
                    size = benchmark_args[["rectangle"]][[2]]
                )
            )
        }
    ),
    "circle" = list(
        " | " = function() {
            return(
                circle(
                    center = benchmark_args[["circle"]][[1]],
                    radius = benchmark_args[["circle"]][[2]]
                )
            )
        }
    ),
    "intersects" = list(
        "polygon | polygon" = function() {
            return(
                intersects(
                    benchmark_args[["intersects"]][[1]],
                    benchmark_args[["intersects"]][[1]]
                )
            )
        },
        "polygon | rectangle" = function() {
            return(
                intersects(
                    benchmark_args[["intersects"]][[1]],
                    benchmark_args[["intersects"]][[2]]
                )
            )
        },
        "polygon | circle" = function() {
            return(
                intersects(
                    benchmark_args[["intersects"]][[1]],
                    benchmark_args[["intersects"]][[3]]
                )
            )
        },
        "rectangle | polygon" = function() {
            return(
                intersects(
                    benchmark_args[["intersects"]][[2]],
                    benchmark_args[["intersects"]][[1]]
                )
            )
        },
        "rectangle | rectangle" = function() {
            return(
                intersects(
                    benchmark_args[["intersects"]][[2]],
                    benchmark_args[["intersects"]][[2]]
                )
            )
        },
        "rectangle | circle" = function() {
            return(
                intersects(
                    benchmark_args[["intersects"]][[2]],
                    benchmark_args[["intersects"]][[3]]
                )
            )
        },
        "circle | polygon" = function() {
            return(
                intersects(
                    benchmark_args[["intersects"]][[3]],
                    benchmark_args[["intersects"]][[1]]
                )
            )
        },
        "circle | rectangle" = function() {
            return(
                intersects(
                    benchmark_args[["intersects"]][[3]],
                    benchmark_args[["intersects"]][[2]]
                )
            )
        },
        "circle | circle" = function() {
            return(
                intersects(
                    benchmark_args[["intersects"]][[3]],
                    benchmark_args[["intersects"]][[3]]
                )
            )
        }
    ),
    "line_intersection" = list(
        "polygon" = function() {
            return(
                line_intersection(
                    benchmark_args[["line_intersection"]][[1]],
                    benchmark_args[["line_intersection"]][[4]]
                )
            )
        },
        "rectangle" = function() {
            return(
                line_intersection(
                    benchmark_args[["line_intersection"]][[2]],
                    benchmark_args[["line_intersection"]][[4]]
                )
            )
        },
        "circle" = function() {
            return(
                line_intersection(
                    benchmark_args[["line_intersection"]][[3]],
                    benchmark_args[["line_intersection"]][[4]]
                )
            )
        }
    ),
    "nodes_on_circumference" = list(
        "polygon | cpp = FALSE" = function() {
            return(
                nodes_on_circumference(
                    benchmark_args[["nodes_on_circumference"]][[1]],
                    cpp = FALSE
                )
            )
        },
        "rectangle | cpp = FALSE" = function() {
            return(
                nodes_on_circumference(
                    benchmark_args[["nodes_on_circumference"]][[2]],
                    cpp = FALSE
                )
            )
        },
        "circle | cpp = FALSE" = function() {
            return(
                nodes_on_circumference(
                    benchmark_args[["nodes_on_circumference"]][[3]],
                    cpp = FALSE
                )
            )
        },
        "polygon | cpp = TRUE" = function() {
            return(
                nodes_on_circumference(
                    benchmark_args[["nodes_on_circumference"]][[1]],
                    cpp = TRUE
                )
            )
        },
        "rectangle | cpp = TRUE" = function() {
            return(
                nodes_on_circumference(
                    benchmark_args[["nodes_on_circumference"]][[2]],
                    cpp = TRUE
                )
            )
        },
        "circle | cpp = TRUE" = function() {
            return(
                nodes_on_circumference(
                    benchmark_args[["nodes_on_circumference"]][[3]],
                    cpp = TRUE
                )
            )
        }
    ),
    "out_object" = list(
        "polygon | cpp = FALSE" = function() {
             return(
                out_object(
                    benchmark_args[["out_object"]][[1]],
                    benchmark_args[["out_object"]][[4]],
                    cpp = FALSE
                )
             )
        },
        "rectangle | cpp = FALSE" = function() {
             return(
                out_object(
                    benchmark_args[["out_object"]][[2]],
                    benchmark_args[["out_object"]][[4]],
                    cpp = FALSE
                )
             )
        },
        "circle | cpp = FALSE" = function() {
             return(
                out_object(
                    benchmark_args[["out_object"]][[3]],
                    benchmark_args[["out_object"]][[4]],
                    cpp = FALSE
                )
             )
        },
        "polygon | cpp = TRUE" = function() {
             return(
                out_object(
                    benchmark_args[["out_object"]][[1]],
                    benchmark_args[["out_object"]][[4]],
                    cpp = TRUE
                )
             )
        },
        "rectangle | cpp = TRUE" = function() {
             return(
                out_object(
                    benchmark_args[["out_object"]][[2]],
                    benchmark_args[["out_object"]][[4]],
                    cpp = TRUE
                )
             )
        },
        "circle | cpp = TRUE" = function() {
             return(
                out_object(
                    benchmark_args[["out_object"]][[3]],
                    benchmark_args[["out_object"]][[4]],
                    cpp = TRUE
                )
             )
        }
    ),
    "rng_point" = list(
        "polygon | middle_edge = FALSE" = function() {
            return(
                rng_point(
                    benchmark_args[["rng_point"]][[1]],
                    middle_edge = FALSE
                )
            )
        },
        "rectangle | middle_edge = FALSE" = function() {
            return(
                rng_point(
                    benchmark_args[["rng_point"]][[2]],
                    middle_edge = FALSE
                )
            )
        },
        "circle | middle_edge = FALSE" = function() {
            return(
                rng_point(
                    benchmark_args[["rng_point"]][[3]],
                    middle_edge = FALSE
                )
            )
        },
        "polygon | middle_edge = TRUE" = function() {
            return(
                rng_point(
                    benchmark_args[["rng_point"]][[1]],
                    middle_edge = TRUE
                )
            )
        },
        "rectangle | middle_edge = TRUE" = function() {
            return(
                rng_point(
                    benchmark_args[["rng_point"]][[2]],
                    middle_edge = TRUE
                )
            )
        },
        "circle | middle_edge = TRUE" = function() {
            return(
                rng_point(
                    benchmark_args[["rng_point"]][[3]],
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
                    benchmark_args[["plot"]][[1]]
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
                    benchmark_args[["plot"]][[2]]
                )
            )
        },
        "rectangle | optimize = FALSE" = function() {
            return(
                plot(
                    benchmark_args[["plot"]][[3]]
                )
            )
        },
        "circle | optimize = FALSE" = function() {
            return(
                plot(
                    benchmark_args[["plot"]][[4]]
                )
            )
        },
        "state | optimize = FALSE" = function() {
            return(
                plot(
                    benchmark_args[["plot"]][[5]],
                    optimize = FALSE
                )
            )
        },
        "agent | optimize = TRUE" = function() {
            return(
                plot(
                    benchmark_args[["plot"]][[1]]
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
                    benchmark_args[["plot"]][[2]]
                )
            )
        },
        "rectangle | optimize = TRUE" = function() {
            return(
                plot(
                    benchmark_args[["plot"]][[3]]
                )
            )
        },
        "circle | optimize = TRUE" = function() {
            return(
                plot(
                    benchmark_args[["plot"]][[4]]
                )
            )
        },
        "state | optimize = TRUE" = function() {
            return(
                plot(
                    benchmark_args[["plot"]][[5]],
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
                    benchmark_args[["adjust_edges"]][[1]],
                    benchmark_args[["adjust_edges"]][[2]],
                    supermarket, 
                    new_objects = benchmark_args[["adjust_edges"]][[3]],
                    precomputed_edges = few_edges,
                    reevaluate = FALSE
                )
            )
        },
        "many edges | reevaluate = FALSE" = function() {
            return(
                adjust_edges(
                    benchmark_args[["adjust_edges"]][[1]],
                    benchmark_args[["adjust_edges"]][[2]],
                    supermarket, 
                    new_objects = benchmark_args[["adjust_edges"]][[3]],
                    precomputed_edges = many_edges,
                    reevaluate = FALSE
                )
            )
        },
        "few edges | reevaluate = TRUE" = function() {
            return(
                adjust_edges(
                    benchmark_args[["adjust_edges"]][[1]],
                    benchmark_args[["adjust_edges"]][[2]],
                    supermarket, 
                    new_objects = benchmark_args[["adjust_edges"]][[3]],
                    precomputed_edges = few_edges,
                    reevaluate = TRUE
                )
            )
        },
        "many edges | reevaluate = TRUE" = function() {
            return(
                adjust_edges(
                    benchmark_args[["adjust_edges"]][[1]],
                    benchmark_args[["adjust_edges"]][[2]],
                    supermarket, 
                    new_objects = benchmark_args[["adjust_edges"]][[3]],
                    precomputed_edges = many_edges,
                    reevaluate = TRUE
                )
            )
        }
    ),
    "combine_nodes" = list(
        "single" = function() {
            return(
                combine_nodes(
                    benchmark_args[["combine_nodes"]][[1]]
                )
            )
        },
        "pair" = function() {
            return(
                combine_nodes(
                    benchmark_args[["combine_nodes"]][[1]],
                    benchmark_args[["combine_nodes"]][[1]]
                )
            )
        }
    ),
    "compute_edges" = list(
        "no limited access | many_nodes = FALSE" = function() {
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
                    benchmark_args[["compute_edges"]][[1]], 
                    many_nodes = FALSE
                )
            )
        },
        "no limited access | many_nodes = TRUE" = function() {
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
                    benchmark_args[["compute_edges"]][[1]], 
                    many_nodes = TRUE
                )
            )
        }
    ),
    "create_edges" = list(
        "no limited access | many_nodes = FALSE" = function() {
            return(
                create_edges(
                    benchmark_args[["create_edges"]][[1]],
                    benchmark_args[["create_edges"]][[2]],
                    supermarket, 
                    many_nodes = FALSE
                )
            )
        },
        "limited access | many_nodes = FALSE" = function() {
            return(
                create_edges(
                    benchmark_args[["create_edges"]][[1]],
                    benchmark_args[["create_edges"]][[2]],
                    benchmark_args[["create_edges"]][[3]], 
                    many_nodes = FALSE
                )
            )
        },
        "no limited access | many_nodes = TRUE" = function() {
            return(
                create_edges(
                    benchmark_args[["create_edges"]][[1]],
                    benchmark_args[["create_edges"]][[2]],
                    supermarket, 
                    many_nodes = TRUE
                )
            )
        },
        "limited access | many_nodes = TRUE" = function() {
            return(
                create_edges(
                    benchmark_args[["create_edges"]][[1]],
                    benchmark_args[["create_edges"]][[2]],
                    benchmark_args[["create_edges"]][[3]], 
                    many_nodes = TRUE
                )
            )
        }
    ),
    "create_nodes" = list(
        "many_nodes = FALSE" = function() {
            return(
                create_edges(
                    benchmark_args[["create_edges"]][[1]],
                    benchmark_args[["create_edges"]][[2]],
                    supermarket, 
                    many_nodes = FALSE
                )
            )
        },
        "many_nodes = TRUE" = function() {
            return(
                create_edges(
                    benchmark_args[["create_edges"]][[1]],
                    benchmark_args[["create_edges"]][[2]],
                    supermarket, 
                    many_nodes = TRUE
                )
            )
        }
    ),
    "evaluate_edges" = list(
        "no limited access | few nodes" = function() {
            return(
                evaluate_edges(
                    benchmark_args[["evaluate_edges"]][[1]],
                    supermarket,
                    0.5
                )
            )
        },
        "limited access | few nodes" = function() {
            return(
                evaluate_edges(
                    benchmark_args[["evaluate_edges"]][[1]],
                    benchmark_args[["evaluate_edges"]][[3]],
                    0.5
                )
            )
        },
        "no limited access | many nodes" = function() {
            return(
                evaluate_edges(
                    benchmark_args[["evaluate_edges"]][[2]],
                    supermarket,
                    0.5
                )
            )
        },
        "limited access | many nodes" = function() {
            return(
                evaluate_edges(
                    benchmark_args[["evaluate_edges"]][[2]],
                    benchmark_args[["evaluate_edges"]][[3]],
                    0.5
                )
            )
        }
    ),
    "prune_edges" = list(
        " | " = function() {
            return(
                prune_edges(
                    benchmark_args[["prune_edges"]][[1]],
                    benchmark_args[["prune_edges"]][[2]]
                )
            )
        }
    ),

    # simulate.R
    "add_agent" = list(
        "sort_goals = FALSE | individual_differences = FALSE" = function() {
            return(
                add_agent(
                    benchmark_args[["add_agent"]][[1]],
                    sort_goals = FALSE,
                    individual_differences = FALSE
                )
            )
        },
        "sort_goals = TRUE | individual_differences = FALSE" = function() {
            return(
                add_agent(
                    benchmark_args[["add_agent"]][[1]],
                    sort_goals = TRUE,
                    individual_differences = FALSE
                )
            )
        },
        "sort_goals = FALSE | individual_differences = TRUE" = function() {
            return(
                add_agent(
                    benchmark_args[["add_agent"]][[1]],
                    sort_goals = FALSE,
                    individual_differences = TRUE
                )
            )
        },
        "sort_goals = TRUE | individual_differences = TRUE" = function() {
            return(
                add_agent(
                    benchmark_args[["add_agent"]][[1]],
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
                    benchmark_args[["create_initial_condition"]][[1]],
                    benchmark_args[["create_initial_condition"]][[2]],
                    goal_number = benchmark_args[["create_initial_condition"]][[3]]
                )
            )
        }
    ),
    "simulate" = list(
        "precompute_edges = FALSE | many_nodes = FALSE" = function() {
            return(
                capture.output(
                    simulate(
                        benchmark_args[["simulate"]][[1]],
                        iterations = benchmark_args[["simulate"]][[2]],
                        max_agents = 70,
                        initial_agents = benchmark_args[["simulate"]][[3]],
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
                        benchmark_args[["simulate"]][[1]],
                        iterations = benchmark_args[["simulate"]][[2]],
                        max_agents = 70,
                        initial_agents = benchmark_args[["simulate"]][[3]],
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
                        benchmark_args[["simulate"]][[1]],
                        iterations = benchmark_args[["simulate"]][[2]],
                        max_agents = 70,
                        initial_agents = benchmark_args[["simulate"]][[3]],
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
                        benchmark_args[["simulate"]][[1]],
                        iterations = benchmark_args[["simulate"]][[2]],
                        max_agents = 70,
                        initial_agents = benchmark_args[["simulate"]][[3]],
                        precompute_edges = TRUE,
                        many_nodes = TRUE
                    )
                )
            )
        }
    ),

    # update.R
    "create_agent_specifications" = list(
        "cpp = FALSE" = function() {
            return(
                create_agent_specifications(
                    benchmark_args[["create_agent_specifications"]][[1]],
                    cpp = FALSE
                )
            )
        },
        "cpp = TRUE" = function() {
            return(
                create_agent_specifications(
                    benchmark_args[["create_agent_specifications"]][[1]],
                    cpp = TRUE
                )
            )
        }
    ),
    "predict_movement" = list(
        "cpp = FALSE" = function() {
            return(
                predict_movement(
                    benchmark_args[["predict_movement"]][[1]],
                    cpp = FALSE
                )
            )
        },
        "cpp = TRUE" = function() {
            return(
                predict_movement(
                    benchmark_args[["predict_movement"]][[1]],
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
                    benchmark_args[["compute_utility_variables"]][[1]]@agents[[3]],
                    benchmark_args[["compute_utility_variables"]][[1]],
                    benchmark_args[["compute_utility_variables"]][[1]]@setting,
                    benchmark_args[["compute_utility_variables"]][[2]],
                    benchmark_args[["compute_utility_variables"]][[3]],
                    benchmark_args[["compute_utility_variables"]][[4]],
                    cpp = FALSE
                )
            )
        },
        "cpp = TRUE" = function() {
            return(
                compute_utility_variables(
                    benchmark_args[["compute_utility_variables"]][[1]]@agents[[3]],
                    benchmark_args[["compute_utility_variables"]][[1]],
                    benchmark_args[["compute_utility_variables"]][[1]]@setting,
                    benchmark_args[["compute_utility_variables"]][[2]],
                    benchmark_args[["compute_utility_variables"]][[3]],
                    benchmark_args[["compute_utility_variables"]][[4]],
                    cpp = TRUE
                )
            )
        }
    ),
    "utility" = list(
        "data | cpp = FALSE" = function() {
            return(
                utility(
                    benchmark_args[["utility"]][[1]][1, ],
                    benchmark_args[["utility"]][[2]],
                    cpp = FALSE
                )
            )
        },
        "trace | cpp = FALSE" = function() {
            return(
                utility(
                    benchmark_args[["utility"]][[3]]@agents[[1]],
                    benchmark_args[["utility"]][[3]],
                    benchmark_args[["utility"]][[3]]@setting,
                    benchmark_args[["utility"]][[4]],
                    benchmark_args[["utility"]][[5]],
                    benchmark_args[["utility"]][[6]],
                    cpp = FALSE
                )
            )
        },
        "data | cpp = TRUE" = function() {
            return(
                utility(
                    benchmark_args[["utility"]][[1]][1, ],
                    benchmark_args[["utility"]][[2]],
                    cpp = TRUE
                )
            )
        },
        "trace | cpp = TRUE" = function() {
            return(
                utility(
                    benchmark_args[["utility"]][[3]]@agents[[1]],
                    benchmark_args[["utility"]][[3]],
                    benchmark_args[["utility"]][[3]]@setting,
                    benchmark_args[["utility"]][[4]],
                    benchmark_args[["utility"]][[5]],
                    benchmark_args[["utility"]][[6]],
                    cpp = TRUE
                )
            )
        }
    )
)
                           
# Create a variable that tells us what the hierarchy between the functions is
benchmark_hierarchy <- list(
    "compute_limited_access" = "background.R", 
    "background" = "background.R",
    "limit_access" = "background.R",

    "time_series" = "data.R",
    "unpack_trace" = "data.R",

    "line_line_intersection" = "general.R",
    "perpendicular_orientation" = "general.R",

    "add_goal" = "goals.R",
    "change" = "goals.R",
    "find_path" = "goals.R",
    "goal_stack" = "goals.R",
    "goal" = "goals.R",
    "interact" = "goals.R",

    "mll" = "likelihood.R",

    "agents_between_goal" = "moving_options.R",
    "compute_centers" = "moving_options.R",
    "moving_options" = "moving_options.R",

    "add_nodes" = "objects.R",
    "enlarge" = "objects.R",
    "in_object" = "objects.R",
    "polygon" = "objects.R",
    "rectangle" = "objects.R",
    "circle" = "objects.R",
    "intersects" = "objects.R",
    "nodes_on_circumference" = "objects.R",
    "out_object" = "objects.R",
    "rng_point" = "objects.R",

    "plot" = "plot.R",

    "adjust_edges" = "routing.R",
    "combine_nodes" = "routing.R",
    "compute_edges" = "routing.R",
    "create_edges" = "routing.R",
    "create_nodes" = "routing.R",
    "evaluate_edges" = "routing.R",
    "prune_edges" = "routing.R",

    "add_agent" = "simulate.R",
    "create_initial_condition" = "simulate.R",
    "simulate" = "simulate.R",

    "create_agent_specifications" = "update.R",
    "predict_movement" = "update.R",

    "compute_utility_variables" = "utility.R",
    "utility" = "utility.R"
)
