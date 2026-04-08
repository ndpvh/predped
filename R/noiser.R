#' Add noise to data with reachability checking
#'
#' Adds measurement error to pedestrian trajectory data, optionally checking
#' whether each noised position is reachable within a
#' \code{\link[predped]{background}}. When a noised position falls inside an
#' obstacle or outside the room boundary, new noise is sampled and added to the
#' original (noiseless) position until a valid position is found or \code{ntry}
#' attempts are exhausted.
#'
#' Either \code{data} or \code{trace} must be supplied, but not both. When
#' \code{trace} is provided, the background is extracted automatically and
#' \code{\link[predped]{unpack_trace}} is called to obtain the data.
#'
#' @param data Dataframe containing at least columns for x, y, and time.
#' Mutually exclusive with \code{trace}.
#' @param trace List of \code{\link[predped]{state-class}} objects as returned
#' by \code{\link[predped]{simulate}}. When supplied, the background is
#' extracted from the trace and \code{\link[predped]{unpack_trace}} is called
#' to obtain the data. Mutually exclusive with \code{data}.
#' @param time_step Numeric denoting the time step in seconds, passed to
#' \code{\link[predped]{unpack_trace}} when \code{trace} is supplied. Defaults
#' to \code{0.5}. Ignored when \code{data} is supplied directly.
#' @param cols Named vector mapping the standard column names \code{"time"},
#' \code{"x"}, and \code{"y"} to the actual column names in \code{data}.
#' Defaults to \code{NULL}, assuming columns are already named \code{"x"},
#' \code{"y"}, and \code{"time"}.
#' @param .by String denoting the grouping variable (e.g. agent id). Defaults
#' to \code{NULL}.
#' @param model String or function denoting the measurement error model.
#' Built-in options are \code{"independent"} and \code{"temporal"}.
#' Defaults to \code{"temporal"}.
#' @param background Object of \code{\link[predped]{background-class}}. When
#' supplied, each noised position is checked for reachability. Invalid
#' positions are resampled. Defaults to \code{NULL}. Extracted automatically
#' when \code{trace} is supplied.
#' @param ntry Integer denoting the maximum number of resampling attempts per
#' invalid position before issuing a warning. Defaults to \code{100}.
#' @param ... Additional arguments passed to the measurement error model (e.g.
#' \code{covariance}, \code{transition}, \code{sampling_rate}).
#'
#' @return Noised \code{data.frame} with the same structure as \code{data}.
#'
#' @importFrom expm %^%
#'
#' @export
noiser <- function(data = NULL,
                   trace = NULL,
                   time_step = 0.5,
                   cols = NULL,
                   .by = NULL,
                   model = "temporal",
                   background = NULL,
                   ntry = 100,
                   span = NULL,
                   ...) {

    # Validate that exactly one of data or trace is provided
    if (is.null(data) && is.null(trace)) {
        stop("One of `data` or `trace` must be supplied.")
    }
    if (!is.null(data) && !is.null(trace)) {
        stop("`data` and `trace` are mutually exclusive. Supply only one.")
    }

    # If trace provided, extract background and unpack to data.frame
    if (!is.null(trace)) {
        background <- setting(trace[[1]])
        data <- unpack_trace(trace, time_step = time_step)
    }

    # Determine the actual x, y, and time column names in data
    x_col <- if (!is.null(cols) && "x"    %in% names(cols)) unname(cols["x"])    else "x"
    y_col <- if (!is.null(cols) && "y"    %in% names(cols)) unname(cols["y"])    else "y"
    t_col <- if (!is.null(cols) && "time" %in% names(cols)) unname(cols["time"]) else "time"

    # Record original row order so we can restore it after noising.
    # We must also sort by (group, time) to align x_orig/y_orig with the
    # row order that lapply produces internally.
    data$.orig_order <- seq_len(nrow(data))
    if (!is.null(.by)) {
        data <- data[order(match(data[[.by]], unique(data[[.by]])), data[[t_col]]), ]
    } else {
        data <- data[order(data[[t_col]]), ]
    }

    # Save original (noiseless) x and y before noising
    x_orig <- data[[x_col]]
    y_orig <- data[[y_col]]

    # Prepare the data (standardise column names, handle grouping)
    preparation <- .noiser_prepare(data, cols = cols, .by = .by)
    cols_prepared <- preparation$cols
    group         <- preparation$group
    data_prep     <- preparation$data

    if (!is.null(model)) {
        # Load the measurement error model
        if (is.character(model)) {
            error_fn <- function(x) .noise_models[[model]](x, ...)
        } else {
            error_fn <- function(x) model(x, ...)
        }

        # Apply the measurement model to each group
        data_prep <- lapply(
            seq_along(group),
            function(i) {
                data_i <- data_prep[data_prep$id == group[i], ]
                data_i <- data_i[order(data_i$time), ]
                return(error_fn(data_i))
            }
        )
        data_prep <- do.call("rbind", data_prep)
    }

    # Restore original column names
    data <- .noiser_finalize(data_prep, cols = cols_prepared, .by = .by)

    # Build reachability checker (returns TRUE everywhere when no background)
    if (!is.null(background)) {
        obj <- objects(background)
        shp <- shape(background)
        is_valid <- function(x, y) {
            coord <- matrix(c(x, y), nrow = 1, ncol = 2)
            colnames(coord) <- c("x", "y")
            in_shp     <- in_object(shp, coord)
            not_in_obj <- if (length(obj) == 0) TRUE else
                !any(sapply(obj, \(o) in_object(o, coord)))
            in_shp & not_in_obj
        }
    } else {
        is_valid <- function(x, y) TRUE
    }

    # For each row, check reachability of noised position and resample if needed
    # (only when a noise model was applied)
    for (i in if (!is.null(model)) seq_len(nrow(data)) else integer(0)) {
        if (!is_valid(data[[x_col]][i], data[[y_col]][i])) {
            tmp <- data.frame(x = x_orig[i], y = y_orig[i],
                              time = data[[t_col]][i])
            found <- FALSE
            for (try in seq_len(ntry)) {
                noised_tmp <- error_fn(tmp)
                if (is_valid(noised_tmp$x, noised_tmp$y)) {
                    data[[x_col]][i] <- noised_tmp$x
                    data[[y_col]][i] <- noised_tmp$y
                    found <- TRUE
                    break
                }
            }
            if (!found) {
                warning("Cannot find a reachable location, consider a larger value of ntry")
            }
        }
    }

    # Boxcar averaging: data is currently sorted by (group, time), which is
    # the correct order for windowing. Averaged position is checked for
    # reachability; if unreachable, the first row of the window is kept as-is.
    if (!is.null(span)) {
        groups_bc <- if (!is.null(.by)) unique(data[[.by]]) else list(NULL)
        data <- do.call("rbind", lapply(groups_bc, function(g) {
            d <- if (!is.null(.by)) data[data[[.by]] == g, ] else data
            dt <- min(diff(d[[t_col]]))
            w  <- max(1L, round(span / dt))
            rows <- lapply(seq_len(nrow(d)), function(i) {
                win_idx <- which(d[[t_col]] >= d[[t_col]][i] &
                                 d[[t_col]] <  d[[t_col]][i] + span)
                if (length(win_idx) < w) return(NULL)
                out <- d[win_idx[1], , drop = FALSE]
                avg_x <- mean(d[[x_col]][win_idx])
                avg_y <- mean(d[[y_col]][win_idx])
                if (is_valid(avg_x, avg_y)) {
                    out[[x_col]] <- avg_x
                    out[[y_col]] <- avg_y
                    out[[t_col]] <- mean(d[[t_col]][win_idx])
                }
                out
            })
            do.call("rbind", rows)
        }))
    }

    # Restore original row order
    data <- data[order(data$.orig_order), ]
    data$.orig_order <- NULL

    return(data)
}


# ------------------------------------------------------------------------------
# Internal helpers (not exported)
# ------------------------------------------------------------------------------

.noiser_prepare <- function(data, cols = NULL, .by = NULL) {

    if (!is.data.frame(data)) {
        stop("Argument `data` should contain a data.frame.")
    }

    if (!is.null(cols)) {
        if (!all(c("time", "x", "y") %in% names(cols))) {
            stop(paste(
                "Names of the `cols` argument does not contain the required names.",
                "Please make sure the labels are 'time', 'x', and 'y' or change",
                "your data's column names."
            ))
        }
    } else {
        cols <- c("time" = "time", "x" = "x", "y" = "y")
    }

    if (!is.null(.by)) {
        group <- unique(data[, .by])
        cols["id"] <- .by
    } else {
        data$id <- 1
        group <- 1
        cols["id"] <- "id"
    }

    # Identify extra columns not covered by cols
    extra_col_names <- setdiff(names(data), unname(cols))

    # Select and rename core columns, then cbind any extra columns
    data_core <- data[, unname(cols), drop = FALSE]
    colnames(data_core) <- names(cols)
    if (length(extra_col_names) > 0) {
        data_core <- cbind(data_core, data[, extra_col_names, drop = FALSE])
    }

    list("data" = data_core, "cols" = cols, "group" = group)
}


.noiser_finalize <- function(data, cols = NULL, .by = NULL) {

    # Extra columns are those not in the core set
    extra_col_names <- setdiff(names(data), names(cols))

    # Rename core columns back to their original names
    data_out <- data[, names(cols), drop = FALSE]
    colnames(data_out) <- unname(cols)

    if (is.null(.by)) {
        data_out[[cols["id"]]] <- NULL
    }

    # Attach extra columns unchanged
    if (length(extra_col_names) > 0) {
        data_out <- cbind(data_out, data[, extra_col_names, drop = FALSE])
    }

    return(data_out)
}


.noise_independent <- function(data,
                                mean = c(0, 0),
                                covariance = c(0.031^2, 0, 0, 0.027^2) |>
                                    matrix(nrow = 2, ncol = 2)) {

    if (length(mean) == 1) {
        mean <- rep(mean, 2)
    } else if (length(mean) > 2) {
        mean <- mean[1:2]
    }

    if (!is.matrix(covariance)) {
        stop("Provided covariance is not a matrix. Cannot add multivariate noise.")
    }
    if (all(dim(covariance) != c(2, 2))) {
        stop(paste(
            "Provided covariance matrix does not have the right dimensionality.",
            "A", dim(covariance)[1], "x", dim(covariance)[2],
            "matrix is provided instead of the required 2 x 2 matrix."
        ))
    }

    residuals <- MASS::mvrnorm(nrow(data), mu = mean, Sigma = covariance)
    if (!is.matrix(residuals)) residuals <- matrix(residuals, nrow = 1)
    data[, c("x", "y")] <- data[, c("x", "y")] + residuals
    return(data)
}


.noise_temporal <- function(data,
                             intercept = c(0, 0),
                             transition = c(0.925, 0.085, 0.085, 0.87) |>
                                 matrix(nrow = 2, ncol = 2),
                             covariance = c(0.015^2, 0, 0, 0.015^2) |>
                                 matrix(nrow = 2, ncol = 2),
                             sampling_rate = 6.13) {

    if (length(intercept) == 1) {
        intercept <- rep(intercept, 2)
    } else if (length(intercept) > 2) {
        intercept <- intercept[1:2]
    }

    if (!is.matrix(covariance)) {
        stop("Provided covariance is not a matrix. Cannot add multivariate noise.")
    }
    if (all(dim(covariance) != c(2, 2))) {
        stop(paste(
            "Provided covariance matrix does not have the right dimensionality.",
            "A", dim(covariance)[1], "x", dim(covariance)[2],
            "matrix is provided instead of the required 2 x 2 matrix."
        ))
    }
    if (!is.matrix(transition)) {
        stop("Provided transition parameter is not a matrix. Cannot add multivariate noise.")
    }
    if (all(dim(transition) != c(2, 2))) {
        stop(paste(
            "Provided transition matrix does not have the right dimensionality.",
            "A", dim(transition)[1], "x", dim(transition)[2],
            "matrix is provided instead of the required 2 x 2 matrix."
        ))
    }
    if (sampling_rate <= 0) {
        stop("Sampling rate is lower than or equal to 0, which is impossible.")
    }

    transition <- transition %^% (6.13 / sampling_rate)

    residuals <- MASS::mvrnorm(nrow(data), mu = c(0, 0), Sigma = covariance)
    if (!is.matrix(residuals)) residuals <- matrix(residuals, nrow = 1)
    epsilon   <- matrix(0, nrow = nrow(data), ncol = 2)

    for (i in seq_len(nrow(data))) {
        if (i == 1) {
            epsilon[i, ] <- solve(diag(2) - transition) %*% intercept +
                residuals[i, ]
        } else {
            epsilon[i, ] <- intercept +
                transition %*% as.numeric(epsilon[i - 1, ]) +
                residuals[i, ]
        }
    }

    data[, c("x", "y")] <- data[, c("x", "y")] + epsilon
    return(data)
}


.noise_models <- list(
    "independent" = .noise_independent,
    "temporal"    = .noise_temporal
)
