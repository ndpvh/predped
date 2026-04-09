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
#' @param kalman Logical. When \code{TRUE}, applies a constant-velocity Kalman
#' smoother (Cartesian state [x, y, vx, vy]) to each group's trajectory after
#' noise is added. Smoothed positions failing the reachability check revert to
#' their pre-smoothed values. Defaults to \code{FALSE}.
#' @param span Numeric. When non-\code{NULL}, applies a boxcar (sliding-window)
#' average to each group's trajectory. For each row with time \eqn{t_0}, the
#' window covers rows where \eqn{t_0 \le t < t_0 + \text{span}}. Averaged
#' positions failing the reachability check are left unchanged. Defaults to
#' \code{NULL}.
#' @param thin Integer. When non-\code{NULL}, retains every \code{thin}-th row
#' per group after all other processing. Defaults to \code{NULL}.
#' @param ekalman Logical. When \code{TRUE}, applies an Extended Kalman Filter
#' with a Rauch-Tung-Striebel backward smoother to each group's trajectory
#' after noise (and any \code{kalman} smoothing) has been applied. The state
#' vector is \eqn{[x, y, v, \theta]^\top} (position, speed, heading), and the
#' constant-velocity process model is nonlinear in \eqn{\theta}, hence the
#' extended formulation. The smoother updates the \code{x}, \code{y},
#' \code{speed}, and \code{orientation} columns. Smoothed positions failing the
#' reachability check revert to their pre-smoothed values. Defaults to
#' \code{FALSE}.
#' @param optimal Logical. Only used when \code{ekalman = TRUE}. When
#' \code{TRUE}, the \code{speed} and \code{orientation} columns already present
#' in \code{data} are treated as near-perfect measurements of the velocity
#' state at every time step (measurement noise \code{ekalman_state_noise}).
#' This is the \emph{oracle} mode: in a simulation context these columns
#' contain the true pre-noise speed and heading, so \code{optimal = TRUE}
#' represents an upper bound on filter performance. When \code{FALSE} (default),
#' speed and heading are inferred entirely from the noisy position differences.
#' @param ekalman_R Numeric \eqn{2 \times 2} matrix giving the position
#' measurement noise covariance used by the EKF. When \code{NULL} (default),
#' the variance is estimated from the data via second differences:
#' \eqn{\hat{\sigma}^2 = \mathrm{var}(\Delta^2 x) / 6}, which is unbiased under
#' the constant-velocity assumption. Supply an explicit matrix (e.g.\
#' \code{diag(c(0.031^2, 0.031^2))}) when the noise level is known.
#' @param ekalman_state_noise Numeric scalar. Near-zero measurement noise
#' variance applied to the speed and orientation state components when
#' \code{optimal = TRUE}. Smaller values pin those components more tightly to
#' the observed true values. Defaults to \code{1e-6}.
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
                   kalman = FALSE,
                   span = NULL,
                   thin = NULL,
                   ekalman = FALSE,
                   optimal = FALSE,
                   ekalman_R = NULL,
                   ekalman_state_noise = 1e-6,
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

    # Kalman smoothing: applied per group on the time-sorted data.
    # After smoothing, any position that is not reachable reverts to the
    # pre-Kalman value for that row.
    if (kalman) {
        groups_kf <- if (!is.null(.by)) unique(data[[.by]]) else list(NULL)
        for (g in groups_kf) {
            idx <- if (!is.null(.by)) which(data[[.by]] == g) else seq_len(nrow(data))
            x_pre <- data[[x_col]][idx]
            y_pre <- data[[y_col]][idx]
            smoothed <- .kalman_smooth(x_pre, y_pre, data[[t_col]][idx], ...)
            for (j in seq_along(idx)) {
                if (is_valid(smoothed[j, 1], smoothed[j, 2])) {
                    data[[x_col]][idx[j]] <- smoothed[j, 1]
                    data[[y_col]][idx[j]] <- smoothed[j, 2]
                } # else keep pre-Kalman value already in place
            }
        }
    }

    # Extended Kalman Filter + RTS smoother: applied per group on time-sorted data.
    # Smooths x, y, speed, and orientation jointly under a constant-velocity model
    # (constant speed and heading). Two modes:
    #   optimal = TRUE  — treats the 'speed' and 'orientation' columns as near-perfect
    #                     measurements of the velocity state at every step.
    #   optimal = FALSE — infers speed and heading from noisy position differences.
    # Positions failing the reachability check revert to their pre-smoothing values.
    if (ekalman) {
        has_speed <- "speed"       %in% names(data)
        has_ori   <- "orientation" %in% names(data)
        if (optimal && (!has_speed || !has_ori)) {
            warning(
                "optimal = TRUE requires 'speed' and 'orientation' columns in data. ",
                "Falling back to optimal = FALSE."
            )
            optimal <- FALSE
        }
        groups_ek <- if (!is.null(.by)) unique(data[[.by]]) else list(NULL)
        for (g in groups_ek) {
            idx   <- if (!is.null(.by)) which(data[[.by]] == g) else seq_len(nrow(data))
            x_pre <- data[[x_col]][idx]
            y_pre <- data[[y_col]][idx]
            t_pre <- data[[t_col]][idx]
            spd   <- if (optimal && has_speed) data[["speed"]][idx]       else NULL
            ori   <- if (optimal && has_ori)   data[["orientation"]][idx] else NULL

            res <- .ekalman_smooth(
                x               = x_pre,
                y               = y_pre,
                t               = t_pre,
                speed           = spd,
                orientation     = ori,
                R               = ekalman_R,
                optimal         = optimal,
                state_noise     = ekalman_state_noise
            )
            for (j in seq_along(idx)) {
                if (is_valid(res$x[j], res$y[j])) {
                    data[[x_col]][idx[j]] <- res$x[j]
                    data[[y_col]][idx[j]] <- res$y[j]
                    if (has_speed && !is.na(res$speed[j]))
                        data[["speed"]][idx[j]]       <- res$speed[j]
                    if (has_ori   && !is.na(res$orientation[j]))
                        data[["orientation"]][idx[j]] <- res$orientation[j]
                }
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

    # Thinning: keep every thin-th row per group, preserving all columns.
    # Done last so thinning applies to fully processed data.
    if (!is.null(thin)) {
        groups_th <- if (!is.null(.by)) unique(data[[.by]]) else list(NULL)
        data <- do.call("rbind", lapply(groups_th, function(g) {
            d <- if (!is.null(.by)) data[data[[.by]] == g, ] else data
            d[seq(1, nrow(d), by = thin), ]
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


# Kalman smoother (constant-velocity model) for a single agent's trajectory.
# x, y, t are numeric vectors of equal length, already sorted by time.
# Additional arguments (e.g. error) are passed through ...
# Returns an n x 2 matrix of smoothed [x, y] values.
.kalman_smooth <- function(x, y, t, error = 0.031^2, ...) {

    n <- length(x)
    if (n < 2) return(cbind(x, y))

    if (length(error) == 1) error <- rep(error, 2)

    # Precompute time differences and per-step speeds
    dt     <- c(0, diff(t))
    dxdt   <- c(0, diff(x) / diff(t))
    dydt   <- c(0, diff(y) / diff(t))

    # Estimate process noise variances (error-corrected)
    mean_dt2 <- mean(dt[-1])^2
    var_vx <- max(var(dxdt, na.rm = TRUE) - 2 * error[1] / mean_dt2, 1e-10)
    var_vy <- max(var(dydt, na.rm = TRUE) - 2 * error[2] / mean_dt2, 1e-10)

    # Transition matrix F(dt) and process noise W(dt) for constant-velocity model
    make_F <- function(d) {
        matrix(c(1, 0, d, 0,
                 0, 1, 0, d,
                 0, 0, 1, 0,
                 0, 0, 0, 1), nrow = 4, byrow = TRUE)
    }
    make_W <- function(d) {
        matrix(c(d^2*var_vx, 0,         d*var_vx, 0,
                 0,          d^2*var_vy, 0,        d*var_vy,
                 d*var_vx,   0,          var_vx,   0,
                 0,          d*var_vy,   0,         var_vy), nrow = 4, byrow = TRUE)
    }

    # Measurement matrix and noise covariance (Cholesky)
    H <- matrix(c(1,0,0,0, 0,1,0,0), nrow = 2, byrow = TRUE)
    R <- chol(diag(error))

    # Initial state and covariance
    x0 <- matrix(c(mean(x), mean(y), mean(dxdt, na.rm=TRUE), mean(dydt, na.rm=TRUE)), ncol = 1)
    P0 <- diag(c(var(x), var(y),
                 var(dxdt, na.rm=TRUE), var(dydt, na.rm=TRUE)))

    sx <- x
    sy <- y

    for (i in seq_len(n)) {
        # Predict
        if (i == 1) {
            xp <- x0;  Pp <- P0
        } else {
            F  <- make_F(dt[i])
            W  <- make_W(dt[i])
            xp <- F %*% x0
            Pp <- F %*% P0 %*% t(F) + W
        }

        # Innovate
        z   <- matrix(c(x[i], y[i]), ncol = 1)
        Rc  <- t(R) %*% R
        inn <- z - H %*% xp
        S   <- H %*% Pp %*% t(H) + Rc
        K   <- Pp %*% t(H) %*% solve(S)

        # Update
        x0 <- xp + K %*% inn
        P0 <- (diag(4) - K %*% H) %*% Pp

        sx[i] <- x0[1]
        sy[i] <- x0[2]
    }

    cbind(sx, sy)
}


# Extended Kalman Filter + Rauch-Tung-Striebel smoother for a single agent's
# trajectory. State vector: [x, y, v, theta]^T where v is speed (m/s) and
# theta is heading (radians internally; degrees for 'orientation' column I/O).
#
# Process model (nonlinear):
#   x_{k+1} = x_k + v_k * cos(theta_k) * dt
#   y_{k+1} = y_k + v_k * sin(theta_k) * dt
#   v_{k+1} = v_k
#   theta_{k+1} = theta_k
#
# Measurement model (linear):
#   optimal = FALSE: z = [x_noisy, y_noisy]
#   optimal = TRUE:  z = [x_noisy, y_noisy, v_true, theta_true]  (near-perfect)
#
# Returns a list with smoothed x, y, speed, orientation (degrees).
.ekalman_smooth <- function(x, y, t,
                             speed       = NULL,
                             orientation = NULL,
                             R           = NULL,
                             optimal     = FALSE,
                             state_noise = 1e-6) {

    n <- length(x)

    # Not enough points to smooth: return originals unchanged
    if (n < 3) {
        return(list(
            x           = x,
            y           = y,
            speed       = if (!is.null(speed))       speed       else rep(NA_real_, n),
            orientation = if (!is.null(orientation)) orientation else rep(NA_real_, n)
        ))
    }

    dts     <- diff(t)
    mean_dt <- mean(dts)

    # Estimate measurement noise from second differences when R not supplied.
    # Under a constant-velocity model, var(x[i+1] - 2x[i] + x[i-1]) = 6*sigma^2.
    if (is.null(R)) {
        sigma2_x <- max(var(diff(diff(x))) / 6, 1e-10)
        sigma2_y <- max(var(diff(diff(y))) / 6, 1e-10)
        R <- diag(c(sigma2_x, sigma2_y))
    }

    # Orientation column is in degrees; work in radians internally
    theta_rad <- if (!is.null(orientation)) orientation * pi / 180 else NULL

    # ── INITIALISATION ──────────────────────────────────────────────────────────
    if (optimal) {
        # Use true speed / orientation for initial state and Q estimation
        v0   <- speed[1]
        th0  <- theta_rad[1]
        P0   <- diag(c(R[1, 1], R[2, 2], state_noise, state_noise))
        q_v  <- max(var(diff(speed)),     1e-10)
        q_th <- max(var(diff(theta_rad)), 1e-10)

    } else {
        # Estimate speed and heading from noisy position differences
        dx     <- diff(x); dy <- diff(y)
        vx_est <- dx / dts; vy_est <- dy / dts
        v_est  <- pmax(sqrt(vx_est^2 + vy_est^2), 1e-6)
        th_est <- atan2(vy_est, vx_est)

        v0  <- v_est[1]
        th0 <- th_est[1]

        # Bias-corrected process noise: observation noise inflates empirical variance
        # var(diff(v_est)) ≈ var(diff(v_true)) + 4*sigma_pos/dt^2
        sigma_pos <- mean(diag(R))
        bias      <- 4 * sigma_pos / mean_dt^2
        q_v  <- max(var(diff(v_est))  - bias, 1e-10)
        q_th <- max(var(diff(th_est)) - bias, 1e-10)

        P0 <- diag(c(sigma_pos, sigma_pos,
                     max(var(v_est),  1e-10),
                     max(var(th_est), 1e-10)))
    }

    # Position process noise: estimated so that RTS smoother gain G ≈ 0.4–0.8
    if (optimal) {
        # Estimate from constant-velocity prediction residuals
        resid_x  <- x[-1] - (x[-n] + speed[-n] * cos(theta_rad[-n]) * dts)
        resid_y  <- y[-1] - (y[-n] + speed[-n] * sin(theta_rad[-n]) * dts)
        q_pos_x  <- max(var(resid_x) - R[1, 1], R[1, 1] * 0.25)
        q_pos_y  <- max(var(resid_y) - R[2, 2], R[2, 2] * 0.25)
    } else {
        q_pos_x <- R[1, 1]
        q_pos_y <- R[2, 2]
    }

    # ── MEASUREMENT SETUP ────────────────────────────────────────────────────────
    if (optimal) {
        # 4D measurement: [x, y, v, theta]; v and theta observed near-perfectly
        H     <- diag(4)
        R_use <- diag(c(diag(R), state_noise, state_noise))
    } else {
        # 2D measurement: [x, y] only
        H     <- cbind(diag(2), matrix(0, 2, 2))
        R_use <- R
    }

    # ── STORAGE FOR FORWARD PASS ─────────────────────────────────────────────────
    xf <- matrix(0, n, 4)          # filtered means  x_{k|k}
    Pf <- array(0, c(4, 4, n))    # filtered covars P_{k|k}
    xp <- matrix(0, n, 4)          # predicted means  x_{k|k-1}
    Pp <- array(0, c(4, 4, n))    # predicted covars P_{k|k-1}
    Fa <- array(0, c(4, 4, n - 1)) # Jacobians F_k (predict step k → k+1)

    # Prior at step 1 (before first observation)
    xp[1, ] <- c(x[1], y[1], v0, th0)
    Pp[,,1]  <- P0

    # ── FORWARD EKF PASS ─────────────────────────────────────────────────────────
    for (i in seq_len(n)) {
        x_curr <- matrix(xp[i, ], ncol = 1)
        P_curr <- Pp[,,i]

        # ── Update (fuse observation at step i) ──
        if (optimal) {
            z <- matrix(c(x[i], y[i], speed[i], theta_rad[i]), ncol = 1)
        } else {
            z <- matrix(c(x[i], y[i]), ncol = 1)
        }
        inn <- z - H %*% x_curr
        if (optimal) inn[4] <- atan2(sin(inn[4]), cos(inn[4]))  # wrap angle innovation

        S     <- H %*% P_curr %*% t(H) + R_use
        K     <- P_curr %*% t(H) %*% solve(S + diag(1e-12, nrow(S)))
        x_upd <- x_curr + K %*% inn
        x_upd[4] <- atan2(sin(x_upd[4]), cos(x_upd[4]))        # normalise heading
        P_upd <- (diag(4) - K %*% H) %*% P_curr
        P_upd <- 0.5 * (P_upd + t(P_upd))                      # enforce symmetry

        xf[i, ] <- x_upd
        Pf[,,i]  <- P_upd

        # ── Predict to step i+1 ──
        if (i < n) {
            dti <- t[i + 1] - t[i]
            vi  <- x_upd[3]
            thi <- x_upd[4]
            cth <- cos(thi); sth <- sin(thi)

            F_i <- matrix(c(
                1, 0, cth * dti, -vi * sth * dti,
                0, 1, sth * dti,  vi * cth * dti,
                0, 0, 1,          0,
                0, 0, 0,          1
            ), nrow = 4, byrow = TRUE)

            x_nxt    <- matrix(c(
                x_upd[1] + vi * cth * dti,
                x_upd[2] + vi * sth * dti,
                vi,
                thi
            ), ncol = 1)
            x_nxt[4] <- atan2(sin(x_nxt[4]), cos(x_nxt[4]))

            # Scale process noise proportionally to step size
            Q_i   <- diag(c(q_pos_x * dti / mean_dt,
                            q_pos_y * dti / mean_dt,
                            q_v     * dti / mean_dt,
                            q_th    * dti / mean_dt))
            P_nxt <- F_i %*% P_upd %*% t(F_i) + Q_i
            P_nxt <- 0.5 * (P_nxt + t(P_nxt))

            xp[i + 1, ] <- x_nxt
            Pp[,,i + 1]  <- P_nxt
            Fa[,,i]      <- F_i
        }
    }

    # ── BACKWARD RTS SMOOTHER ────────────────────────────────────────────────────
    xs <- xf          # initialise with filtered estimates
    Ps <- Pf

    for (i in (n - 1):1) {
        G_i <- tryCatch(
            Pf[,,i] %*% t(Fa[,,i]) %*% solve(Pp[,,i + 1] + diag(1e-12, 4)),
            error = function(e) matrix(0, 4, 4)
        )
        xs[i, ] <- xf[i, ] + G_i %*% (xs[i + 1, ] - xp[i + 1, ])
        Ps[,,i]  <- Pf[,,i] + G_i %*% (Ps[,,i + 1] - Pp[,,i + 1]) %*% t(G_i)
        Ps[,,i]  <- 0.5 * (Ps[,,i] + t(Ps[,,i]))
        xs[i, 4] <- atan2(sin(xs[i, 4]), cos(xs[i, 4]))          # normalise heading
    }

    list(
        x           = xs[, 1],
        y           = xs[, 2],
        speed       = pmax(xs[, 3], 0),       # speed is non-negative
        orientation = xs[, 4] * 180 / pi      # radians → degrees
    )
}
