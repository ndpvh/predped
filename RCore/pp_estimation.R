##############################################################################-
# This file contains all functions related to estimation. 
##############################################################################-


# Prepare traces ----------------------------------------------------------

prepUtility <- function(state, N, objects) {
  
  # Goal Angle
  GAs <- sapply(1:N, function(n) {
    list(destinationAngle(a = state$a[n], p1 = state$p[n, , drop = FALSE], 
                          P1 = state$P[n, , drop = FALSE]) / 90)
  })
  
  # Interpersonal Distance
  IDs <- sapply(1:N, function(n) {
    list(predClose(n, p1 = state$p[n, , drop = FALSE], a1 = state$a[n], 
                   p2=state$p, r = state$r, centres = state$centres[[n]], 
                   p_pred = state$p_pred, objects = objects))
  })
  
  # Blocked Angle
  BAs <- sapply(1:N, function(n) {
    list(blockedAngle(n, state, p_pred = state$p_pred, objects = objects))
  })
  
  # Follow the Leader
  FLs <- sapply(1:N, function(n) {
    list(getLeaders(n, state, centres = state$centres[[n]], objects = objects))
  })
  
  # Walk Beside
  WBs <- sapply(1:N, function(n) {
    list(getBuddy(n, group = state$group, a = state$a, p_pred = state$p_pred, 
                  centres = state$centres, objects = objects, state = state))
  })
  
  # Add pre-computed parts to state
  state <- c(state, list(GA = GAs, ID = IDs, BA = BAs, FL = FLs, WB = WBs))
  return(state)
}

prepSimTrace <- function(sim_trace, constant = NULL) {

  get_state <- function(state,objects) {
    # # @CT Problem for estimation if cell = -1, stop and turn around
    # # For now, change -1 cells to 0
    # state$cell[state$cell == -1] <- 0
    
    # Number of pedestrians in state
    N <- dim(state$p)[1]
    
    # Determine current goal for each pedestrian
    current_goals <- lapply(state$P, function(ped_goal) {
      ped_goal[attr(ped_goal, "i"), c("x", "y")]
    })
    
    # Transform list to matrix
    current_goals <- matrix(unlist(current_goals), ncol = 2,
                            dimnames = list(names(state$P), c("x", "y")))
    
    # Determine current distance to goal for each pedestrian
    current_dist <- lapply(state$P, function(ped_goal) {
      ped_goal[attr(ped_goal, "i"), "dist"]
    })
    
    # Transform list to vector
    current_dist <- unlist(current_dist)
    
    # Compute centres
    centres <- sapply(1:N, function(n) {
      list(c_vd(1:33, p1 = state$p[n, ], v1 = state$v[n], a1 = state$a[n]))
    })
    
    # Compute oks
    oks <- sapply(1:N, function (n) {
      list(okObject(n, objects, state, centres[[n]]))
    })
    
    # Predict positions pedestrians in next iteration
    p_pred <- predictPed(state$p, state$v, state$a, state$cell)
    
    # Save P, d, centres and p_pred to state
    state$P <- current_goals
    state$d <- current_dist
    state$centres <- centres
    state$p_pred <- p_pred
    state$ok <- oks
    
    # Pre-compute utility
    state <- prepUtility(state, N, objects)
    
    # Remove unnecessary components from state
    state$bads <- NULL
    state$a <- NULL
    state$P <- NULL
    state$centres <- NULL
    state$p_pred <- NULL
    state$r <- NULL
    
    # State contains everything needed to pre-compute utility
    return(state)
  }
  
  trace <- lapply(sim_trace,get_state,objects=attr(sim_trace, "space")$objects)
                
  # Add alpha and nests attributes
  attr(trace, "alpha") <- attr(sim_trace, "alpha")
  attr(trace, "nests") <- attr(sim_trace, "nests")
  
  # pMat of all subjects
  pMat <- do.call(rbind, lapply(trace, function (x) {
    x$pMat
  }))
  pMat <- pMat[unique(row.names(pMat)), ] 
  # Change to natural
  pMat <- t(apply(pMat, 1, toNatural)) 
  # Add constants 
  if (!is.null(constant)) {
    n_const <- length(constant)
    pMat <- cbind(matrix(rep(constant, each = dim(pMat)[1]), ncol = n_const), 
                  pMat)
    colnames(pMat)[1:n_const] <- names(constant)
  }
  attr(trace, "pMat") <- pMat
  
  # Add subject and parameter names
  sp_names <- dimnames(attr(trace, "pMat"))
  attr(trace, "subject_names") <- sp_names[[1]]
  attr(trace, "param_names") <- sp_names[[2]]
  
  # Add type of trace as attribute
  attr(trace, "elements") <- "iterations"
  
  return(trace)
}

getSubjects <- function(trace) {
  # Check if trace is iteration based
  if (!(attr(trace, "elements") == "iterations")) {
    stop("Please provide prepped iteration based trace.")
  }
  
  # Converts trace to subject data structure
  getSubject <- function(state, s) {
    
    # Check if subject in iteration
    n <- which(row.names(state$p) == s)
    if (length(n) > 0) {
      out <- state
      
      # Only save information of subject in state
      out$cell <- state$cell[n]
      out$ok <- state$ok[[n]]
      out$GA <- state$GA[[n]]
      out$ID <- state$ID[[n]]
      out$BA <- state$BA[[n]]
      out$FL <- state$FL[[n]]
      out$WB <- state$WB[[n]]
      out$n <- n
    } else {
      out <- NULL
    }
    
    return(out)
  }
  
  # Create list of subjects
  snams <- attr(trace, "subject_names")
  out <- vector(mode = "list", length = length(snams))
  names(out) <- snams
  
  # Fill list with trace per subject
  for (s in snams) {
    tmp <- lapply(trace, getSubject, s = s)
    
    # Only keep iterations where subject was present
    out[[s]] <- tmp[!unlist(lapply(tmp, is.null))]
  }
  
  # Take over all attributes from trace (alpha, nests, pMat, ps.names)
  attributes(out) <- attributes(trace)
  
  # Set type of trace to subjects
  attr(out, "elements") <- "subjects"
  
  return(out)
}

# # Example
# simTrace <- readRDS("Experiments/Escience/Data/3-play/play_escience_m01s01p01r001.RDS")
# Itrace <- prepSimTrace(simTrace)
# Strace <- getSubjects(Itrace)

# Overall utility ---------------------------------------------------------

# Model utility
utility <- function(p, n, state, P_n = NULL, p_pred = NULL, centres = NULL, 
                    objects = list(), ok = NULL, iInfo = NULL, 
                    precomputed = FALSE, subject = TRUE) {
  
  # Pre-compute
  if (!precomputed) {
    PS <- dist1(state$p[n, ], state$P[[n]][attr(state$P[[n]], "i"), 1:2, 
                                           drop = FALSE])
    GA <- destinationAngle(state$a[n], state$p[n, , drop = FALSE], 
                           P_n[n, , drop = FALSE]) / 90
    ID <- predClose(n, p1 = state$p[n, , drop = FALSE], a1 = state$a[n], 
                    p2 = state$p, r = state$r, centres, p_pred, 
                    objects = objects)
    BA <- blockedAngle(n, state, p_pred, objects)
    FL <- getLeaders(n, state, centres, objects)
    WB <- getBuddy(n, group = state$group, a = state$a, p_pred, centres, 
                   objects, state = state)
    
    # Subject based
  } else if (subject) {
    ok <- state$ok
    PS <- state$d[n]
    GA <- state$GA
    ID <- state$ID
    BA <- state$BA
    FL <- state$FL
    WB <- state$WB
    # Iteration based
  } else {
    ok <- state$ok[[n]]
    PS <- state$d[n]
    GA <- state$GA[[n]]
    ID <- state$ID[[n]]
    BA <- state$BA[[n]]
    FL <- state$FL[[n]]
    WB <- state$WB[[n]]
  }
  # Utility sum
  out <- psUtility(p, state$v[n], PS) +
    gaUtility(p, GA) + 
    caUtility(p) +
    idUtility(p, n, ID, ok, group = state$group) + 
    baUtility(p, BA) +
    flUtility(p, FL) +
    wbUtility(p, WB)
  
  # Stop baseline (set by gwUtility) and scaling
  c(-p["bS"], out) / p["rU"]
}

# # Example
# stateI <- Itrace[[13]]
# p <- attr(Itrace, "pMat")
# n <- 1
# VI <- utility(p["A_1", ], n, stateI, precomputed = TRUE, subject = FALSE)
# 
# stateS <- Strace[[1]][[13]] 
# p <- attr(Strace, "pMat")
# n <- 1
# VS <- utility(p["A_1", ], n, stateS, precomputed = TRUE, subject = TRUE)
# 
# # Check if 
# all(VI == VS)

pCNL <- function(cell, V, muM = rep(1, length(nests)), nests, alpha, mu = 1,
                 cellNest = cbind(
                   t(matrix(c(c(1, 5),           # cell 0: Central, dec
                              rep(2:3, 5),       # cell 1-5: NonCentral, acc
                              c(1, 3),           # cell 6: Central, acc
                              rep(2:3, 5),       # cell 7-11: NonCentral, acc
                              rep(c(2, 4), 5),   # cell 12-16: NonCentral, const
                              c(1, 4),           # cell 17: Central, const
                              rep(c(2, 4), 5),   # cell 18-22: NonCentral, const
                              rep(c(2, 5), 5),   # cell 23-27: NonCentral, dec
                              c(1, 5),           # cell 28: Central, dec
                              rep(c(2, 5), 5)),  # cell 29-33: NonCentral, dec
                            nrow = 2)),
                   # Index cell in NonCentral/Central and acc/const/dec nest
                   cbind(c(1, 1:5, 2, 6:15, 3, 16:25, 4, 26:30), 
                         c(1, 1:11, 1:11, 2:12)))) {
  
  # Probability of alternatives within nests
  pAinNest <- function(Vlist, nests, alpha, muM) {
    pim <- nests
    for (m in 1:length(nests)) { # alternatives in each nest
      tmp <- alpha[[m]] * exp(muM[m] * Vlist[[m]])  
      bad <- tmp == Inf # Overflow
      if (any(bad)) tmp <- ifelse(bad, 1, 0)
      if (all(tmp == 0)) {
        pim[[m]] <- tmp 
      } else {
        pim[[m]] <- tmp / sum(tmp)
      }
    }
    return(pim)
  }
  
  # Probability of nest m
  pNest <- function(Vlist, nests, alpha, mu, muM) {
    mu_muM <- mu / muM
    tmp <- sapply(1:length(nests), function(m) {
      (sum(alpha[[m]] * exp(muM[m] * Vlist[[m]])))^mu_muM[m]
    })
    bad <- tmp == Inf # Overflow
    if (any(bad)) {
      tmp <- ifelse(bad, 1, 0)
    }
    if (all(tmp == 0)) {
      tmp 
    } else {
      tmp / sum(tmp)
    }
  }
  
  # Add 1 to cells if there is a cell for stopping, 0 can't be used for index
  if (any(unlist(nests) == 0)) {
    nests <- lapply(nests, function(x) {
      x + 1
    })
    cell <- cell + 1
  }
  
  # Nest list with utility of cells in nest
  Vlist <- lapply(nests, function(x) {
    V[x]
  })
  
  # Set largest V to zero to avoid numerical issues
  maxV <- max(unlist(lapply(Vlist, max)))
  Vlist <- lapply(Vlist, function(x) {
    x - maxV
  })
  
  # Probability of nest
  pN <- pNest(Vlist, nests, alpha, mu, muM)
  
  # Probability of alternative in nest
  pAN <- pAinNest(Vlist, nests, alpha, muM)
  
  # pAN * pN [NonCentral/Central] + pAN * pN [acc/const/dec]
  pAN[[cellNest[[cell, 1]]]][cellNest[cell, 3]] *    # [[ind nest]][ind cell]
    pN[cellNest[cell, 1]] +                          # [ind nest]
    pAN[[cellNest[[cell, 2]]]][cellNest[cell, 4]] *  # [[ind nest]][ind cell]
    pN[cellNest[cell, 2]]                            # [ind nest]
}

# # Example
# nests <- attr(Itrace, "nests")
# alpha <- attr(Itrace, "alpha")
# pCNL(stateI$cell[n], VI, muM = getmuM(p[1, ]), nests, alpha)
# pCNL(stateS$cell, VS, muM = getmuM(p[1, ]), nests, alpha)


# Estimation --------------------------------------------------------------

# Compute likelihood of one iteration or subject state
like_state <- function(state, p, n, nests, alpha, 
                       elements = c("iterations", "subjects")) {
  # Get parameter vector participant n
  p_n <- p[names(state$v)[n], ]
  
  # Compute utility
  if (elements == "iterations") {
    V <- utility(p_n, n, state, precomputed = TRUE, subject = FALSE)
    cell <- state$cell[n]
  } else if (elements == "subjects") {
    V <- utility(p_n, n, state, precomputed = TRUE, subject = TRUE)
    cell <- state$cell
  }
  
  # Compute probability of picking chosen cell
  prob <- pCNL(cell, V, muM = getmuM(p_n), nests, alpha)
  return(prob)
}

# Computes the likelihood of a list of iteration or subject states
like_states <- function(trace, p, elements = c("iterations", "subjects"), 
                        nests, alpha) {
  
  lhoods <- lapply(trace, function(element) {
    # If trace contains iterations as elements
    if (elements == "iterations") {
      # Compute likelihood for each subject in iteration
      iteration <- sapply(1:length(element$v), function(n) {
        like_state(element, p, n, nests, alpha, elements = elements)
      })
      return(iteration)
      # If trace contains subjects as elements
    } else if (elements == "subjects") {
      # Compute likelihood for each iteration of subject
      subject <- lapply(element, function(state) {
        like_state(state, p, n = state$n, nests, alpha, elements = elements)
      })
      return(subject)
    }
  })
  
  return(lhoods)
}

# Computes sum log likelihood, p on reals, constant (on natural) added in # @CT changed p to natural
msumlogLike <- function(p, prepped_trace, cores = 2, minLike = 1e-10, 
                        mult = -1) { 
  
  # Create matrix to divide states over cores
  imat <- suppressWarnings(matrix(1:length(prepped_trace), ncol = cores))
  # Remove duplicated assignments
  imat[duplicated(as.vector(imat))] <- NA 
  
  # Create lists of prepped_trace, collections of iterations to spread over cores
  prepped_traces <- apply(imat, 2, function (x) {
    prepped_trace[x[!is.na(x)]]
  })
  
  # Spread iterations or subjects over cores and compute likelihood
  out <- mclapply(prepped_traces, like_states, p = p, 
                  elements = attr(prepped_trace, "elements"),
                  nests = attr(prepped_trace, "nests"),
                  alpha = attr(prepped_trace, "alpha"),
                  mc.cores = cores)
  
  # Compute summed log likelihood
  mult * sum(log(pmax(unlist(out), minLike)))
}

# msumlogLike(p, Itrace, cores = 2, minLike = 1e-10, mult = -1)
# msumlogLike(p, Strace, cores = 2, minLike = 1e-10, mult = -1)

# Function that handles the formatting of constants and p for JDEoptim 
optim_wrapper <- function(p, constants, params, prepped_trace, r_rcpp = "rcpp", 
                          min_like = 1e-10, mult = -1,
                          # Arguments needed only for C++ implementation
                          nests = NULL, alpha = NULL, cell_nest = NULL, 
                          # Arguments needed only for R implementation
                          cores = 2) {
  # Grab subject and parameter info from attributes trace
  subject_names <- attributes(prepped_trace)$subject_names
  n_subjects <- length(subject_names)
  param_names <- attributes(prepped_trace)$param_names
  
  # Combine parameters and constants in one vector
  par_cons <- numeric(length(constants) + length(params))
  names(par_cons) <- param_names
  
  # Parameters are updated each iteration as p is changed
  par_cons[params] <- p
  
  # Assign constant values to constants
  par_cons[names(constants)] <- constants
  
  # Transform vector in a matrix as needed for msumlogLike, rows are identical
  par_cons <- matrix(rep(par_cons, n_subjects), nrow = n_subjects, 
                     byrow = TRUE)
  
  # Each row is a subject, and each column a parameter
  rownames(par_cons) <- subject_names
  colnames(par_cons) <- param_names
  
  # Call msumLogLike, either the R or the C++ version as assigned by "r_rcpp"
  if (r_rcpp == "r") {
    msumlogLike(par_cons, prepped_trace, cores, min_like, mult)
  } else if (r_rcpp == "rcpp") {
    m4ma::msumlogLike(par_cons, prepped_trace, nests, alpha, cell_nest, min_like, 
                      mult)
  } else {
    stop("The 'r_rcpp' argument should be either 'r' or 'rcpp'.")
  }
}


# Profile likelihood ------------------------------------------------------

# n_point = 50
# cores = 1
# digits = 2
# ylim = NA
# verbose = FALSE
profilePed <- function(p_name, min_p, max_p, trace, n_point = 50, 
                       cores = 1, digits = 2, ylim = NA, verbose = FALSE) {
  
  pMat <- attr(trace, "pMat")
  
  # Check of given parameter is in pMat
  if (!(p_name %in% colnames(pMat))) {
    stop("p_name is not a parameter")
  }
  
  # Range to compute likelihood over
  range_p <- seq(min_p, max_p, length.out = n_point)
  llike <- numeric(n_point)
  
  # For each point in range to profile
  for (i in 1:n_point) {
    # Adjust the parameter of interest
    pMat[, p_name] <- pMat[, p_name] + range_p[i]
    
    # Compute the sum of the log-likelihood
    llike[i] <- msumlogLike(pMat, trace, cores = cores)
    
    # Print iteration if verbose
    if (verbose) {
      cat(i)
    }
  }
  
  if (verbose) {
    cat("\n")
  }
  
  # names(llike) <- round(range_p, digits)  # @CT why?
  
  # Plot profile
  if (any(is.na(ylim))) {
    plot(range_p, llike, type = "l", xlab = p_name, ylab = "log-likelihood") 
  } else {
    plot(range_p, llike, type = "l", xlab = p_name, ylab = "log-likelihood", 
         ylim = ylim)
  }
  
  # Add mean 
  abline(v = mean(pMat[, p_name]))
  # ll[ll==max(ll)]
  
  # Return value of parameter where likelihood was max
  return(range_p[llike == max(llike)])
}

# # Examples
# p_name = "sPref"
# min_p = -1
# max_p = 1
# pMat = Itrace[[1]]$pMat
# trace = Itrace
# 
# profilePed(p_name = "sPref", min_p = -2, max_p = 2, trace = Itrace)
# profilePed(p_name = "sPref", min_p = -1, max_p = 1, trace = Strace)
# 
# profilePed(p_name = "bPS", min_p = -1, max_p = 1, trace = Itrace)
# profilePed(p_name = "bPS", min_p = -1, max_p = 1, trace = Strace)

