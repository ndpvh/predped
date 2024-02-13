##############################################################################-
# This file contains all functions to fix collisions.
##############################################################################-


# Fix collision -----------------------------------------------------------

# Scan around in 10 degree increments when stopped to find best utility
bestAngle <- function(p, n, state, P_n, p_pred, objects, iInfo, cores = 1,step=45) {  

  get_ok <- function(n, objects, state, centres,r)
  {
    ok <- okObject(n, objects, state, centres)
    if (!all(!ok)) {
      ok <- bodyObjectOK(r, centres, objects, ok)
      ok[!ok[,3],2] <- FALSE
      ok[!ok[,2],1] <- FALSE
    }
    # if (!all(!ok)) ok <- seesGoalOK(n, objects, state, centres, ok)
    if (!all(!ok)) { # If *all* next blocked go anyway to trigger re-planning
      notok <- !ok # weird bug: running seesGoalOK_rcpp changes ok or copy of ok
      okSee <- seesGoalOK(n, objects, state, centres, ok)
      if (!all(!okSee)) ok <- okSee else ok <- !notok # Do move if all cant see as otherwise get stuck
    }
    if (!all(!ok)) ok <- okBodyBody(n, state, centres, ok) # Don't move where someone is now
    ok
  }  
  
  getVA <- function(i, n, p, statei, P_n, objects, iInfo) {
    statei$a[n] <- i * step  
    centres <- c_vd(1:33, p1 = statei$p[n, ], v1 = statei$v[n], a1 = statei$a[n])
    ok <- get_ok(n, objects, statei, centres,statei$r[n])
    V <- utility(p, n, statei, P_n, p_pred, centres, objects, ok, iInfo)[-1]
    c(V = max(V), a = statei$a[n])
  }
  
  nturn <- 360 %/% step
  VA <- matrix(nrow=nturn,ncol=2)
  for (i in 0:(nturn-1)) VA[i+1,] <- getVA(i,n,p, state,P_n,objects,iInfo)
  # if ( all(VA[,1]==-Inf) ) NA else 
  VA[which.max(VA[,1]),2]
}

# return n in state to oldState position, angle and goal, but stop 
revertState <- function(n, state, oldState, P_n, p_pred, objects, iInfo,
                        reorient = FALSE, usebestAngle = FALSE, cores = 1, 
                        sStop = 0.2) {
  p <- toNatural(state$pMat[n, ])
  state$p[n, ] <- oldState$p[n, ]
  state$P[[n]] <- oldState$P[[n]]
  state$v[n] <- sStop
  if (reorient & usebestAngle) {
    cat("Choosing best angle \n")
    state$a[n] <- bestAngle(p, n, state, P_n, p_pred, objects, iInfo, 
                            cores = cores)
  } else {
    state$a[n] <- oldState$a[n] 
  }
  state$cell[n] <- 0
  return(state)
}

# Find cases where new positions cause body clash and revert back to old 
# position in a stationary state.
fixCollision <- function(state, oldState, P_n, p_pred, objects, iInfo, 
                         usebestAngle = FALSE, cores = 1) {
  nped <- dim(state$p)[1]
  bads <- numeric(0)
  
  for (n in 1:nped) {
    bad <- c(1:nped)[-n][dist1(state$p[n, , drop = FALSE], 
                               state$p[-n, , drop = FALSE]) < 
                           (state$r[n] + state$r[-n])]
    if (length(bad) > 0) {
      bads <- c(bads, n)
      cat(paste("\nClash involving", names(state$v)[n], "with", 
                names(state$v[bad]), "\n"))
      state <- revertState(n, state, oldState, P_n, p_pred, objects, iInfo,
                           reorient = n %in% oldState$bads, 
                           usebestAngle = usebestAngle, cores = cores)
      cat("\n")
    }
  }
  state$bads <- bads
  
  # for (n in 1:(nped - 1)) {
  #   bad <- c((n + 1):nped)[dist1(state$p[n, , drop = FALSE],
  #                                state$p[-c(1:n), , drop = FALSE]) <
  #                            (state$r[n] + state$r[-c(1:n)])]
  #   if (length(bad) > 0) {
  #     bads <- c(bads, n, bad)
  #     cat(paste("\nClash involving", names(state$v)[n], "\n"))
  #     state <- revertState(n, state, oldState, P_n, p_pred, objects, iInfo,
  #                          reorient = n %in% oldState$bads, 
  #                          usebestAngle = usebestAngle, cores = cores)
  #     for (i in bad) {
  #       cat(paste("   with", names(state$v)[i], "\n"))
  #       state <- revertState(i, state, oldState, P_n, p_pred, objects, iInfo,
  #                            reorient = i %in% oldState$bads, 
  #                            usebestAngle = usebestAngle, cores = cores)
  #     }
  #     cat("\n")
  #   }
  # }
  # state$bads <- bads
  # for (n in 1:(nped - 1)) {
  #   bad <- c((n + 1):nped)[dist1(state$p[n, , drop = FALSE],
  #                                state$p[-c(1:n), , drop = FALSE]) < (
  #                                  state$r[n] + state$r[-c(1:n)])]
  #   if (length(bad) > 0) {
  #     cat(paste("Second try: Clash involving", names(state$v)[n], "\n"))
  #     state <- revertState(n, state, oldState)
  #     for (i in bad) {
  #       cat(paste("   with", names(state$v)[i], "\n"))
  #       state <- revertState(i, state, oldState)
  #     }
  #     cat("\n")
  #   }
  # }
  return(state)
}