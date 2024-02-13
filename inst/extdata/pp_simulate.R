##############################################################################-
# This file contains all pedestrian movement functions for simulation. 
##############################################################################-


# Start state -------------------------------------------------------------

makeState <- function(gstack, p, pSD, group, v0 = 1, pedName = "A_1",types=NULL) {
  p_n <- gstack[1, 1:2, drop = FALSE]
  dimnames(p_n) <- list(pedName, c("x", "y"))
  P_n <- list(gstack)
  names(P_n) <- pedName
  a_n <- angle2(p_n, P_n[[1]][attr(P_n[[1]], "i"), 1:2, drop = FALSE])
  v_n <- v0
  cell <- 28
  pMat <- getpMat(length(a_n), p, pSD, pedName,types)
  r <- p["r",names(attr(pMat,"type"))]
  names(P_n) <- names(a_n) <- names(v_n) <- names(r) <- names(group) <- pedName
  
  # Store names
  numLetter <- rep(1, 52)
  names(numLetter) <- c(LETTERS, letters)
  attr(P_n, "numLetter") <- numLetter
  attr(P_n, "numLetter")["A"] <- attr(P_n, "numLetter")["A"] + 1
  
  # Make state
  # It should be explicit what all the elements of this state are!
  state <- list(p = p_n, v = v_n, r = r, a = a_n, P = P_n, G = P_n[[1]][1,1:2,drop=FALSE],
                group = group, pMat = pMat, cell = cell)
  return(state)
}


# Remove and add peds -----------------------------------------------------

exitPed <- function(state, closeEnough = 0.5) {
  done <- unlist(lapply(state$P, function(x) { 
    dim(x)[1] == attr(x, "i")  # last goal
  }))
  
  for (i in 1:length(done)) { 
    # Close to last goal
    if (done[i]) {
      done[i] <- dist1(state$p[i, ], state$P[[i]][dim(state$P[[i]])[1], 1:2, 
                                                  drop = FALSE]) < closeEnough
    }
  }
  
  if (any(done)) {
    nams <- unlist(lapply(strsplit(row.names(state$p), "_"), function(x) { 
      x[1]
    }))[done]
    state$p <- state$p[!done, , drop=FALSE]
    state$a <- state$a[!done]
    state$v <- state$v[!done]
    state$r <- state$r[!done]
    state$cell <- state$cell[!done]
    numLetter <- attr(state$P, "numLetter")
    replan <- attr(state$P, "replan")
    reroute <- attr(state$P, "reroute")
    state$P <- state$P[!done]
    state$group <- state$group[!done]
    type <- attr(state$pMat,"type")[!done] 
    state$pMat <- state$pMat[!done, , drop = FALSE]
    attr(state$pMat,"type") <- type
    state$G <- state$G[!done, , drop = FALSE]
    if (!is.null(numLetter)) 
      attr(state$P, "numLetter") <- numLetter
    attr(state$P, "reroute") <- reroute[names(state$P)]
    attr(state$P, "replan") <- replan[names(state$P)]
  }
  state
}

# Adds single ped unless the entry goal is occupied. 
# Use group and spref to assign same group number and sPref (natural scale) 
# for group members. 
# useUpper: NA = take next, TRUE = use next uppercase, FALSE = next lower case
# useLetter: use this letter value if not NA
addPed <- function(gstack, p, pSD, state = NULL, v = 1, group = NA,
                   sPref = NA, useUpper = NA, useLetter = NA, 
                   addPedReport = FALSE,types,objects) {
  
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
  
  oldstate <- state
  # Get new name
  numLetter <- attr(state$P, "numLetter")
  if (!is.na(useLetter)) {
    let <- useLetter 
  } else {
    # Letters already used 
    pnams <- unlist(lapply(strsplit(row.names(state$p), "_"), function(x) { 
      x[1]
    }))
      
    # Next available upper and lower case letters (maybe none)
    lets <- c(LETTERS[!(LETTERS %in% pnams)][1],
              letters[!(letters %in% pnams)][1])
    if (is.na(lets[1]))  # no unique upper, start using repeats
        lets[1] <- LETTERS[sample(1:26, 1, prob = (1 / numLetter[1:26]) / 
                                  sum(1 / numLetter[1:26]))]
    if (is.na(lets[2]))  # no unique lower, start using repeats
        lets[2] <- letters[sample(1:26, 1, prob = (1 / numLetter[27:52]) / 
                                    sum(1 / numLetter[27:52]))]
    if (is.na(useUpper)) {  # randomly pick upper/lower
      let <- lets[sample(1:2,1)] 
    } else  
      if (useUpper) {  # deliberately pick upper/lower
        let <- lets[1] 
      } else {
        let <- lets[2]
      } 
  }
  nam <- paste(let, numLetter[let], sep = "_")
  numLetter[let] <- numLetter[let] + 1
    
  if (is.na(group)) {
    group <- max(state$group) + 1
  }
    
  startState <- makeState(gstack, p, pSD, v = v, group = group, pedName = nam,
                          types=types)
  if (!(is.na(sPref))) {
    startState$pMat[,"sPref"] <- sPref
  }
    
  # Combine
  state$p <- rbind(state$p, startState$p)
  state$v <- c(state$v, startState$v)
  state$a <- c(state$a, startState$a)
  state$r <- c(state$r, startState$r)
  state$group <- c(state$group, startState$group)
  type <- c(attr(state$pMat,"type"),attr(startState$pMat,"type"))
  state$pMat <- rbind(state$pMat, startState$pMat)
  attr(state$pMat,"type") <- type
  state$cell <- c(state$cell, startState$cell)
  replan <- attr(state$P, "replan")
  reroute <- attr(state$P, "reroute")
  state$P <- c(state$P, startState$P)
  attr(state$P, "numLetter") <- numLetter
  tmp <- setNames(FALSE,nam)
  attr(state$P, "reroute") <- c(reroute,tmp)
  attr(state$P, "replan") <- c(replan,tmp)
  state$G <- rbind(state$G,startState$G)
  
  n=length(state$a)
  centres <- c_vd(1:33, p1 = state$p[n, ], v1 = state$v[n], a1 = state$a[n])
  
  if (occupied(P = gstack[1,1:2], startState$r, oldstate) | 
     !any(get_ok(n,objects,state,centres,startState$r))) {
    if (addPedReport) cat("No room to add pedestrian!\n")
    return(oldstate)
  } else {
    if (addPedReport) cat(paste0("Added pedestrian ",nam,"\n")) 
    return(state)
  }
} 



# Move --------------------------------------------------------------------

# plotGrid = NULL;printChoice = NULL; sStop = 0.2; usebestAngle = FALSE
# angle = c(-72.5, -50, -32.5, -20, -10, 0, 10, 20, 32.5, 50, 72.5)
# For one pedestrian calculate object blocking, utility, cell probability, 
# and animate move. Returns new state
#
# Comment: Many if-statements here: Can this be made into cases?
move <- function(n, state, P_n, p_pred, nests, alpha, objects, iInfo,
                 plotGrid = NULL, printChoice = NULL, 
                 sStop = 0.2,  # speed to resume after stop
                 usebestAngle = FALSE, angle = c(-72.5, -50, -32.5, -20, -10, 
                                                 0, 10, 20, 32.5, 50, 72.5)) {
  
  get_ok <- function(n, objects, state, centres,r)
  {
    ok <- okObject(n, objects, state, centres)
    if (class(try(!all(!ok)))=="try-error") {
      bad <<- list(i=1,n=n,objects=objects,state=state,centres=centres)
    }
    if (!all(!ok)) {
      ok <- bodyObjectOK(r, centres, objects, ok)
      ok[!ok[,3],2] <- FALSE
      ok[!ok[,2],1] <- FALSE
    }
    # if (!all(!ok)) ok <- seesGoalOK(n, objects, state, centres, ok)
    if (class(try(!all(!ok)))=="try-error") {
      bad <<- list(i=2,n=n,objects=objects,state=state,centres=centres)
    }
    if (!all(!ok)) { # If *all* next blocked go anyway to trigger re-planning
      notok <- !ok # weird bug: running seesGoalOK_rcpp changes ok or copy of ok
      okSee <- seesGoalOK(n, objects, state, centres, ok)
      if (!all(!okSee)) ok <- okSee else ok <- !notok # Do move if all cant see as otherwise get stuck
    }
    if (class(try(!all(!ok)))=="try-error") {
      bad <<- list(i=3,n=n,objects=objects,state=state,centres=centres)
    }
    if (!all(!ok)) ok <- okBodyBody(n, state, centres, ok) # Don't move where someone is now
    ok
  }  


  if (attr(state$P[[n]], "stop") > 0) {  # in interaction wait state
    cell <- 0 
    ok <- TRUE
  } else {  # do update
    if (attr(state$P[[n]], "stop")==-1) { # Starting after interaction, reorient
      turn_report <- FALSE
      attr(state$P[[n]], "stop") <- 0
      p <- toNatural(state$pMat[n, ])
      state$a[n] <- bestAngle(p, n, state, P_n, p_pred, objects, iInfo)
    } else turn_report <- TRUE
    p <- toNatural(state$pMat[n, ])
    muM <- getmuM(p) 
    centres <- c_vd(1:33, p1 = state$p[n, ], v1 = state$v[n], a1 = state$a[n])
    ok <- get_ok(n, objects, state, centres,r=state$r[n])
    if (!any(ok) | state$cell[n]==0) {  # stop or will have to 
      state$v[n] <- sStop
      # cell <- 0
      # # Turn to current goal, sometimes does nothing which can be problematic
      # state$a[n] <- angle2(state$p[n,,drop = FALSE], 
      #   state$P[[n]][attr(state$P[[n]], "i"), 1:2,drop = FALSE])
      # centres <- c_vd(1:33, p1 = state$p[n, ], v1 = state$v[n], a1 = state$a[n])
      # cat(paste(rownames(state$p)[n], "blocked, turning to goal\n"))
      # ok <- get_ok(n, objects, state, centres,state$r[n])
       
       # state$a[n] <- (state$a[n] + 180) %% 360 # and turn around
       # cell <- -1
      best <- bestAngle(p, n, state, P_n, p_pred, objects, iInfo)
      # if (!is.na(best)) {
        state$a[n] <- best
        turn <- paste("to",state$a[n],"degrees")
        cell <- 0
      # } else {
      #   cell <- bestSide(n,centres,objects,state)
      #   if (cell==-1) { # turn 90 degrees anti-clockwise
      #     state$a[n] <- (state$a[n] + 90) %% 360
      #     turn <- "anti-clockwise"
      #   } else if (cell==-2) { # turn backwards
      #     state$a[n] <- (state$a[n] + 180) %% 360
      #     turn <- "around"
      #   } else { # cell==-3, turn 90 degrees clockwise
      #     state$a[n] <- (state$a[n] - 90) %% 360
      #     turn <- "clockwise"
      #   }
      # }
      centres <- c_vd(1:33, p1 = state$p[n, ], v1 = state$v[n], a1 = state$a[n])
      ok <- get_ok(n, objects, state, centres,r=state$r[n])
      if (turn_report) cat(paste(rownames(state$p)[n], "turning",turn,"\n"))
    } 
    V <- utility(p, n, state, P_n, p_pred, centres, objects, ok, 
                   iInfo = iInfo)
    Pr <- pCNLs(V, muM, nests, alpha)
    names(Pr) <- 0:33
    if (!is.null(plotGrid) && plotGrid[n]) {
      draw_grid(state$p[n, ], state$v[n], state$a[n], plotPoints = TRUE, 
                  Pr = Pr[-1])
    }
    cell <- sample.int(length(V), 1, TRUE, prob = Pr) - 1
    if (!is.null(printChoice) && printChoice[n]) {
      if (cell == 0) {
        cat("Standing still\n\n")
      } else {
        cat(paste("\nPedestrian =", row.names(state$p)[n], 
                  " Choice =", cell, 
                  " Ring =", c("accelerate", "constant", 
                               "slow")[ringNum(cell)],
                    " Cone =", coneNum(cell), "\n\n"))
      }
      cat(paste("Probability of standing still =", round(Pr[1], 3)))
      print(matrix(round(Pr[-1], 2), nrow = 3, byrow = TRUE,
                     dimnames = list(speed = c("accelerate", "constant", 
                                               "slow"),
                                     cone = 1:11)))
      cat("\n")
    }
    if (cell != 0) {
        state$p[n, ] <- c_vd(cell, state$p[n, ], state$v[n], state$a[n])
        state$v[n] <- pmax(state$v[n] * c(1.5, 1, .5)[ringNum(cell)], sStop)
        state$a[n] <- (state$a[n] - angle[coneNum(cell)]) %% 360
    } else {
        state$v[n] <- sStop  # stopped, reset velocity
    }
  }
  out <- list(p = state$p[n, ], v = state$v[n], a = state$a[n], r = state$r[n], 
              group = state$group[n], pMat = state$pMat[n, ], cell = cell,P=state$P[[n]])
  attr(out, "ok") <- ok
  
  # # Bug catch for outside total area
  # if (!inObject(state$p[n,,drop=F],xlim=objects[[1]]$x,ylim=objects[[1]]$y,outside=FALSE))
  #   stop(n)
  
  out
}



# Move all pedestrians, optionally print and plot state, and pause
# 
# # seed <- .Random.seed
# .Random.seed <- seed

# states <- readRDS("~/Downloads/m4ma/Experiments/Estimation/Data/3-play/play_estimation_m01s03p27r001.RDS")
# states <- readRDS("~/Downloads/m4ma/Experiments/Estimation/Data/3-play/play_estimation_m01s03p28r001.RDS")
# alpha <- attributes(states)$alpha
# nests <- attributes(states)$nests
# objects <- attributes(states)$space$objects
# state <- states[[10]]
# # state <- badstate
# 
# delay = 0; collisionFix = TRUE;showMind = NULL;printChoice = NULL; sequentialUpdate=TRUE
# plotComponents = FALSE; plotCircle = FALSE; plotGrid = TRUE;usebestAngle = FALSE
# 
# fname=""; interactionTime=1; cores=1; reportGoal = FALSE; plotSim = TRUE


# lapply(state$P,function(x){dimnames(x)[[1]]})
# lapply(state$P,function(x){dimnames(x)[[1]][attr(x,"i"):dim(x)[1]]})
# unlist(lapply(state$P,function(x){dimnames(x)[[1]][dim(x)[1]]}))
# 
# endok <- function(state) all(unlist(lapply(state$P,function(x){dimnames(x)[[1]][dim(x)[1]]}))=="Gend")
# 
# unlist(lapply(states,endok))
# unlist(lapply(p18,endok))


# cores = 1; plotSim = TRUE; fname = ""; reportGoal = FALSE; interactionTime = 1
#
# delay = 0;
# collisionFix = TRUE; showMind = NULL; printChoice = NULL;
# plotComponents = FALSE; plotCircle = FALSE; plotGrid = TRUE;
# usebestAngle = FALSE; sequentialUpdate=TRUE

moveAll <- function(state, objects, nests, alpha, delay = 0, cores = 1, 
                    collisionFix = TRUE, showMind = NULL, printChoice = NULL, 
                    plotComponents = FALSE, plotCircle = FALSE, plotGrid = TRUE, 
                    plotSim = TRUE, usebestAngle = FALSE, fname = "", 
                    interactionTime = 1, reportGoal = FALSE, 
                    sequentialUpdate=TRUE, iteration=1) {
  
  combineState <- function(state.list, nams, P_n) {
    p <- t(apply(state.list, 2, function(x) {
      x$p
    }))
    row.names(p) <- nams
    v <- apply(state.list, 2, function(x) {
      x$v
    })
    names(v) <- nams
    a <- apply(state.list, 2, function(x) {
      x$a
    })
    names(a) <- nams
    r <- apply(state.list, 2, function(x) {
      x$r
    })
    names(r) <- nams
    group <- apply(state.list, 2, function(x) {
      x$group
    })
    names(group) <- nams
    pMat <- t(apply(state.list, 2, function(x) {
      x$pMat
    }))
    row.names(pMat) <- nams
    cell <- apply(state.list, 2, function(x) {
      x$cell
    })
    list(p = p, v = v, a = a, r = r, P = P_n, group = group, pMat = pMat, 
         cell = cell)
  }
  
  # Information about interactions among pedestrians
  if (is.list(state$P) & length(state$P) > 1) {  # only relevant if more than 1
    iInfo <- getiInfo(state) 
  } else {
    iInfo <- NULL
  }
  # Extract goals
  if (is.list(state$P)) {
    P_n <- getP(state)
    stops <- unlist(lapply(state$P,function(x){attr(x,"stop")>0}))
  } else {
    P_n <- state$P
    stops <- attr(P_n,"stop")>0
  }
  # Predicted positions
  p_pred <- predictPed(state$p, state$v, state$a, state$cell)
  
  # Inspect utility
  if (!is.null(showMind)) {
    inspect <- showMind 
  } else {
    inspect <- FALSE
  }
  if (!is.null(printChoice)) {
    inspect <- inspect | printChoice
  }
  if (any(inspect)) {
    for (n in 1:length(inspect)) {
      if (inspect[n]) {
        if (is.null(state$cell)) {
          state$cell <- rep(1, length(state$a))
        }
        plotPed(state$p, P_n, state$a, state$r, objects, state$cell, 
                plotCircle = plotCircle,types=attr(state$pMat,"type"),stops=stops)
        
        # Save tmp to get ok attribute
        tmp <- move(n, state = state, P_n = P_n, p_pred = p_pred, 
                    nests = nests, alpha = alpha, iInfo = iInfo,
                    objects = objects, plotGrid = showMind, 
                    printChoice = printChoice)
        invisible(readline(prompt = "Press [enter] to continue, [esc] to terminate"))
        cat("\n")
        if (!is.null(printChoice) && printChoice[n] & plotComponents) {
          if (!any(attr(tmp, "ok"))) {
            cat("No cells OK!\n")  
          } else {
            centres <- c_vd(1:33, p1 = state$p[n, ], v1 = state$v[n], 
                            a1 = state$a[n])
            plotUtility(n, state, P_n, p_pred, centres, objects, nests, alpha,
                        ok = attr(tmp, "ok"), iInfo) 
          }
          invisible(readline(prompt = "Press [enter] to continue, [esc] to terminate"))
        }
      }
    }
  }

  # Main update
  if (sequentialUpdate) { # Avoids collisions, reverse update so new entry guaranteed to get in.
    for (n in length(state$a):1) { 
    # for (n in 1:43) {  # l=21, p=25
      tmp <- move(n,state,P_n,p_pred,nests,alpha,objects,iInfo,usebestAngle = usebestAngle) 
      state$p[n,] <- tmp$p
      state$v[n] <- tmp$v
      state$a[n] <- tmp$a
      state$cell[n] <- tmp$cell
      state$P[[n]] <- tmp$P
      p_pred <- predictPed(state$p, state$v, state$a, state$cell)
    }
  } else { 
    oldState <- state
    state.list <- mcmapply(move, 1:length(state$a), mc.cores = cores, 
                           MoreArgs = list(state = state, P_n = P_n, 
                                           p_pred = p_pred, objects = objects, 
                                           alpha = alpha, nests = nests, 
                                           iInfo = iInfo, 
                                           usebestAngle = usebestAngle))
    state <- combineState(state.list, nams = row.names(state$p), P_n = state$P)
    # Check and fix collisions
    if (collisionFix) {
      if (is.list(state$P)) {
        P_n <- getP(state) 
      } else {
        P_n <- state$P
      }
      iInfo <- getiInfo(state)
      p_pred <- predictPed(state$p, state$v, state$a, state$cell)
      state <- fixCollision(state, oldState, P_n, p_pred, objects, iInfo, 
                            cores = cores)
    }
  }
  if (plotSim) {
    plotPed(p_n = state$p, P_n = P_n, a_n = state$a, r_n = state$r, objects,stops=stops, 
            cells = state$cell, plotCircle = plotCircle, fname = fname,types=attr(state$pMat,"type"))
    title(main=paste("Iteration",iteration))
  }
  
  # store current goal matrix
  state$G <- do.call(rbind,lapply(state$P,function(x){x[attr(x,"i"),1:2,drop=FALSE]}))

  if (is.list(state$P)) {
    # Goal stack
    state <- updateGoal(state, objects, interactionTime = interactionTime,
                        reportGoal = reportGoal) 
  }
  Sys.sleep(delay)
  state
}



