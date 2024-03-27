##############################################################################-
# This file contains all utility (component) functions. 
##############################################################################-


# Individual utility ------------------------------------------------------

## PS: Preferred speed ----------------------------------------------------

# Preferred speed utility for cell 1..33 given current speed v
# d is distance from goal. If closer than sSlow seconds to goal 
# slows linearly
psUtility <- function(p, v, d) {
  # sPref <- pmin(p["sPref"], d * p["sPref"] / p["sSlow"])

  # Determine whether one can keep their preferred speed, or whether they
  # should slow down as their goal is nearby
  sPref <- pmin(p["sPref"], p["sPref"] * (d /(v * p["sSlow"])) ) 

  # Change the weight on the utility of speeding up, slowing down, or 
  # staying the same speed (if closer to goal, then you automatically 
  # slow down to some extent: slowing down then means slowing down 
  # even more than you initially thought you would)
  # This works in an attractor kind of way, with preferred speed 
  # working like an attractor and aPS being the power the attractor 
  # is taken to (at this moment taken to be 2; simple linear model)
  -p["bPS"] * c(rep(abs(v * 1.5 - sPref)^p["aPS"], 11), 
                rep(abs(v - sPref)^p["aPS"], 11),
                rep(abs(v/2 - sPref)^p["aPS"], 11))  
}


## GA: Goal angle ---------------------------------------------------------

# Goal angle utility, GA = goal angle (degrees/90) for 11 cones  
gaUtility <- function(p, GA) {
    # Question: No attractor? Or is GA this difference already?
  -rep(p["bGA"] * GA^p["aGA"], times = 3)
}


## CA: Current angle and side preference ----------------------------------

# Current angle utility
caUtility <- function(p, angles = c(10, 20, 32.5, 50, 72.5) / 90) {
    # Take the angles to a power
  ap <- angles^p["aCA"]

  # Here, I am not sure what to think of it
  -rep(c(rep(p["bCA"] * p["bCAlr"], 5), 1, rep(p["bCA"] / p["bCAlr"], 5)) * 
         c(ap[5:1], 0, ap), times = 3)
}


# Social utility ----------------------------------------------------------


## ID: Interpersonal distance ---------------------------------------------

# Inter-personal distance utility for cell 1..33. b parameter divided by
# sum over power of distances between bodies for cell to all inFront peds
idUtility <- function(p, n, ID, ok, group) {
  # None in front return -Inf for cells blocked by objects
  if (is.null(ID) | !any(ok)) {  
    return(as.vector(ifelse(ok, 0, -Inf)))
  }
  
  # Group dependent b, bigger for outgroup by dID
  namesInGroup <- names(group[-n][group[-n] == group[n]])
  bID <- ifelse(dimnames(ID)[[1]] %in% namesInGroup, p["bID"], 
                p["bID"] + p["dID"])
  
  # Object or ped clash
  ok <- ok & apply(ID, 2, function(x) {
    all(x > 0)
  })
  out <- ifelse(ok, 0, -Inf)
  
  # Repulsion
  if (p["bID"] != 0) {
    out[ok] <- -apply(bID / (ID[, ok, drop = FALSE]^p["aID"]), 2, sum)
  }
  
  as.vector(out)
}


## BA: Blocked angle ------------------------------------------------------

# Repulsion
baUtility <- function(p, BA) {
  if (is.null(BA)) {
    return(rep(0,33))
  }
  cells <- rep(0, 33)
  cells[as.numeric(names(BA))] <- p["bBA"] / (pmax(BA, 0)^p["aBA"])
  return(-cells)
}


## FL: Follow the leader --------------------------------------------------

flUtility <- function(p, FL) {
  if (is.null(FL)) {
    return(numeric(33))
  }
  
  # Ingroup and same direction weighted b for each leader
  b <- (p["bFL"] + p["dFL"] * FL$leaders["inGroup", ]) * 
    FL$leaders["angleDisagree", ]
  
  # Sum of utilities for leaders
  -apply(sapply(1:length(b), function(i) {
    b[i] * FL$dists[i, ]^p["aFL"]
  }), 1, sum)
}


## WB: Walk Beside --------------------------------------------------------

wbUtility <- function(p, WB) {
  if (is.null(WB)) {
    return(numeric(33))
  }
  
  # Sum of utilities for buddies
  -apply(sapply(1:dim(WB$dists)[1], function(i) {
    p["bWB"] * WB$buddies["angleDisagree", i] * WB$dists[i, ]^p["aWB"]
  }), 1, sum)
}


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
