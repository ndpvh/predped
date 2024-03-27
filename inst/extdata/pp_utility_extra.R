##############################################################################-
# This file contains all functions that pre-compute parts of utility functions
##############################################################################-


# Individual utility ------------------------------------------------------

## GA: Goal angle ---------------------------------------------------------

# Absolute angular difference between 11 directions with zero cone having
# angle a and destination angle for pedestrian at p1 heading to P1
# a = state$a[n]; p1 = state$p[n, , drop = FALSE]; P1 = P_n[n, , drop = FALSE]
destinationAngle <- function(a, p1, P1,
                             angles = c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 
                                        327.5, 310, 287.5)) {
  sapply((angles + a) %% 360, minAngle, a2 = angle2(p1, P1))
}

# Example
# p1 = matrix(c(0, 0), nrow = 1); P1 = matrix(c(1, -1), nrow = 1); a = 270
# angle2(p1, P1)
# sapply((angles + a) %% 360, minAngle, a2 = angle2(p1, P1))   


# Social utility ----------------------------------------------------------

## ID: Interpersonal distance ---------------------------------------------

# Matrix of predicted distance between edge of bodies from ped n to others in  
# front, each column is a cell each row an in front ped, if none returns null
# p1 = state$p[n, , drop = FALSE]; a1 = state$a[n]; r = state$r
predClose <- function(n, p1, a1, p2, r, centres, p_pred, objects) {
  if (dim(p_pred)[1] == 1) {
    return(NULL)
  }
  
  # remove self and pedestrians you cant see 
  occluded <- c(1:dim(p2)[1])[-n][!seesMany(p1, ps = p2[-n, , drop = FALSE], 
                                            objects)]
  p_pred <- p_pred[-c(n,occluded), , drop = FALSE]
  if (dim(p_pred)[1] <= 1) {
    return(NULL)
  }
  p2 <- p2[-c(n, occluded), , drop = FALSE]
  
  # Peds in field of vision now and when moved
  inFront <- (minAngle(a1, angle2(p1, p2)) < 85) & 
    (minAngle(a1, angle2(p1, p_pred)) < 85) 
  if (!any(inFront)) {
    return(NULL)
  }
  
  # Distance to predicted positions
  d <- matrix(apply(centres, 1, dist1, p2 = p_pred[inFront, , drop = FALSE]),
              nrow = sum(inFront), 
              dimnames = list(names(inFront[inFront]), NULL))
  d <- d - (r[n] + r[names(inFront)[inFront]]) # subtract bodies
  d
}

## BA: Blocked angle ------------------------------------------------------

# "egocentric" objects = end points of line segments at right angles to a line
# from point p1 (a matrix with columns x and y) to p2, where p2 a matrix of x,y 
# coordinates of n circular pedestrian objects (one per row) each of radius r 
# (a vector, same length as rows in p2 or a single value replicated), where the  
# the line lengths are equal to the width of the object at p2 from the 
# perspective of p1.
# Output is an n (peds) x 2 (x,y) by ends (ac=anticlockwise, cw=clockwise) array
# p1 = state$p[13, , drop = FALSE]; p2 = state$p[-13, , drop = FALSE]; 
# r = 0.75 / 2
eObjects <- function(p1, p2, r) {
  d <- dist1(p1, p2)
  a12 <- angle2(p1, p2)
  r <- rep(r, length.out = dim(p2)[1])  # in case a single value
  theta <- atan(r / d) * 180 / pi
  ac <- t(as.vector(p1) + t(aTOd((a12 + theta) %% 360) * d))  # anti-clockwise
  cw <- t(as.vector(p1) + t(aTOd((a12 - theta) %% 360) * d))  # clockwise
  # points(acw,pch=16)
  # points(cw,pch=16)
  array(c(ac, cw), dim = c(dim(ac), 2), 
        dimnames = list(names(d), dimnames(ac)[[2]], ends = c("ac", "cw")))
}

# Intersecting cones for closest p2 pedestrian profiles (circles of radius r) from 
# the perspective of p1 (calculated by eObject) where the point on the profile 
# at the cone midline can be seen (i.e., not occluded by objects) given heading 
# angle a. 
# Output NULL or cone number named vector of distances from p1 to point of intersection.
# Now returns NULL when just one pedestrian inside
# p1 = state$p[n, , drop = FALSE]; p2 = p_pred[-n, , drop = FALSE]; 
# a = state$a[n]; r = state$r
#
# BUG: should pass actual position not p_pred to evaluate SEEN
iCones <- function(p1, a, p2, r, objects) {
  
  # One end in, one out, fill in extreme cone for out
  fix <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    }
    if (all(is.na(x) == c(FALSE, TRUE))) {
      x <- c(x[1],11)  # cw out
    }
    if (all(is.na(x) == c(TRUE, FALSE))) {
      x <- c(1,x[2])  # ac out
    }
    if (length(x) > 1) {
      x <- x[1]:x[2]  # fill in intervening
    }
    return(x)
  }
  
  if (dim(p2)[1] == 0) {
    return(NULL)
  }
  ends <- eObjects(p1, p2, r)
  endCones <- apply(ends, 3, function(x) {
    Iangle(p1, a, x)
  })
  if (dim(ends)[1] == 1) { 
    endCones <- matrix(endCones, nrow = 1, 
                       dimnames = list(dimnames(p2)[1], NULL))
  }
  cList <- vector(mode = "list", length = dim(endCones)[1])
  names(cList) <- dimnames(endCones)[[1]]
  for (i in 1:length(cList)) {
    cList[[i]] <- fix(endCones[i, ])
  }
  cList <- cList[unlist(lapply(cList, function(x) {
    !any(is.na(x))
  }))]
  if (length(cList) == 0) {
    return(NULL)
  }
  
  # List of candidate cones for each participant in front
  cList <- cList[!unlist(lapply(cList, is.null))]
  if (length(cList) == 0) {
    return(NULL)
  }
  
  # End of unit line from p1 in direction of each cone
  coneLineEnds <- c_vd(1:11, as.vector(p1), rep(1, 11), a)
  
  # Distances to objects in cones
  cDist <- vector(mode = "list", length = length(cList)) 
  for (i in names(cList)) {  # remove when cant see
    if (length(cList[[i]]) == 1) {
      if (!seesGoal(p1, p2[i, , drop = FALSE], objects)) {
        cList[[i]] <- numeric(0)
      } else {
        cDist[[i]] <- dist1(p1, p2[i, , drop = FALSE])
      }
    } else {  # more than one cone to check
      for (j in 1:length(cList[[i]])) {  # remove cant see
        # Intersection of cone line and end
        P_n <- matrix(line.line.intersection(p1, coneLineEnds[cList[[i]][j], ], 
                                             ends[i, , 1], ends[i, , 2]), 
                      nrow = 1)
        if (!seesGoal(p1, P_n, objects)) {
          cList[[i]][j] <- NA 
        } else {
          if (j == 1) {
            cDist[[i]] <- dist1(p1,P_n) 
          } else {
            cDist[[i]] <- c(cDist[[i]], dist1(p1, P_n))
          }
        }
      }
      cList[[i]] <- cList[[i]][!is.na(cList[[i]])]
    }
  }
  cList <- cList[!unlist(lapply(cList, function(x) {
    length(x) == 0
  }))]
  if (length(cList) == 0) {
    return(NULL)
  }
  cDist <- cDist[!unlist(lapply(cDist, is.null))]
  for (i in 1:length(cList)) {
    names(cDist[[i]]) <- cList[[i]]
  }
  outCones <- out <- numeric(0)
  for (i in 1:11) {
    d <- unlist(lapply(cDist, function(x) {
      x[names(x) == i]
    }))
    if (length(d) > 0) {
      outCones <- c(outCones, i)
      out <- c(out, min(d))
    }
  }
  names(out) <- outCones
  out
}

# Turns output of iCones into cell-named vector of distances to each ring
# for pedestrian moving at speed v
iCones2Cells <- function(iC, v, vels = c(1.5, 1, .5)) {
  out <- rep(iC, times = 3) - rep(scaleVel(v) * vels, each = length(iC))
  names(out) <- rep(as.numeric(names(iC)), times = 3) + 
    rep(c(0, 11, 22), each = length(iC))
  return(out)
}

# Example
# for (i in 1:length(state$a)) {
#   # iC=iCones(p1=state$p[i,,drop=FALSE],p2=state$p[-i,,drop=FALSE],a=state$a[i],
#   #              r=0.75/2,objects=objects)
#   iC=iCones(p1=state$p[i,,drop=FALSE],p2=p_pred[-i,,drop=FALSE],a=state$a[i],
#                r=0.75/2,objects=objects)
#   print(names(state$a)[i])
#   print(iC)
#   cat("\n")
#   print(round(iCones2Cells(iC,state$v[i]),2))
#   cat("\n\n")
# }

# Distance from each cell to closest pedestrian profile in each cone
# adjusted from ring. Outputs a cell-named vector of distances. 
# BUG: should pass actual position not p_pred to evaluate SEEN
blockedAngle <- function(n, state, p_pred, objects) {
  iC <- iCones(p1 = state$p[n, , drop = FALSE], a = state$a[n], 
               p2 = p_pred[-n, , drop = FALSE], r = state$r, objects)
  iCones2Cells(iC, state$v[n])
}


## FL: Follow the leader --------------------------------------------------

# Which cell for ped n (p1) are other pedestrians (p2) in? 
# onlyGroup => p2 must be in p1's group, preferGroup => group members first.
# If more than one remaining in a cell pick the one whose current heading is 
# closest to the target heading of p1.
# Return a list of matices, "leaders" with column for each potential leader,
# rows for their cell normalized (0-1) angle disagreement and inGroup status, dists with
# row per leader and columns of cells with distance from cell to chosen cell
getLeaders <- function(n, state, centres, objects, onlyGroup = FALSE, 
                       preferGroup = TRUE, pickBest = FALSE) {
  p1 <- state$p[n, , drop = FALSE]
  a1 <- state$a[n] 
  v1 <- state$v[n] 
  
  # Remove peds cant see
  occluded <- c(n, c(1:length(state$v))[-n][
    !seesMany(p1, ps = state$p[-n, , drop = FALSE], objects)])
  a2 <- state$a[-occluded]
  p2 <- state$p[-occluded, , drop = FALSE]
  
  if (dim(p2)[1] == 0) {
    return(NULL)
  }
  if (!is.list(state$P)) {
    P1 <- state$P[n, 1:2, drop = FALSE] 
  } else {
    P1 <- state$P[[n]][attr(state$P[[n]], "i"), 1:2, drop = FALSE]
  }
  
  I_n <- Iangle(p1, a1, p2)  # is in cone
  I_n <- I_n[!is.na(I_n)]
  if (length(I_n) == 0) {
    return(NULL)
  }
  
  # Subset in rings 1-3
  ring <- as.numeric(
    as.character(cut(dist1(p1, p2[names(I_n), , drop = FALSE]), 
                     c(0, scaleVel(v1) * c(.5, 1, 5)), 
                     c("3", "2", "1"))))
  names(ring) <- names(I_n)
  ring <- ring[!is.na(ring)]
  if (length(ring) == 0) {
    return(NULL)
  }
  
  candidates <- I_n[names(ring)] + 11 * (ring - 1)
  inGroup <- state$group[names(candidates)] == state$group[n]
  if (onlyGroup) {
    if (!any(inGroup)) {
      return(NULL) 
    } else {
      candidates <- candidates[inGroup]
    }
  } else if (preferGroup & any(inGroup)) {
    candidates <- candidates[inGroup]
  }
  
  # Difference in angle between leader heading and destination
  angles <- sapply(a2[names(candidates)], minAngle, a2 = Dn(p1, P1))
  ok <- angles < 90
  if (!any(ok)) {
    return(NULL)
  }
  candidates <- candidates[ok]
  angles <- angles[ok]
  if (!any(duplicated(candidates))) {
    leaders <- candidates 
  } else {
    leaders <- unique(candidates)
    for (i in 1:length(leaders)) { 
      names(leaders)[i] <- names(candidates)[candidates == leaders[i]][
        which.min(angles[candidates == leaders[i]])] 
    }
    angles <- angles[names(leaders)]
  }
  
  # Distances to leader cells
  d <- array(dim = c(length(leaders), 33), 
             dimnames = list(names(leaders), 1:33))
  for (i in 1:length(leaders)) {
    d[i, ] <- dist1(centres[leaders[i], ], centres)
  }
  if (pickBest) {
    best <- which.min(angles) 
  } else {
    best <- 1:length(angles)
  }
  
  list(dists = d[best, , drop = FALSE], 
       leaders = rbind(cell = leaders, 
                       angleDisagree = (angles[names(leaders)]) / 90, 
                       inGroup = inGroup[names(leaders)])[, best, drop = FALSE])
}


## WB: Walk Beside --------------------------------------------------------

# Distance of your cells from cell centre closest to buddy
getBuddy <- function(n, group, a, p_pred, centres, objects, pickBest = FALSE, 
                     state) {
  # Remove peds cant see and self
  occluded <- c(n, c(1:length(state$v))[-n][
    !seesMany(state$p[n, , drop = FALSE], ps = state$p[-n, , drop = FALSE], 
              objects)])
  p_pred <- p_pred[-occluded, , drop = FALSE]
  if (dim(p_pred)[1] == 0) {
    return(NULL)
  }
  
  # NB: Not checking in front as assume you know where your group members are.
  #     If they are behind this formulation will tend to slow you down so they 
  #     can catch up.
  inGroup <- group[-occluded] == group[n]
  p_pred <- p_pred[inGroup, , drop = FALSE]
  nped <- dim(p_pred)[1]
  if (nped == 0) {
    return(NULL)
  }
  
  # Potential buddy matrix of difference between cone angles (rows) 
  # and heading angle for potential buddy (columns)
  headingDifference <- sapply(a[row.names(p_pred)], headingAngle, a1 = a[n])
  
  # Most parallel cones for each potential companion
  parallelCone <- apply(headingDifference, 2, which.min)
  
  # Distances from predicted buddy position to acc, const and dec rings in each 
  # cone
  d <- sapply(1:nped, function(x) {
    dist1(p_pred[names(parallelCone)[x], ],
          centres[c(parallelCone[x], parallelCone[x] + 11, 
                    parallelCone[x] + 22), ])
  })
  
  # Ring closest to buddies
  ring <- apply(d, 2, which.min)
  
  # Cell closest to buddy
  cell <- cbind(parallelCone, parallelCone + 11, 
                parallelCone + 22)[cbind(1:nped, ring)]
  names(cell) <- names(parallelCone)
  
  # Heading difference for each buddy
  angleDisagree <- (headingDifference[cbind(parallelCone, 
                                            1:length(parallelCone))]) / 90
  
  # Distances to buddies cells
  d <- array(dim = c(nped, 33), dimnames = list(names(angleDisagree), 1:33))
  for (i in 1:nped) {
    d[i,] <- dist1(centres[cell[i], ], centres)
  }
  if (pickBest) {
    best <- which.min(angleDisagree) 
  } else {
    best <- 1:nped
  }
  list(buddies = rbind(cell, angleDisagree)[, best, drop = FALSE],
       dists = d[best, , drop = FALSE])
}

