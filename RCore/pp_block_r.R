##############################################################################-
# This file contains all blocking functions.
##############################################################################-


# Base blocking -----------------------------------------------------------

# Is xy inside or outside of the square
#
# Can be made even simpler I think, but depends on its use-case
#   -> What does the outside do here: It reverses the conclusion of inObject
inObject <- function(xy, xlim, ylim, outside = TRUE) {
  ok <- (xy[1] > xlim[1]) & (xy[1] < xlim[2]) & (xy[2] > ylim[1]) &
    (xy[2] < ylim[2])
  if (outside) {
    return(!ok)
  } else {
    return(ok)
  }
}

# # Turns an rectangular object into a specification of its constituent lines
# # Output a 2 (x/y) x 4 (lines) x 2 (begin/end point) array
# object2lines <- function(o) {
#   array(c(o$x[1], o$y[1], o$x[1], o$y[2], o$x[1], o$y[1], o$x[2], o$y[1],
#           o$x[2], o$y[1], o$x[2], o$y[2], o$x[1], o$y[2], o$x[2], o$y[2]),  
#         dim = c(2, 4, 2), dimnames = list(c("x", "y"), 
#                                           c("L1", "L2", "L3", "L4"), 
#                                           c("P1", "P2")))
# }
# 
# 
# Cells OK ----------------------------------------------------------------

# Is line of sight from p_n to centres blocked?
isBlocked <- function(centres, p_n, objects, ok = logical(33)) {
  # inner ring blocked?
  ok[23:33] <- apply(centres[23:33, ], 1, seesGoal, P_n = p_n,
                     objects = objects)
  remain <- c(12:22)[ok[23:33]]
  if (length(remain) > 0) {
    ok[remain] <- apply(centres[remain, , drop = FALSE], 1, seesGoal,
                        P_n = p_n, objects = objects)
  }
  remain <- c(1:11)[ok[12:22]]
  if (length(remain)>0) {
    ok[remain] <- apply(centres[remain, , drop = FALSE], 1, seesGoal,
                        P_n = p_n, objects = objects)
  }
  return(!ok)
}

# Boolean indicating if each cell is in the room and not blocked by objects
okObject <- function(n, objects, state, centres) {
  # Inside the room (defined by objects[[1]])
  ok <- apply(centres, 1, inObject, xlim = objects[[1]]$x,
              ylim = objects[[1]]$y, outside = FALSE)
  # Are cells blocked by objects?
  if (length(objects) > 1) {
    blocked <- isBlocked(centres, state$p[n, ], objects[2:length(objects)])
  } else {
    blocked <- NULL
  }
  if (!is.null(blocked)) {
    ok <- ok & !blocked
  }
  matrix(ok, ncol = 3)
}

# # For body radius r at each coordinate in centres is there overlap with any of 
# # the constituent lines in oL that specify an object. 
# # returns boolean vector, TRUE for cells with overlap
# bodyObjectOverlap <- function(oL, r, okCentres) {
#   # Right angles to each object line
#   a <- unique((angle2(p1 = t(oL[, , "P1"]), p2 = t(oL[, , "P2"])) + 90) %% 180)
#   
#   # dx and dy to move along line
#   x <- r * sin(a * pi / 180)
#   y <- r * cos(a * pi / 180)
#   
#   # For each centre check for overlap
#   apply(okCentres, 1, function(p) { 
#     # Lines 
#     segments <- array(c(p - rbind(x, y), p + rbind(x, y)), 
#                       dim = c(2, length(x), 2),
#                       dimnames = list(c("x", "y"), NULL, c("P1", "P2")))
#     out <- FALSE 
#     for (j in 1:dim(segments)[2]) {
#       out <- out | any(sapply(1:(dim(oL)[2]), function(i){
#         all(is.finite(line.line.intersection(segments[,j,"P1"], 
#                                              segments[,j,"P2"], 
#                                              oL[,i,"P1"], oL[,i,"P2"], TRUE)
#         )) 
#       }))
#     }
#     out
#   })
# }
# 
# # Is the cell OK or does body radius r positioned at centres overlap with an 
# # object? 
# bodyObjectOK <- function(r, centres, objects, ok) {
#   if (!any(ok)) {
#     return(NULL)
#   }
#   oLines <- lapply(objects, object2lines)
#   
#   # Does it overlap
#   out <- !logical(33)
#   out[ok] <- apply(matrix(unlist(
#     lapply(oLines, bodyObjectOverlap, r = r,
#            okCentres = centres[as.vector(ok), , drop = FALSE])),
#     nrow = sum(ok)), 1, function(x) {
#       any(x)
#     })
#   # If it doesn't overlap it is OK
#   matrix(!out, nrow = 11)
# }
# 

# Is the cell OK or does body radius r positioned at centres overlap with
# another body?
okBodyBody <- function(n, state, centres, ok) {
  if (!any(ok)) {
    return(NULL)
  }
  d <- state$r[n] + state$r[-n]
  out <- !logical(33)

  # Any overlap?
  out[ok] <- apply(centres[ok, , drop = FALSE], 1, function(x) {
    any(dist1(matrix(x, nrow = 1), state$p[-n, , drop = FALSE]) < d)
  })

  # If it doesnt overlap it is OK
  matrix(!out, nrow = 11)
}

# Cells OK distances, used in re-routing ---------------------------------------

dbodyObject <- function (r, centres, objects) 
{
    
  bodyObjectOverlapd <- function (oL, r, centres) 
  {
    nL <- dim(oL)[2]
    nC <- dim(centres)[1]
    d21 <- apply(oL, 1:2, diff)
    d01 <- xy <- dxy <- array(dim = c(2, nL, nC), dimnames = list(c("x", 
        "y"), NULL, NULL))
    param <- array(dim = c(nL, nC))
    for (i in 1:nL) {
        d01[, i, ] <- as.vector(t(centres)) - as.vector(oL[, 
            i, 1])
        param[i, ] <- d21[, i] %*% d01[, i, ]
    }
    len_sq <- apply(d21^2, 2, sum)
    len0 <- len_sq == 0
    param[!len0, ] <- param[!len0, ]/len_sq[!len0]
    for (j in 1:nC) {
        for (i in 1:nL) {
            if (param[i, j] < 0) 
                xy[, i, j] <- oL[, i, 1]
            else if (param[i, j] > 1) 
                xy[, i, j] <- oL[, i, 2]
            else xy[, i, j] <- oL[, i, 1] + param[i, j] * d21[, 
                i]
            dxy[, i, ] <- t(centres) - xy[, i, ]
        }
    }
    len <- sqrt(apply(dxy^2, 2:3, sum))-r
    len[len>0] <- 0
    apply(abs(len),2,sum)
  }
    
  oLines <- lapply(objects, object2lines_r)
  out <- apply(matrix(unlist(
    lapply(oLines, bodyObjectOverlapd, r = r, centres)
    ),nrow = 33), 1, sum)
  matrix(out, nrow = 11)
}
 
  
dBodyBody <- function(n, state, centres) 
{
  d <- state$r[n] + state$r[-n]
  # Any overlap?
  out <- apply(centres, 1, function(x) {
    bad <- dist1(matrix(x, nrow = 1), state$p[-n, , drop = FALSE])-d;
    abs(sum(bad[bad<0]))
  })
  matrix(out, nrow = 11)
}
  
bestSide <- function(n,centres,objects,state) 
{
  # sum of distances that violate seperation
  bad <- dbodyObject(state$r[n], centres, objects) + dBodyBody(n, state, centres)
  # left bad - right bad
  l_r <- sum(bad[1:5,])-sum(bad[7:11,])
  if (l_r<0) return(-1) else # turn left
  if (l_r>0) return(-3) else # turn right
    return(-2)               # turn around
}



# Blocking goal -----------------------------------------------------------
 
nBlock <- function(n, state, r, usePredictedPos = FALSE) 
  # Number of pedestrians in the goal direction (goal cone or the cones on either
  # side of it) that are closer than the goal and can be seen.
  # returns number and if more than one attribute "ends" specifying blocking
  # profiles from current perspective
{
  p1 <- state$p[n, , drop = FALSE]
  # state$P[[n]][1:dim(state$P[[n]])[1],]
  P <- state$P[[n]][attr(state$P[[n]], "i"), 1:2, drop = FALSE]
  
  if (usePredictedPos) 
    p2 <- predictPed(state$p, state$v, state$a, state$cell)[-n, , drop = FALSE]

  p2 <- state$p[-n, , drop = FALSE]
  p2 <- p2[dist1(p1,p2) < dist1(p1,P),,drop=FALSE]  # nearer than goal
  p2 <- p2[seesMany(p1,p2,objects),,drop=FALSE]     # Can see

  if (dim(p2)[1] == 0) {
    return(0)
  }

  goalCone <- Iangle(p1, state$a[n], P)
  if (is.na(goalCone)) {
    return(0)
  }
  coneSet <- c(goalCone - 1, goalCone, goalCone + 1)
  coneSet <- coneSet[coneSet > 0 & coneSet < 12]

  ends <- eObjects(p1, p2, r)
  if (is.list(ends)) ends <- array(unlist(ends),dim=c(dim(ends[[1]]),2),
    dimnames=list(dimnames(ends[[1]])[[1]],dimnames(ends[[1]])[[2]],names(ends)))
  endCones <- apply(ends, 3, function(x) {
    Iangle(p1, state$a[n], x)
  })
  if (dim(ends)[1] == 1) {
    endCones <- matrix(endCones, nrow = 1,
                       dimnames = list(dimnames(p2)[1], NULL))
  }

  blockers <- apply(endCones, 1, function(x) {
    if (any(is.na(x))) {
      FALSE
    } else {
      any(x %in% coneSet)
    }
  })
  blockers <- names(blockers[blockers])
  
  # B<-state$p[blockers,]
  # for (i in 1:dim(B)[1]) points(B[i,1],B[i,2],pch=16,cex=2)
  
  nBlockers <- length(blockers)
  if (nBlockers > 0) {
    attr(nBlockers, "ends") <- ends[blockers, , , drop = FALSE]
  }
  return(nBlockers)
}



