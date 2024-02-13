##############################################################################-
# This file contains all blocking functions.
##############################################################################-


# Base blocking -----------------------------------------------------------

# Is xy inside or outside of the square
inObject <- function(xy, xlim, ylim, outside = TRUE) {
  ok <- (xy[1] > xlim[1]) & (xy[1] < xlim[2]) & (xy[2] > ylim[1]) & 
    (xy[2] < ylim[2])
  if (outside) {
    return(!ok)
  } else {
    return(ok)
  }
}

# Turns an rectangular object into a specification of its constituent lines
# Output a 2 (x/y) x 4 (lines) x 2 (begin/end point) array
object2lines <- function(o) {
  array(c(o$x[1], o$y[1], o$x[1], o$y[2], o$x[1], o$y[1], o$x[2], o$y[1],
          o$x[2], o$y[1], o$x[2], o$y[2], o$x[1], o$y[2], o$x[2], o$y[2]),  
        dim = c(2, 4, 2), dimnames = list(c("x", "y"), 
                                          c("L1", "L2", "L3", "L4"), 
                                          c("P1", "P2")))
}


# Cells OK ----------------------------------------------------------------

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

# For body radius r at each coordinate in centres is there overlap with any of 
# the constituent lines in oL that specify an object. 
# returns boolean vector, TRUE for cells with overlap
bodyObjectOverlap <- function(oL, r, okCentres) {
  # Right angles to each object line
  a <- unique((angle2(p1 = t(oL[, , "P1"]), p2 = t(oL[, , "P2"])) + 90) %% 180)
  
  # dx and dy to move along line
  x <- r * sin(a * pi / 180)
  y <- r * cos(a * pi / 180)
  
  # For each centre check for overlap
  apply(okCentres, 1, function(p) { 
    # Lines 
    segments <- array(c(p - rbind(x, y), p + rbind(x, y)), 
                      dim = c(2, length(x), 2),
                      dimnames = list(c("x", "y"), NULL, c("P1", "P2")))
    out <- FALSE 
    for (j in 1:dim(segments)[2]) {
      out <- out | any(sapply(1:(dim(oL)[2]), function(i){
        all(is.finite(line.line.intersection(segments[,j,"P1"], 
                                             segments[,j,"P2"], 
                                             oL[,i,"P1"], oL[,i,"P2"], TRUE)
        )) 
      }))
    }
    out
  })
}

# Is the cell OK or does body radius r positioned at centres overlap with an 
# object? 
bodyObjectOK <- function(r, centres, objects, ok) {
  if (!any(ok)) {
    return(NULL)
  }
  oLines <- lapply(objects, object2lines)
  
  # Does it overlap
  out <- !logical(33)
  out[ok] <- apply(matrix(unlist(
    lapply(oLines, bodyObjectOverlap, r = r,
           okCentres = centres[as.vector(ok), , drop = FALSE])),
    nrow = sum(ok)), 1, function(x) {
      any(x)
    })
  # If it doesn't overlap it is OK
  matrix(!out, nrow = 11)
}

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


# Blocking goal -----------------------------------------------------------

# Number of pedestrians in the goal direction (goal cone or the cones on either 
# side of it).
# returns number and if more than one attribute "ends" specifying blocking
# profiles from current perspective 
nBlock <- function(n, state, r, usePredictedPos = FALSE) {
  p1 <- state$p[n, , drop = FALSE]
  P <- state$P[[n]][attr(state$P[[n]], "i"), 1:2, drop = FALSE]
  
  if (usePredictedPos) {
    p2 <- predictPed(state$p, state$v, state$a, state$cell)[-n, , drop = FALSE] 
  } else {
    p2 <- state$p[-n, , drop = FALSE]
  }
  
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
  
  blockers <- blockers[blockers]
  nBlockers <- length(blockers)
  if (nBlockers > 0) {
    attr(nBlockers, "ends") <- ends[names(blockers), , , drop = FALSE]
  }
  return(nBlockers)
}

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

