##############################################################################-
# This file contains all functions to predict positions of pedestrians.
##############################################################################-


# Predict next position of pedestrians on current heading. If stayStop
# pedestrians who have just turned or stopped predicted to stay stopped.
predictPed <- function(p, v, a, cell = NULL, stayStop = TRUE) {
  out <- p + scaleVel(v) * aTOd(a)
  if (stayStop & !is.null(cell)) {
    out[cell < 1, ] <- p[cell < 1, ]
  }
  out
}

# Position P (start position) not occupied or predicted to be occupied, where
# occupied = pedestrian bodies touch, i.e., sum of radii
occupied <- function(P, r, state) {
  any(dist1(P, state$p) < (r + state$r)) | 
    any(dist1(P, predictPed(state$p, state$v, state$a, state$cell)) < 
          (r + state$r))
}

# Dummy, used to collect information about interactions for current state
# @CT can probably be removed when deleted from other functions
getiInfo <- function(state) {
  NULL
}
