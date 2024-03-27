##############################################################################-
# This file contains all geometry functions.
##############################################################################-


# Distance ----------------------------------------------------------------

# Distance from p1 to p2, same sized xy column matrices
dist <- function(p1, p2) {
  sqrt(apply((p1 - p2)^2, 1, sum))
}

# distance from p1 to p2, p1 is a single point, p2 is a matrix
dist1 <- function(p1, p2) {
  sqrt(apply((t(p2) - as.vector(p1))^2, 2, sum))
}


# Angle -------------------------------------------------------------------

# Shortest angle anti-clockwise from p1 as orign to p2 (> -180 to 180)
angle2s <- function(p1, p2) {
  round((atan2(p2[, 2] - p1[, 2], p2[, 1] - p1[, 1]) * 180 / pi), 10)
}

# Example
# angle2s(p1=matrix(rep(c(0,0),each=8),nrow=8),
#        p2=matrix(c(1,1,0,-1,-1,-1,0,1,0,1,1,1,0,-1,-1,-1),nrow=8))

# Anti-clockwise angle from p1 as origin to p2 (x,y pairs matrices) 0 to <360
angle2 <- function(p1, p2) {
  round((atan2(p2[, 2] - p1[, 2], p2[, 1] - p1[, 1]) * 180 / pi), 10) %% 360
}

# Example
# angle2(p1=matrix(rep(c(0,0),each=8),nrow=8),
#        p2=matrix(c(1,1,0,-1,-1,-1,0,1,0,1,1,1,0,-1,-1,-1),nrow=8))

# Angle to L2 normalized carteisn direction, so sqrt(d(x)^2+d(y)^2)=1)  
aTOd <- function(a) {
  cbind(x = cos(a * pi / 180), y = sin(a * pi / 180))
}

# Which angle cone (1..11, NA means outside of view) is p2 in relative to 
# p1 heading at angel a1 
Iangle <- function(p1, a1, p2, border = c(-85, -60, -40, -25, -15, -5, 5, 15, 
                                          25, 40, 60, 85)) {
  
  tomp <- function(x) { # -180 ... 180
    neg <- x < -180
    x[neg] <- 360 + x[neg]
    
    pos <- x > 180
    x[pos] <- x[pos] - 360
    
    x
  }
  
  a <- (angle2(p1, p2) - a1)
  
  out <- 12 - .bincode(tomp(a), border)
  names(out) <- row.names(p2)
  out
}  

# Angle to destination for all pedestrians
Dn <- function(p_n, P_n) {
  out <- numeric(dim(p_n)[1])
  for (i in 1:dim(p_n)[1]) {
    out[i] <- angle2(p_n[i, , drop = FALSE], P_n[i, , drop = FALSE])
  }
  out
}

# Shortest absolute angle between a1 and a2
minAngle <- function(a1,a2) {
  pmin(abs(a1 - a2), abs(pmin(a1, a2) + (360 - pmax(a1, a2))))
}

# Absolute angular difference between 11 directions with zero cone having
# angle a1 and a2
headingAngle <- function(a2, a1, angles = c(72.5, 50, 32.5, 20, 10, 0, 350, 
                                            340, 327.5, 310, 287.5)) {
  sapply((angles + a1) %% 360, minAngle, a2 = a2)  
}


# Velocity ----------------------------------------------------------------

# Scale velocity by time step
scaleVel <- function(v, tStep = 0.5) {
  v * tStep  
}


# Position ----------------------------------------------------------------

# Calcualte cell centres for set of cells (index 1..33) for p1 heading at 
# velocity v1 at angle a1 given time step tStep seconds.
c_vd <- function(cells, p1, v1, a1,
                 vels = matrix(rep(c(1.5, 1, .5), each = 11), ncol = 3),
                 angles = matrix(rep(c(72.5, 50, 32.5, 20, 10, 0, 350, 340, 
                                       327.5, 310, 287.5), times = 3), 
                                 ncol = 3)) {
  t(p1 + t(scaleVel(v1) * vels[cells] * aTOd((angles[cells] + a1) %% 360)))
}


# Cones and rings ---------------------------------------------------------

# k in 1..33
coneNum <- function(k) {
  1 + ((k-1) %% 11)
}

ringNum <- function(k) {
  1 + (k-1) %/% 11
}


