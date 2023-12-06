# Turns an rectangular object into a specification of its constituent lines
# Output a 2 (x/y) x 4 (lines) x 2 (begin/end point) array
object2lines <- function(o) {
  array(c(o$x[1], o$y[1], o$x[1], o$y[2], o$x[1], o$y[1], o$x[2], o$y[1],
          o$x[2], o$y[1], o$x[2], o$y[2], o$x[1], o$y[2], o$x[2], o$y[2]),  
        dim = c(2, 4, 2), dimnames = list(c("x", "y"), 
                                          c("L1", "L2", "L3", "L4"), 
                                          c("P1", "P2")))
}