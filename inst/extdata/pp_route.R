##############################################################################-
# This file contains all routing functions. 
##############################################################################-


# Picks n goals (x,y coords) with probability proportional to segment length  
pickGoals <- function(n, goalLines, goalRectangles, objects) {
  
  # Contains the coordinates for creating a rectangle with 4 lines. 
  # Rectangles are actually squares with length = r
  rect2line <- function(r) {
    list(list(x = c(r$x[1], r$x[1]), y = r$y),
         list(x = c(r$x[2], r$x[2]), y = r$y),
         list(x = r$x, y =c (r$y[1], r$y[1])),
         list(x = r$x, y = c(r$y[2], r$y[2])))
  }
  
  # Computes the length of a line by differencing the coordinates:
  # sqrt( (x - x)^2 + (y - y)^2 )
  #
  # Why not distance as a function?
  lLength <- function(l) {
    sqrt(sum(abs(unlist(lapply(l, diff)))^2))
  }
  
  # Pick a random point on line l: Pretty clear with the runif
  pickLinePoint <- function(l) {
    pt <- runif(1)
    unlist(lapply(l, function(d) {
      min(d) + pt * abs(diff(d))
    }))
  } 
  
  # Move pt outside of its grid
  moveOutside <- function(pt, objects, eta = 1e-6) {
    
    # Check whether person is inside bounds
    inon <- function(xy, xlim, ylim, outside = TRUE) {
      (xy[1] >= xlim[1]) & (xy[1] <= xlim[2]) & (xy[2] >= ylim[1]) & 
        (xy[2] <= ylim[2])
    }
    
    # Determine the grid the person is on
    on <- objects[-1][unlist(lapply(objects[-1], function(x){
      inon(as.numeric(pt), x$x, x$y)
    }))]

    # A round of checks to move the person off off 
    # its current place
    okp <- pt + c(eta, 0)
    if (!inon(as.numeric(okp), on[[1]]$x, on[[1]]$y)) {
      return(okp)
    }
    okp <- pt + c(-eta, 0)
    if (!inon(as.numeric(okp), on[[1]]$x, on[[1]]$y)) {
      return(okp)
    }
    okp <- pt + c(0, eta)
    if (!inon(as.numeric(okp), on[[1]]$x, on[[1]]$y)) {
      return(okp)
    }
    okp <- pt + c(0, -eta)
    if (!inon(as.numeric(okp), on[[1]]$x, on[[1]]$y)) {
      return(okp)
    }
    error("Could not move goal outside object")
  }
  
  # Create the rectangles
  allLines <- c(goalLines, do.call(c, lapply(goalRectangles, rect2line)))

  # Get the lengths of those rectangles
  lens <- unlist(lapply(allLines, lLength))

  # Determine which grids can be picked to move to
  picks <- .bincode(runif(n), c(0, cumsum(lens / sum(lens))))

  # Pick one of these
  out <- data.frame(do.call(rbind, lapply(allLines[picks], pickLinePoint)))
  row.names(out) <- paste0("G", 1:dim(out)[1])
  
  # Move goal outside objects
  data.frame(t(apply(out, 1, moveOutside, objects = objects)))
}

# Get horizontal aisle are from two bounding shelf objects
getAisle <- function(o1, o2) {
  out <- o1
  o1above <- o1$y[1] > o2$y[1]
  if (o1above) {
    out$y <- sort(c(o1$y[1], o2$y[2])) 
  } else
    out$y <- sort(c(o2$y[1], o1$y[2]))
  out
}

# Does the path from p_n to P_n pass through each oneWay area?
passThrough <- function(p, P, oneWay) {
  
  # Line between passes through or both points inside obj
  intersects <- function(obj, p, P) { 
    allInside <- inObject(p, obj$x, obj$y, outside = FALSE) & 
      inObject(P, obj$x, obj$y, outside = FALSE)
    names(allInside) <- NULL
    any(is.finite(c(line.line.intersection(p, P, c(obj$x[1], obj$y[1]), 
                                           c(obj$x[2], obj$y[1]), 
                                           interior.only = TRUE),
                    line.line.intersection(p, P, c(obj$x[1], obj$y[1]), 
                                           c(obj$x[1], obj$y[2]), 
                                           interior.only = TRUE),
                    line.line.intersection(p, P, c(obj$x[1], obj$y[2]), 
                                           c(obj$x[2], obj$y[2]), 
                                           interior.only = TRUE),
                    line.line.intersection(p, P, c(obj$x[2], obj$y[1]), 
                                           c(obj$x[2], obj$y[2]), 
                                           interior.only = TRUE)
    ))) | allInside
  }
  
  unlist(lapply(oneWay, intersects, p = p, P = P))  
}

# Probabilistic decision to re-route, where probability of re-routing is a 
# function of the number of pedestrians in the goal cone or the 
# cones on either side of it. pReroute is number of blockers to produce p=.5
p_reroute <- function(nBlock, pReroute = 10) {
  # runif(1) < 1 - (1 - pReroute)^nBlock
  pnorm(nBlock-pReroute)
}


# Edges -------------------------------------------------------------------

# Takes ps, a matrix of xy cordinate columns, with named rows for entry (G0), 
# exit ("Gend"), mustVisit goals (G1, G2, ...), path points (P1, P2, ...), 
# objects (used to figure out which points can see each other with seesGoal), 
# and returns cppRouting edges makegraph object with distances (optionally 
# plots with iGraph)
# makeGraph = FALSE returns raw (one way) point1, point2, 
# distance connection matrix
# oneWay specifies regions where flow is one way
getEdges <- function(ps, objects, plotIt = FALSE, main = "", makeGraph = TRUE,
                     oneWay = NULL, pointcol = "#92C5DE", 
                     linkcol = "#969696") {
  
  # Matrix to store which points can be seen from each other and their distance
  out <- matrix(nrow = 0, ncol = 3, 
                dimnames = list(NULL, c("from", "to", "dist")))
  nams <- row.names(ps)
  
  # For each combination of points
  for (i in 1:(dim(ps)[1] - 1)) {
    for (j in (i + 1):dim(ps)[1]) {
      # Check if there is a line of sight between points
      if (seesGoal(as.numeric(ps[i, ]), as.numeric(ps[j, ]), objects)) {
        # Compute distance between points
        d <- sqrt(sum((ps[i, ] - ps[j, ])^2))
        
        # Check if point i is left of point j
        left1 <- ps[i, 1] < ps[j, 1]
        
        # Create two entries LR and RL with distance
        if (left1) {
          LR <- c(nams[i], nams[j], d)
          RL <- c(nams[j], nams[i], d)
        } else {
          RL <- c(nams[i], nams[j], d)
          LR <- c(nams[j], nams[i], d)
        }
        
        # Check if there are one-way directions
        if (!is.null(oneWay)) {
          inAisle <- passThrough(oneWay, p = as.numeric(ps[i, ]), 
                                 P = as.numeric(ps[j, ]))
          if (!any(inAisle)) {
            badLR <- badRL <- FALSE 
          } else {
            badLR <- any(names(inAisle[inAisle]) == "<") 
            badRL <- any(names(inAisle[inAisle]) == ">") 
          }
        } else { 
          badLR <- badRL <- FALSE
        }
        if (badLR) {
          LR <- NULL 
        }
        if (badRL) {
          RL <- NULL 
        }
        out <- rbind(out, LR, RL)
      }
    }
  }
  
  # Convert out to data frame and make distances numeric
  out <- data.frame(out, stringsAsFactors = FALSE)
  out$dist <- as.numeric(out$dist)
  
  # Make and plot graph
  if (makeGraph) {
    out <- makegraph(out, directed = TRUE,
      coords = cbind.data.frame(node_ID = row.names(ps),X = ps$x, Y = ps$y))
    if (plotIt) {
      igr1 <- graph_from_data_frame(out$data)
      plot.igraph(igr1, edge.arrow.size = .3, main = main, 
                  vertex.color = pointcol,
                  vertex.frame.color = pointcol,
                  vertex.label.family = "Helvetica",
                  vertex.label.cex = 1.2,
                  edge.color = linkcol,
                  edge.width = 1)
    }
  }
  
  return(out)
}

# Adds edges for new point P0 to existing edges object, except for any goals in 
# bad
addEdge <- function(P0, edges, objects, oneWay = NULL) {
  out <- matrix(nrow = 0, ncol = 3, 
                dimnames = list(NULL, c("from", "to", "dist")))
  ok <- substr(edges$dict$ref, 1, 1) != "G"
  ps <- edges$coords[ok, -1]
  nams <- edges$coords[ok, 1]
  for (j in 1:dim(ps)[1]) {
    if (seesGoal(P0, as.numeric(ps[j, ]), objects)) {
      d <- sqrt(sum((P0 - ps[j, ])^2))
      left1 <- P0[1] < ps[j, 1]
      if (left1) {
        LR <- c("P0", nams[j], d)
        RL <- c(nams[j], "P0", d)
      } else {
        RL <- c("P0", nams[j], d)
        LR <- c(nams[j], "P0", d)
      }
      if(!is.null(oneWay)) {
        inAisle <- passThrough(oneWay, p = P0, P = as.numeric(ps[j, ]))
        if (!any(inAisle)) {
          badLR <- badRL <- FALSE 
        } else {
          badLR <- any(names(inAisle[inAisle]) == "<") 
          badRL <- any(names(inAisle[inAisle]) == ">") 
        }
      } else {
        badLR <- badRL <- FALSE
      }
      if (badLR) {
        LR <- NULL 
      }
      if (badRL) {
        RL <- NULL 
      }
      out <- rbind(out, LR, RL)
    }
  }
  out <- data.frame(out, stringsAsFactors = FALSE)
  row.names(out) <- NULL
  out$dist <- as.numeric(out$dist)
  edges$dict <- rbind(edges$dict, c(0, edges$nbnode)) 
  edges$nbnode <- edges$nbnode + 1
  edges$dict[edges$nbnode, 1] <- "P0"
  edges$coords <- rbind(edges$coords, c(0, P0))
  edges$coords[edges$nbnode, 1] <- "P0"
  dict <- as.numeric(edges$dict[, 2])
  names(dict) <- edges$dict[, 1]
  out$to <- dict[out$to]
  out$from <- dict[out$from]
  edges$data <- rbind(edges$data, out)
  
  edges
}

# Remove edges that intersect blocker profiles
removeEdges <- function(edges, nBlockers) {
  
  blocked <- function(P12, ends, coords) {
    
    isIntersect <- function(L2, P1, P2) {
      any(is.finite(line.line.intersection(P1, P2, P3 = L2[, 1], P4 = L2[, 2], 
                                           interior.only = TRUE)))
    }
    
    any(apply(ends, 1, isIntersect,
              P1 = as.numeric(coords[coords[, 1] == as.character(P12[1]),2:3]),
              P2 = as.numeric(coords[coords[, 1] == as.character(P12[2]),2:3])))
  }
  
  if (nBlockers == 0) {
    return(edges) 
  } else {
    ends <- attr(nBlockers, "ends")
    dict <- edges$dict
    row.names(dict) <- dict[, 2]
    data <- edges$data
    data$from <- dict[as.character(data$from), "ref"]
    data$to <- dict[as.character(data$to), "ref"]
    edges$data <- edges$data[!apply(data, 1, blocked, ends = ends, 
                                    coords = edges$coords), ]
    return(edges)
  }
}


# Paths -------------------------------------------------------------------

# Remove other goals 
removeGoals <- function(edge, ft, gnams = NULL) {
  if (is.null(gnams)) {
    gnams <- edge$dict$ref
    gnams <- gnams[substr(gnams, 1, 1) == "G"]
  }
  bad <- edge$dict[edge$dict$ref %in% gnams[!(gnams %in% ft)], "id"] 
  edge$data <- edge$data[!(edge$data$from %in% bad | edge$data$to %in% bad), ]
  return(edge)
}

# Gets shortest paths between goals using cppRouting 
getGoalPaths <- function(edge, algorithm = "bi") {
  
  # Get names of goals and pathpoints
  gnams <- edge$dict$ref
  
  # Select goals, all names starting with "G"
  gnams <- gnams[substr(gnams, 1, 1) == "G"]
  
  # Matrix with combination of goals in both directions, e.g., G0 G1 and G1 G0
  pnams <- outer(gnams, gnams, paste)
  
  # Remove diagonal (link to self)
  pnams <- c(pnams[upper.tri(pnams)], pnams[lower.tri(pnams)])
  
  # Split at " " to separate from and to
  from <- strsplit(pnams, " ")
  to <- unlist(lapply(from, function(x) {
    x[2]
  }))
  from <- unlist(lapply(from, function(x) {
    x[1]
  }))
  fromto <- cbind(from, to)
  
  # Find shortest paths between each combination
  out <- apply(fromto, 1, function(x) {
    suppressMessages(unlist(get_path_pair(removeGoals(edge, x, gnams),
                                              from = x[1], to = x[2], 
                                              algorithm = algorithm), 
                                use.names = FALSE))
  }, simplify = FALSE)
  # suppressMessages(out <- get_path_pair(edge,from,to,algorithm))  
  # out <- lapply(out,function(x){x[length(x):1]})
  attr(out, "gnams") <- gnams
  out
}

# Makes a symmetric matrix of shortest path distances between goals
# obtained using getGoalPaths  
# Can insert dummy node to enforce start (G0) and end (Gend) points
getGoalDists <- function(edge, doDummy = TRUE, algorithm = "bi") {
  goalPaths <- getGoalPaths(edge, algorithm = algorithm)
  dists <- unlist(lapply(goalPaths, function(x) {
    dists <- numeric(length(x) - 1)
    for (i in 1:(length(x) - 1)) {
      dists[i] <- edge$data[edge$data$from ==
                              edge$dict$id[edge$dict$ref == x[i]] &
                              edge$data$to ==
                              edge$dict$id[edge$dict$ref == x[i + 1]], "dist"]
    }
    sum(dists)
  }))
  
  gnams <- attr(goalPaths, "gnams")
  out <- matrix(nrow = length(gnams), ncol = length(gnams), 
                dimnames = list(gnams, gnams))
  out[upper.tri(out)] <- dists[1:sum(upper.tri(out))]
  out[lower.tri(out)] <- dists[-c(1:sum(upper.tri(out)))]
  diag(out) <- 0
  if (doDummy) {
    r <- rep(Inf, dim(out)[2])
    G0 <- colnames(out) == "G0"
    Gend <- colnames(out) == "Gend"
    r[G0] <- 0
    r[Gend] <- 0 
    out <- rbind(out, Dummy = r)
    out <- cbind(out, Dummy = c(r, 0))
  }
  out
}

# Takes in distance matrix and either 1) uses chain of closest goal starting
# from G0 (greedy=TRUE) or 2) finds finds a tour using the TSP package,   
# nearestFirst removes G0 and replaces with nearest to G0
# pSwap = probability of swapping two elements in 
# If originalOrder = TRUE, no routing algorithm is used and the in dmat is used
# Returns a name vector, which is ordered from "G0" to "Gend"
getTour <- function(dmat, pSwap = 0, originalOrder = FALSE, greedy = TRUE, 
                    nearestFirst = TRUE, methodTSP = "cheapest_insertion", 
                    two_opt = TRUE) {
  
  # Always takes next closest
  greedyTSP <- function(dmat) {
    
    gnams <- row.names(dmat)[!(row.names(dmat) %in% c("Dummy", "G0", "Gend"))]
    out <- "G0"
    use <- gnams
    for (i in 1:length(gnams)) {
      d <- dmat[out[i], ][use]
      out <- c(out, names(d)[which.min(d)])
      use <- use[use != out[i + 1]]
    }
    c(out, "Gend")
  }
  
  if (originalOrder) {
    gnams <- row.names(dmat)[!(row.names(dmat) %in% c("Dummy", "G0", "Gend"))]
    out <- c("G0", gnams, "Gend")
    return(out)
  }
  
  if (greedy) {
    out <- greedyTSP(dmat) 
  } else {
    if (nearestFirst) {
      nearest <- dmat[dimnames(dmat)[[1]] == "G0",
                      !(dimnames(dmat)[[1]] %in% c("Dummy", "G0"))]
      nearest <- names(nearest[nearest == min(nearest)])
      dmat <- dmat[dimnames(dmat)[[1]] != "G0", dimnames(dmat)[[1]] != "G0"]
      dmat[nearest, "Dummy"] <- dmat["Dummy", nearest] <- 0
    }
    tour <- solve_TSP(ETSP(dmat), method = methodTSP, two_opt = two_opt)
    if (any(dimnames(dmat)[[1]] == "Dummy")) {
      if (any(dimnames(dmat)[[1]] == "G0")) {
        G0 <- "G0" 
      } else {
        G0 <- dmat["Dummy", dmat[, "Dummy"] == 0] 
        G0 <- names(G0[!(names(G0) %in% c("Gend", "Dummy"))])
      }
      tnams <- names(tour)[!(names(tour) %in% c("Gend", "Dummy"))]
      G0pos <- c(1:length(tnams))[tnams == G0]
      if (G0pos == length(tnams)) { 
        out <- c(G0, tnams[1:(length(tnams) - 1)], "Gend") 
      } else {
        if (G0pos == 1) {
          out <- c(tnams, "Gend") 
        } else {
          out <- c(tnams[G0pos:length(tnams)], tnams[1:(G0pos - 1)], "Gend")
        }
      }
      if (G0 != "G0") out <- c("G0",out)
    } else out <- names(tour)
  }
  if (pSwap > 0) {
    for (i in 2:(length(out) - 1)) {
      if (runif(1) < pSwap) {
        j <- sample(2:(length(out) - 1), 1) 
      } else {
        j <- i
      }
      out[c(i, j)] <- out[c(j, i)]  
    }
  }
  out
}

# Expands tour to goal stack and distances. 
# If expansion fails returns returns NULL
expandTour <- function(Tour, edge, objects = NULL, pDetour = 0) {
  for (i in 1:(length(Tour) - 1)) {
    suppressMessages(path <- (get_path_pair(
      removeGoals(edge,c(Tour[i],Tour[i + 1])),Tour[i],Tour[i + 1])[[1]]))
    if (length(path) > 2 && i != (length(Tour) - 1) && runif(1) < pDetour) {
      detours <- edge$dict$ref[!(edge$dict$ref %in% c(path, "G0", "Gend")) &
                                 substr(edge$dict$ref, 1, 1) != "G"]
      newPath <- vector(mode = "list", length = length(path) - 1)
      for (j in 1:(length(path) - 2)) {
        if (length(detours) != 0) {
          detoursj <- detours[
            seesMany(as.numeric(edge$coords[edge$coords$node_ID == path[j], 
                                            -1]),
                     as.matrix(edge$coords[edge$coords$node_ID %in% detours, 
                                           -1, drop = FALSE]), objects) &
              seesMany(as.numeric(edge$coords[edge$coords$node_ID == 
                                                path[j + 1], -1]),
                       as.matrix(edge$coords[edge$coords$node_ID %in% detours, 
                                             -1, drop = FALSE]), objects)]
          if (length(detoursj) != 0) {
            pick <- sample.int(length(detoursj), 1)
            newPath[[j]] <- c(path[j], detoursj[pick])
            detours <- detours[detours != detoursj[pick]]
          } else {
            newPath[[j]] <- path[j]
          }
        } else {
          newPath[[j]] <- path[j]
        }
      }
      path <- c(unlist(newPath), path[(length(path) - 1):length(path)])
    }
    outi <- t(sapply(path, function(x) { 
      edge$coords[edge$coords$node_ID == x, 2:3]
    }))
    if (i == 1) {
      out <- outi 
    } else {
      out <- rbind(out, outi[-1, , drop = FALSE])
    }
  }
  if (dim(out)[2]==0) return(NULL) # Expansion not possible
  mat <- cbind(x = unlist(out[, 1]), y = unlist(out[, 2]))
  cbind(mat, dist = c(NA, dist(mat[-dim(mat)[1], ], mat[-1, ])))
}


# Goal stacks -------------------------------------------------------------

# Make and optionally show a goal stack visiting n goals (can pass in goals as 
# n too) 

# n=nGoals[i]; pSwap = pSwap[i];pDetour = pDetour[i];
# oneWay = switch(oneWay[i] + 1, NULL, oneWay1,oneWay2)
# exit = switch(exit[i], exitLow, exitHigh)
# 
# showTour = FALSE;tourDelay = 3;pdfName = NULL

plot_tour <- function(tour,goals,objects,entry,exit,pathPoints,oneWay=0,tourDelay=1) {
  plotSpace(objects)
  points(rbind(entry, exit), pch = "x")
  points(pathPoints)
  points(goals, pch = c(LETTERS, letters))
  if (!is.null(oneWay)) {
    drawOneWay(oneWay)
  }
  Sys.sleep(tourDelay)
  drawTour(tour, edges, delay = tourDelay)
  text(8, 24, paste("Path = ", round(sum(tour[, 3], na.rm=TRUE)), "m"), 
       cex = 1.5)
}


getGoalStack <- function(n, objects,  # n goals in space defined by objects
                         # Possible goal locations, only if goals still need to be picked
                         goalLines = NULL, goalRectangles = NULL,                      
                         # Way finding inputs
                         entry, exit, pathPoints,                         
                         # Way finding options
                         algorithm = "bi", oneWay = NULL, plotGraph = FALSE,
                         # If originalOrder = TRUE then order goals is kept
                         originalOrder = FALSE,
                         # greedy tour, p swap solutions
                         greedy = TRUE, pSwap = 0,                           
                         # If not greedy, TSP solution options
                         methodTSP = "cheapest_insertion", two_opt = TRUE,
                         # Insert random points in shortest path
                         pDetour = 0,                                     
                         # Display graphic of tour
                         showTour = FALSE, tourDelay = 3,pdfName = NULL,
                         tour=NULL) {
  # If n is # goals, pick goals randomly from goalLines and goalRectangles
  if (length(n) == 1) {
    goals <- pickGoals(n, goalLines, goalRectangles, objects)  
    # If n already is the locations of the goals
  } else {
    # Remove the third column if it exists (distance)
    goals <- n[substr(row.names(n), 1, 1) == "G" & 
                 !(row.names(n) %in% c("G0", "Gend")), -3]
    # Only keep the unique goal ID's
    goals <- goals[unique(row.names(goals)), ]
  }

  # Get all edges and their distances between goals and pathpoints
  edges <- getEdges(ps = rbind(G0 = entry, Gend = exit, goals, pathPoints), 
                    objects, plotIt = plotGraph, oneWay = oneWay)
  
  # Get all shortest distances between goals 
  # (can require visiting multiple pathpoints between goals)
  dmat <- getGoalDists(edges, algorithm = algorithm)
  
  # Get the order of goals to visit
  tour <- getTour(dmat, originalOrder = originalOrder, greedy = greedy, 
                  pSwap = pSwap, methodTSP = methodTSP, two_opt = two_opt)
  
  # Add path points and possibly detours to the goal tour vector and
  # returns as matrix with x, y and dist
  tour <- expandTour(tour, edges, objects, pDetour)
  
  if (showTour) {
    plotSpace(objects)
    points(rbind(entry, exit), pch = "x")
    points(pathPoints)
    points(goals, pch = c(LETTERS, letters))
    if (!is.null(oneWay)) {
      drawOneWay(oneWay)
    }
    Sys.sleep(tourDelay)
    drawTour(tour, edges, delay = tourDelay)
    text(8, 24, paste("Path = ", round(sum(tour[, 3], na.rm=TRUE)), "m"), 
         cex = 1.5)
  }
  
  # Add edges and oneway to replan attribute
  attr(tour, "replan") <- list(edges = edges, oneWay = oneWay)
  invisible(list(tour=tour,goals=goals))
}



# Convenience function for multiple calls to getGoalStack
# Makes stacks of nGoals goals + beginning and end goals + way points
# Assumes objects, goalLines, goalRectangles, entry, exit, pathPoints, oneWay1, 
# oneWay2 in environment
makeSupermarketGoalStacks <- function(nGoals,
                                      # Probability of swapping points on tour
                                      pSwap = 0,          
                                      # Insert random points in shortest path
                                      pDetour = 0,        
                                      # 0: 2way, 1/2: alternative 1way configs
                                      oneWay = c(0:2)[1],
                                      # Coordinates of entry
                                      entry,
                                      # 1 = exitLow, 2 = exitHigh
                                      exit = c(1, 2)[1],
                                      verbose = FALSE) {
  n <- length(nGoals)
  oneWay <- rep(oneWay, length.out = n)
  exit <- rep(exit, length.out = n)
  pSwap <- rep(pSwap, length.out = n)
  pDetour <- rep(pDetour, length.out = n)
  stacks <- vector(mode = "list", length = n)
  if (verbose & n>100) cat("Each . indicates 100 goals created out of",n,"\n")
  for (i in 1:n) {
    stacks[[i]] <- getGoalStack(nGoals[i], pSwap = pSwap[i], 
                                pDetour = pDetour[i],
                                oneWay = switch(oneWay[i] + 1, NULL, oneWay1, 
                                                oneWay2),
                                exit = switch(exit[i], exitLow, exitHigh),
                                objects = objects, goalLines = goalLines, 
                                goalRectangles = goalRectangles, entry = entry, 
                                pathPoints = pathPoints)
    attr(stacks[[i]], "stop") <- 0  
    attr(stacks[[i]], "i") <- 2
    if (verbose & (i %% 100 == 0)) cat(".")
  }
  if (verbose) cat("\n")
  return(stacks)
}
