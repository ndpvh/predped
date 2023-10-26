##############################################################################-
# This file contains all plotting functions.
##############################################################################-


# Draw shapes -------------------------------------------------------------

# Augment plotrix function to allow removal of correction for plot aspect ratio
draw_circle <- function (x, y, radius, nv = 100, border = NULL, col = NA, 
                         lty = 1, density = NULL, angle = 45, lwd = 1, 
                         correctAspect = FALSE) {
  
  getYmult <- function () {
    if (dev.cur() == 1) {
      warning("No graphics device open.")
      ymult <- 1
    } else {
      xyasp <- par("pin")
      xycr <- diff(par("usr"))[c(1, 3)]
      ymult <- xyasp[1] / xyasp[2] * xycr[2] / xycr[1]
    }
    return(ymult)
  }
  
  xylim <- par("usr")
  plotdim <- par("pin")
  
  if (correctAspect) {
    ymult <- getYmult() 
  } else {
    ymult <- 1
  }
  angle.inc <- 2 * pi / nv
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
  if (length(col) < length(radius)) {
    col <- rep(col, length.out = length(radius))
  }
  for (circle in 1:length(radius)) {
    xv <- cos(angles) * radius[circle] + x
    yv <- sin(angles) * radius[circle] * ymult + y
    polygon(xv, yv, border = border, col = col[circle], lty = lty, 
            density = density, angle = angle, lwd = lwd)
  }
  invisible(list(x = xv, y = yv))
}

# Draw a square
drawSquare <- function(lims, col = "#BDBDBD", border = "#BDBDBD", lwd = 1) {
  rect(lims$x[1], lims$y[1], lims$x[2], lims$y[2], 
       col = col, 
       border = border, 
       lwd = lwd)
}

# Draw choice grid for p1 heading at angle a1 with velocity v1
draw_grid <- function(p1, v1, a1, plotPoints = FALSE, Pr = NULL, 
                      col = "lightgrey", 
                      vels = rep(c(1.75, 1.25, .75), each = 12),
                      angles = rep(c(85, 60, 40, 25, 15, 5,355, 345, 335, 320, 
                                     300, 275), times = 3)) {
  ends <- t(p1 + t(scaleVel(v1) * vels * aTOd((angles + a1) %% 360)))
  for (i in 1:12) {
    lines(c(p1[1], ends[i, 1]), c(p1[2], ends[i, 2]), col = col)
  }
  lines(c(p1[1], ends[1,1]), c(p1[2], ends[1, 2]), col = col)
  for (i in 2:12) {
    lines(c(p1[1], ends[i,1]), c(p1[2], ends[i, 2]), col = col)
    for (j in 1:3) {
      lines(c(ends[(j - 1) * 12 + (i - 1), 1], ends[(j - 1) * 12 + i, 1]),
            c(ends[(j - 1) * 12 + (i - 1), 2], ends[(j - 1) * 12 + i, 2]), 
            col = col)
    }
  }
  if (plotPoints) {
    if (!is.null(Pr)) {
      cexs <- Pr * 3 
    } else {
      cexs <- rep(.25, 33) 
    }
    points(c_vd(1:33, p1, v1, a1), pch = 16, cex = cexs)
  }
}


# Draw pedestrians --------------------------------------------------------

# Draws grid aligned with measurement units
myGrid <- function(obj1, dx = 0.5, dy = 0.5, lty = "dotted", col = "#D9D9D9") {
  
  # Draw vertical lines grid
  for (x in seq(ceiling(obj1$x[1]), floor(obj1$x[2]), by = dx)) {
    lines(x = c(x, x), y = c(obj1$y[1], obj1$y[2]), lty = lty, col = col)
  }
  
  # Draw horizontal lines grid
  for (y in seq(ceiling(obj1$y[1]), floor(obj1$y[2]), by = dy)) {
    lines(x = c(obj1$x[1], obj1$x[2]), y = c(y, y), lty = lty, col = col)
  }
}

# Plot pedestrian circles, optionally with arrows indicating goal direction and 
# circle indicating social distance. If stationary draws grey, if 
# turning around draws red
plotPed <- function(p_n, P_n, a_n, r_n, objects, cells = rep(1, length(a_n)), 
                    socialDistance = 1.5, len = .05, amult = 1, plotGrid = TRUE, 
                    Pr = NULL, plotDirectionArrow = TRUE, plotDestArrow = TRUE, 
                    plotCircle = FALSE, fname = "",types=NULL,stops=NULL,
                    body_border="grey") {
  
  if (fname != "") {
    pdf(paste(fname, ".pdf", sep = ""), width = 20, height = 12)
  }
  
  par(mfrow = c(1, 1))
  nams <- unlist(lapply(strsplit(row.names(p_n), "_"), function(x) {
    x[1]
  }))
  
  # Plot space
  plotSpace(objects, plotGrid = plotGrid)
  
  for (i in 1:nrow(p_n)) {
    if (cells[i] == 0) {
      if (is.null(stops) || !stops[i])
        col <- "lightgrey" else col <- "pink" 
    } else {
      if (cells[i] < 0) {
        col <- "red" 
      } else {
        col <- NA
      }
    }
    draw_circle(p_n[i, 1], p_n[i, 2], r_n[i], border = body_border, col = col)
    if (plotCircle)
      draw_circle(p_n[i, 1], p_n[i, 2], socialDistance / 2, border = "blue")
  }
  
  
  # Plot direction arrow
  end <- p_n + amult*(aTOd(a_n) / 2)
  if (plotDirectionArrow) {
    for (i in 1:length(a_n)) {
      arrows(p_n[i, 1], p_n[i, 2], end[i, 1], end[i, 2], len = len,
             col = "#252525")
    }
  }
  
  # Plot destination arrow
  a_n <- Dn(p_n, P_n)
  end <- p_n + amult*(aTOd(a_n) / 2)
  if (plotDestArrow) {
    for (i in 1:length(a_n)) {
      arrows(p_n[i, 1], p_n[i, 2], end[i, 1], end[i, 2], len = len, 
             col = "#CA0020")
    }
  }
  
  # Add letters for peds
  if (is.null(types)) points(p_n, pch = nams) else 
    points(p_n, pch = nams, col=types)
  
  
  if (fname != "") {
    dev.off()
  }
}


# Animation ---------------------------------------------------------------

# Setup for figure and trace storage @CT different structure now
makeDirectories <- function(nam, outputName = "output", doPDF = FALSE) {
  if (!dir.exists(outputName)) {
    dir.create(outputName)
  }
  if (doPDF) {
    plot_dir <- paste0(outputName, "/", nam, "/")
    if (!dir.exists(plot_dir)) {
      dir.create(plot_dir)
    }
  }
}

# # If nam is a character reads in pdfs else nam is pdfs
# # Saves out animated gif, invisibly returns
# makeGifs <- function(nam, fnam = "output", density = 45, fps = 4, 
#                      useNam = TRUE, fnamOut = TRUE) {
#   
#   # Read in a bunch of pdfs, density sets size
#   getPDFs <- function(dName =".", density = 45) {
#     iNames <- dir(dName, pattern = "*.pdf")
#     
#     # Put in numeric order
#     iNames <- iNames[order(as.numeric(unlist(lapply(
#       strsplit(unlist(strsplit(iNames, ".pdf")), "_"), function(x) {
#         x[2]
#       }))))]
#     
#     movie <- image_read_pdf(paste(dName, iNames[[1]], sep = "/"), 
#                             density = density)
#     for (i in iNames[-1]) {
#       movie <- c(movie, image_read_pdf(paste(dName, i, sep = "/"), 
#                                        density = density))
#     }
#     movie
#   }
#   
#   if (!is.character(nam)) {
#     pdfs <- nam 
#   } else {
#     if (!useNam) {
#       dName <- fnam 
#     } else {
#       dName <- paste(fnam, nam, sep = "/")
#     }
#     pdfs <- getPDFs(dName, density = density)
#   }
#   # image_write(pdfs, path = paste0(nam, ".pdf"), format = "pdf")
#   gifs  <- image_animate(pdfs, fps = fps)
#   if (fnamOut) {
#     outNam <- paste0(fnam, "/", nam, ".gif") 
#   } else {
#     outNam <- paste0(nam, ".gif")
#   }
#   image_write(gifs, path = outNam, format = "gif") 
#   invisible(pdfs)
# }

# Saves out animated gif, invisibly returns
# dName: directory where pdfs are located
# outname: name of output gif
makeGifs <- function(dName, outNam, density = 45, fps = 4) {
  
  # Read in a bunch of pdfs, density sets size
  getPDFs <- function(dName =".", density = 45) {
    iNames <- dir(dName, pattern = "*.pdf")
    
    # Put in numeric order
    iNames <- iNames[order(as.numeric(unlist(lapply(
      strsplit(unlist(strsplit(iNames, ".pdf")), "_N"), function(x) {
        x[2]
      }))))]
    
    # Only keep selection of PDFs for testing
    # iNames <- iNames[3000:3600]
    
    movie <- image_read_pdf(paste(dName, iNames[[1]], sep = "/"), 
                            density = density)
    for (i in iNames[-1]) {
      movie <- c(movie, image_read_pdf(paste(dName, i, sep = "/"), 
                                       density = density))
    }
    movie
  }
  
  # Read PDFs and order them correctly
  pdfs <- getPDFs(dName, density = density)
  
  # Combine frames
  gifs  <- image_animate(pdfs, fps = fps)
  
  # Save gif
  image_write(gifs, path = outNam, format = "gif") 
  invisible(pdfs)
}

# objects <- attr(trace, "objects")


# Plots space 
plotSpace <- function(objects, plotGrid = TRUE) {
  par(mfrow = c(1,1))
  
  # Empty plot
  plot(NA, xlim = objects[[1]]$x, ylim = objects[[1]]$y, xlab = "x", 
       ylab = "y", bty = "n")
  
  # Add grid
  if (plotGrid) {
    myGrid(objects[[1]])
  }
  
  # Add objects, if there are objects
  if (length(objects) > 1) {
    for (i in 2:length(objects)) {
      drawSquare(objects[[i]])
    }
  }
  
  # Plot overall space, after objects so lines are on top
  drawSquare(objects[[1]], col = NA, border = "#636363", lwd = 2)
}


# Trace -------------------------------------------------------------------

# delay <- 0.1
# plotGrid <- TRUE
# plotCircle <- TRUE
# saveGifs <- FALSE
# tmpdir <- "tmp"
# froot <- "trace"
# density <- 45
# fps <- 4
# keepPdfs <- FALSE

# Animates trace in R  
plotTrace <- function(trace, start=1,delay = 0.1, plotGrid = TRUE, plotCircle = FALSE,
                      saveGifs = FALSE, tmpdir = "tmp", froot = "trace", 
                      density = 45, fps = 4, keepPdfs = FALSE,counter=TRUE) {
  if (saveGifs) {
    if (!dir.exists(tmpdir)) {
      dir.create(tmpdir)
    }
    fname <- paste(tmpdir, froot, sep = "/")
    delay <- 0
  } else {
    plotSpace(attr(trace, "space")$objects,plotGrid = plotGrid)
    fname <- ""
  }
  
  if (start>length(trace)) stop("Start too large!")
  for (i in start:length(trace)) {
    if (fname != "") {
      fnam <- paste0(fname,"_",i) 
    } else {
      fnam <- ""
    }
    plotPed(trace[[i]]$p, getP(trace[[i]]), trace[[i]]$a, trace[[i]]$r, 
      attr(trace, "space")$objects,cell=trace[[i]]$cell,fname = fnam,plotCircle = plotCircle,
      types=attr(trace[[i]]$pMat,"type"),
      stops=unlist(lapply(trace[[i]]$P,function(x){attr(x,"stop")>0})))
    if (!is.na(counter)) title(main=paste("Iteration",i))
    Sys.sleep(delay)
  }
  
  if (saveGifs) {
    makeGifs(froot, tmpdir, density = density, fps = fps)#, useNam = FALSE,
             #fnamOut = FALSE)
  }
  if (!keepPdfs) {
    unlink(tmpdir, TRUE)
  }
}

# Utility -----------------------------------------------------------------

plotUtility <- function(n, state, P_n, p_pred, centres, objects, nests, alpha, 
                        ok, iInfo) {
  
  plotLines <- function() {
    abline(v = 11.5, lty = 3)
    abline(v = 22.5, lty = 3)
  }
  
  p <- toNatural(state$pMat[n, ])
  muM <- getmuM(p)
  
  # Pre-compute
  ID <- predClose(n, p1 = state$p[n, , drop = FALSE], a1 = state$a[n], 
                  p2 = state$p, r = state$r, centres, p_pred, 
                  objects = objects)
  GA <- destinationAngle(state$a[n], state$p[n, , drop = FALSE], 
                         P_n[n, , drop = FALSE]) / 90
  FL <- getLeaders(n, state, centres, objects)
  WB <- getBuddy(n, group = state$group, a = state$a, p_pred, centres, objects, 
                 state = state)
  BA <- blockedAngle(n, state, p_pred, objects)
  PS <- dist1(state$p[n, ], 
              state$P[[n]][attr(state$P[[n]], "i"), 1:2, drop = FALSE])
  
  # Utility components
  ID <- idUtility(p, n, ID, ok, group = state$group)
  ID[ID == -Inf] <- NA  # can't plot -Inf
  BA <- baUtility(p, BA)
  BA[BA == -Inf] <- NA  # can't plot -Inf
  PS <- psUtility(p, state$v[n], PS)
  GA <- gaUtility(p, GA)
  CA <- caUtility(p)
  FL <- flUtility(p, FL)
  WB <- wbUtility(p, WB)
  
  V <- utility(p, n, state, P_n, p_pred, centres, objects, ok, iInfo = iInfo)
  Pr <- pCNLs(V, muM, nests, alpha) 
  
  par(mfrow = c(2, 5))
  u <- c(ID, PS, BA, GA, CA, FL, WB)
  ylim <- c(min(u, na.rm = TRUE), max(u, na.rm = TRUE))
  
  plot(ID, ylab = "Interpersonal Distance", xlab = "Cell", ylim = ylim)
  plotLines()
  plot(BA, ylab = "Blocked Angle", xlab = "Cell", ylim = ylim)
  plotLines()
  plot(PS, ylab = "Prefered Speed", xlab = "Cell", ylim = ylim)
  plotLines() 
  plot(GA, ylab = "Goal Angle", xlab = "Cell", ylim = ylim)
  plotLines() 
  plot(CA, ylab = "Current Angle", xlab = "Cell", ylim=ylim)
  plotLines() 
  plot(FL, ylab = "Follow the Leader", xlab = "Cell", ylim = ylim)
  plotLines() 
  plot(WB, ylab = "Walk Beside", xlab = "Cell", ylim = ylim)
  plotLines() 
  
  if (any(is.finite(V[-1]))) {
    plot(1:33, V[-1], pch = 16, xlab = "Cell", ylab = "Utility",
         main = paste("V(0)=", V[1]))
    plotLines()
  } else {
    plot(NA, NA, xlim = c(1, 33), ylim = c(0, 1), xlab = "Cell", 
         ylab = "STOPPED!", main = paste("V(0)=", V[1]))
  }
  
  plot(0:33, Pr, xlab = "Cell", ylab = "Probability")
  plotLines()
}


# Draw paths and goals ----------------------------------------------------

# # Plots circles at goals (rows of G, columns = x,y coords)
# plotGoals <- function(G, radius = 0.75) {
#   invisible(apply(matrix(G, ncol = 2), 1, function(xy) {
#     draw_circle(xy[1], xy[2], radius)
#   }))
# }

# Plots the space and obtain n path points by clicking 
getPathPoints <- function(n, objects, digits = 1) {
  plotSpace(objects)
  pts <- locator(n)
  pts$x <- round(pts$x, digits = digits)
  pts$y <- round(pts$y, digits = digits)
  cbind.data.frame(x = pts$x, y = pts$y)
}

# Draws lines on surfaces where goals can be placed  
drawGoalLines <- function(goalLines, goalRectangles, col = "#CA0020", lwd = 2) {
  lapply(goalLines, lines, col = col, lwd = lwd)  # lines
  for (i in 1:length(goalRectangles)) {
    drawSquare(goalRectangles[[i]], border = col, lwd = lwd)  # rectangles
  }
}

# Plots all links between path points
# Specify use to just plot links for that point name
plotPathLinks <- function(pathLinks, pathPoints, use = NULL, 
                          pointcol = "#92C5DE", linkcol = "#969696") {
  if (is.null(use)) {
    # All links
    pl <- pathLinks
    lwd = 1  # thinner lines if all points
  } else {
    # Links from point specified in use
    pl <- pathLinks[(pathLinks[, 1] == use) | pathLinks[, 2] == use, ]
    lwd = 2  # thicker lines if one point
  }
  
  # Draw lines for each link
  apply(pl, 1, function(x) {
    lines(rbind(pathPoints[as.character(x[1]), ], 
                pathPoints[as.character(x[2]), ]),
          col = linkcol,
          lwd = lwd)
  })
  
  # Add points at used path points
  if (!is.null(use)) {
    points(pathPoints[use, ],
           cex = 2,
           pch = 16,
           col = pointcol)
  } else {
    apply(pathPoints, 1, function(i) {
      points(x = i["x"], y = i["y"],
             cex = 2,
             pch = 16,
             col = pointcol)
    })
  }
}

# Draws arrow for shortest path between two points  
drawPath <- function(p1, p2 = NULL, edges = NULL, col = "#0571B0", len = .15, 
                     lwd = 2) {
  if (is.matrix(p1)) {
    xy <- p1[dim(p1)[1]:1, ] 
  } else {
    suppressMessages(path <- get_path_pair(removeGoals(edges, 
                                                       c(p1, p2)), p1, p2))
    xy <- t(sapply(path[[1]], function(x) {
      as.numeric(edges$coords[edges$coords[, 1] == x, 2:3])
    }))
  }
  for (i in dim(xy)[1]:2) {
    arrows(xy[i, 1], xy[i, 2], xy[i - 1, 1], xy[i - 1, 2], 
           col = col, len = .15, lwd = lwd)
  }
}

# Calculates and draws all goal paths from an edges object  
drawGoalPaths <- function(edges, col = "grey", lty = 1,delay=0,goalPaths=NULL) {
  if (is.null(goalPaths)) goalPaths <- getGoalPaths(edges)
  invisible(lapply(goalPaths, function(x) {
    lines(t(sapply(x, function(x) { 
      edges$coords[edges$coords[, 1] == x, 2:3]
    })),
    col = col, lty = lty)
    Sys.sleep(delay)
  }))
  invisible(goalPaths)
}

# Draw tour through path
drawTour <- function(Tour, edge = NULL, delay = 0, useColor = NA,
                     cols = c("magenta", "cyan", "red", "orange", "yellow", 
                              "blue", "green", "purple", "pink", "tan", 
                              "black")) {
  if (is.matrix(Tour)) {
    iG <- which(substr(row.names(Tour), 1, 1) == "G")
    for (i in 1:(length(iG) - 1)) {
      if (!is.na(useColor)) {
        col <- useColor 
      } else {
        col <- cols [(i - 1) %% 11 + 1] 
      }
      drawPath(p1 = Tour[iG[i]:iG[i + 1], , drop = FALSE], edges = edge, 
               col = col)
      Sys.sleep(delay)
    }
  } else {
    for (i in 1:(length(Tour) - 1)) {
      if (!is.na(useColor)) {
        col <- useColor 
      } else {
        col <- cols [(i - 1) %% 11 + 1] 
      }
      drawPath(p1 = Tour[i], p2 = Tour[i + 1], edge, col)
      Sys.sleep(delay)
    }
  }
}

# Draws one way arrows in aisles
drawOneWay <- function(aisles, col = "#969696", cex = 2) {
  points(do.call(rbind, lapply(aisles, function(x) { 
    unlist(lapply(x, mean))
  })), 
  pch = names(aisles), cex = cex, col = col)
}

