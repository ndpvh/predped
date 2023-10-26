##############################################################################-
# This file contains all goal management functions. 
##############################################################################-

# TO DO: Here, inconsistency in writing down the functions has been made
#        Single-line for-loops and if-statements without brackets

# Get goals ---------------------------------------------------------------

# Get n new p on real line as n row matrix 
getpMat <- function(n, p, pSD, rowNames = NULL,types=NULL) {
  # matrix(rnorm(n * length(p), p, pSD), ncol = length(p), byrow = TRUE,
  #        dimnames = list(rowNames,names(p)))
  anams <- sample(colnames(p),n,replace=TRUE,prob=p[1,])
  if (!is.null(rowNames) && (length(anams)!=length(rowNames)))
    stop("rowNames wrong length in getpMat")
  ps <- t(p[-1,anams,drop=FALSE]) 
  pSDs <- pSD[,anams,drop=FALSE] 
  for (i in 1:dim(ps)[1]) ps[i,] <- rnorm(dim(ps)[2],ps[i,],pSDs[,i])
  if (!is.null(rowNames)) rownames(ps) <- rowNames
  ps[,"pReroute"] <- round(ps[,"pReroute"])
  attr(ps,"type") <- types[anams]
  ps
}

# Extract goal matrix from state
getP <- function(state) {
  do.call(rbind, lapply(state$P, function(x) {
    x[attr(x, "i"), 1:2]
  }))
}


# Change goals ------------------------------------------------------------

# This has to be made simpler, no?
#
# What this does is take `goals` as a vector of different goals to 
# be done, and then uses the first one in this vector after dropping
# the code-string `"G"` (or this is how I understand it now at least)
nexGoal <- function(P) {
  goals <- row.names(P)[attr(P, "i"):dim(P)[1]]
  goals[substr(goals, 1, 1) == "G"][1]
}

# Inserts new goals due to re-routing (the last being the current goal) 
# in place of the current goal 
insertGoal <- function(P, newGoals) {
  attrP <- attributes(P)[-c(1:2)] 
  
  # Remove way points before next goal
  i <- attrP$i
  gi <- c(1:dim(P)[1])[dimnames(P)[[1]]==nexGoal(P)]
  if (gi>i) P <- P[-c(i:(gi-1)),]
  
  # Get bits before insertion point
  if (attrP$i == 1) {  # replace first goal
    P0 <- NULL  
  } else {  # later goal                           
    P0 <- P[1:(attrP$i-1), , drop = FALSE]  
  }
  if (attrP$i == dim(P)[1]) {  # replace last goal
    P1 <- NULL 
  } else {  # earlier goal                         
    P1 <- P[(attrP$i + 1):dim(P)[1], , drop = FALSE]     
  }
  
  # From current goal to end
  P <- rbind(P0, newGoals, P1)

  attributes(P) <- c(attributes(P), attrP)
  return(P)
}

# closeEnough <- 0.5
# sStop <- 0.2
# reportGoal <- FALSE

# Update goal (unless it is the last goal), two types, 
# If !mustVisit update can see next goal or veryClose to current goal. 
# If mustVisit update if within close of achieving it, if not update if can 
# see next goal (or could see on turning).
# Re-routing occurs if cant see next goal or (probabalistically, controlled by
# pReroute) pedestrians are blocking path to next goal.
updateGoal <- function(state, objects, interactionTime = 1,
                       closeEnough = 0.5,
                       sStop = 0.2,  # speed to resume after stop 
                       reportGoal = FALSE, interactiveReport = FALSE) {
  
  
  # For each pedestrian
  replan <- reroute <- setNames(logical(length(state$P)),names(state$P))
  for (j in 1:length(state$P)) {
    pReroute <- state$pMat[j,"pReroute"]
    # The waiting is over turn to new goal
    if (attr(state$P[[j]],"stop") > interactionTime ) { 
      # if (reportGoal) {
      #   cat(paste(rownames(state$p)[j], "Waiting time over, re-orienting\n"))
      # }
      state$a[j] <- angle2(state$p[j, , drop = F], 
                           state$P[[j]][attr(state$P[[j]], "i"), 1:2, 
                                        drop = FALSE])
      attr(state$P[[j]], "stop") <- -1 # Next move turn first
    } else if (attr(state$P[[j]], "stop") > 0) { # Update time waited
      # if (reportGoal) {
      #   if (!interactiveReport) {
      #     cat(paste(rownames(state$p)[j], "Updating wait time\n")) 
      #   } else {
      #     readline(prompt = paste(rownames(state$p)[j], 
      #                           "Updating wait time, press [enter] to continue"))
      #   }
      # }
      attr(state$P[[j]], "stop") <- attr(state$P[[j]], "stop") + 1
    } else { # Fix or update goals
      i <- attr(state$P[[j]], "i")
      # If current goal unseen, or cant move, re-route
      if (!seesCurrentGoal(j, state, objects)) {
        reroute[names(state$P)[j]] <- TRUE 
        if (reportGoal) {
          if (!interactiveReport) {
            cat(paste(rownames(state$p)[j], "Cant see goal, re-routing\n"))
          } else {
            readline(prompt = paste(
              rownames(state$p)[j],
              "Cant see goal, re-routing, press [enter] to continue"))
          }
        }
        newEdges <- addEdge(state$p[j, ], attr(state$P[[j]], "replan")$edges,
                            objects, attr(state$P[[j]], "replan")$oneWay)
        # Puts in new route
        newGoals <- expandTour(c("P0", nexGoal(state$P[[j]])),newEdges, objects)[-1, ]
        state$P[[j]] <- insertGoal(state$P[[j]],newGoals = newGoals)
        # Turn to first new goal and slow down
        state$a[j] <- angle2(state$p[j,,drop=FALSE],newGoals[1,,drop=FALSE])
        state$v[j] <- sStop
      } else {
        if (is.finite(pReroute) 
          #   && (is.null(attr(state$P[[j]],"blocked")) ||
          # !(nexGoal(state$P[[j]]) %in% attr(state$P[[j]],"blocked"))) 
          ){
          nBlockers <- nBlock(j, state, state$r[j])
          # Pedestrians in the way
          if (runif(1) < p_reroute(nBlockers,pReroute)) {
            np <- c(round(nBlockers[1]),round(p_reroute(nBlockers,pReroute), 3))
            newEdges <- removeEdges(addEdge(P0 = state$p[j, ],
              edges = attr(state$P[[j]],"replan")$edges,objects,
              oneWay = attr(state$P[[j]],"replan")$oneWay),nBlockers)
            # Puts in new route
            newGoals = expandTour(c("P0", nexGoal(state$P[[j]])),newEdges, objects)[-1, ]
            if (!is.null(newGoals)) {
              replan[names(state$P)[j]] <- TRUE 
              state$P[[j]] <- insertGoal(state$P[[j]],newGoals = newGoals)
              # Turn to first new goal
              state$a[j] <- angle2(state$p[j,,drop=FALSE],newGoals[1,,drop=FALSE])
              # Dont avoid again
              ng <- nexGoal(state$P[[j]])
              attr(state$P[[j]],"blocked") <- c(attr(state$P[[j]],"blocked"),ng)
              if (reportGoal) {
                if (!interactiveReport) {
                  cat(paste(rownames(state$p)[j]," replanning ",ng," to avoid a crowd of ",
                            np[1]," (p=",np[2],")\n",sep=""))
                } else {
                  readline(prompt = paste(rownames(state$p)[j],
                    " replanning ",ng," to avoid a crowd of ",np[1]," (p = ",np[2],
                    ") press [enter] to continue",sep=""))
                }
              }
              #  state$v[j] <- sStop # and slow down? not implemented
            } else {
              replan[names(state$P)[j]] <- NA 
              if (reportGoal) cat(paste(rownames(state$p)[j],
              " Avoiding a crowd of ",np[1],", re-routing (p = ",np[2],") FAILED\n",sep=""))
            }
          }
        }
      }
      # Not at last goal
      if (dim(state$P[[j]])[1] != i) { 
        # Close to goal
        if (dist1(state$p[j, ], 
                  state$P[[j]][i, 1:2, drop = FALSE]) < closeEnough) {
          # mustVisit goal
          if (substr(row.names(state$P[[j]])[i], 1, 1) == "G") { 
            
            # if (reportGoal) {
            #   if (!interactiveReport) {
            #     cat(paste(rownames(state$p)[j], "Update mustVisit goal\n")) 
            #   } else {
            #     readline(prompt = paste(
            #       rownames(state$p)[j],
            #       "Update mustVisit goal, press [enter] to continue"))
            #   }
            # }
            
            attr(state$P[[j]], "stop") <- 1  # start the counter
            attr(state$P[[j]], "i") <- i + 1
            # Not must visitGoal
          } else { 
            # Next subgoal (i.e., offset = 1) can already be seen OR is very 
            # close (even though not in field of view, otherwise can get stuck)
            if (seesCurrentGoal(j, state, objects, offset = 1) | 
                dist1(state$p[j, ], state$P[[j]][i, 1:2, drop = FALSE]) < 
                closeEnough) {
              attr(state$P[[j]], "i") <- i + 1
              attr(state$P[[j]], "stop") <- 0 
              
              # if (reportGoal) { 
              #   if (!interactiveReport) {
              #     cat(paste(rownames(state$p)[j], 
              #               "Update non-mustVisit goal\n")) 
              #   } else {
              #     readline(prompt = paste(
              #       rownames(state$p)[j],
              #       "Update non-mustVisit goal, press [enter] to continue"))
              #   }
              # }
              
            }  # else if (reportGoal) {
            #   cat("Dont update non-mustVisit goal\n")
            # }
          } 
        } 
      } 
    } # END fix or update goals
  } # END pedestrian loop
  attr(state$P,"replan") <- replan
  attr(state$P,"reroute") <- reroute
  return(state)
}


