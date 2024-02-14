##############################################################################-
# This file contains all functions to compute flow metrics. 
##############################################################################-


# Rate --------------------------------------------------------------------

# Check how to compute flow rate

# trace=Long90_dutch
# stat=c("n","time","distance","pdistance","ndistance","stop","turn","replan",'reroute',"mean","min")[2]
# d_stat=c("mean","min")[1];exclude=NULL;
# lty = 1; col = "black"; lwd = 1;xlim=NULL;ylim=NULL;only_exit=TRUE; only_in=FALSE
# add_line=FALSE;do_plot=TRUE;mean_level=TRUE;increasing=FALSE; relative=FALSE; nostop=FALSE
# xtime = TRUE; step=.5/60; time_unit="(Seconds)"

plot_stat <- function(trace, 
  stat=c("n","time","distance","pdistance","ndistance","stop","replan","replanOK","replanFAIL",'reroute',"mean","min")[1],
  d_stat=c("mean","min")[1],exclude=NULL,
  lty = 1, col = "black", lwd = 1,xlim=NULL,ylim=NULL,only_exit=TRUE,only_in=FALSE,
  add_line=FALSE,do_plot=TRUE,mean_level=TRUE,increasing=FALSE,relative=FALSE,nostop=FALSE,
  xtime = TRUE, step=.5/60,time_unit="(Minutes)",main="") 
# Plot stat line (and/or return stat) (only_exit = only those 
# who exit, except for n, exclude = remove iterations for n or output for others)
#   n = number of people inside per iteration,
#   time = iterations inside per pedestrian
#   distance = distance traveled in m
#   pdistance = planned distance
#   ndistance = distance/planned distance
#   stop = number of stops
#   replan, replanOK, replanFAIL: all/successful/failed replanning
#   reroute: re-routing
# or d_stat over iterations  
#   mean = mean inter-body distance
#   min = minimum inter-body distance
# if mean_level add horizontal line at mean over x axis and prints value
# trace is a vector to plot already computed inside (the output of this function) 
# increasing = TRUE plots time and distance/inter-body distance in increasing 
#              order, but always saves without sorting.
# step = time step in time units
# time_unit name of time unit
{
  
  get_dist <- function(ti,stat) 
    # trace i inter-body distance
  { 
    r_mat <- outer(ti$r,ti$r,"+")
    d_mat <- apply(ti$p,1,dist1,p2=ti$p)
    diag(d_mat) <- NA
    if (is.numeric(stat))
      apply(d_mat-r_mat,1,function(x) sum(x<stat,na.rm=TRUE)) else
      apply(d_mat-r_mat,1,function(x) do.call(stat,list(x,na.rm=TRUE)))
  }
  
  first_in <- function(trace) {
    insim <- lapply(trace,function(x) names(x$a))
    agents <- unique(unlist(insim))
    first <- setNames(numeric(length(agents)),agents)
    for (i in 1:length(insim)) {
      unams <- names(first)[first==0]
      first[unams[unams %in% insim[[i]]]] <- i
    }
    first
  }
  
  if (increasing) xtime <- FALSE
  if (is.list(trace)) {
    if (stat=="n") {
      inside <- unlist(lapply(trace, function(state) {nrow(state$p)}))
    } else {
      if (only_exit & only_in) {
        warning("Cannot have both only_exit and only_in TRUE, using only_in")
        only_exit=FALSE
      }
      first <- first_in(trace) # iteration when first entering
      dplan <- setNames(numeric(length(first)),names(first))
      for (i in 1:length(first)) {
        dplan[i] <- sum(trace[[first[i]]]$P[[names(first[i])]][,"dist"],na.rm=TRUE) 
      }
      nams <- unlist(lapply(trace,function(x){names(x$a)}))
      i_nams <- unique(nams)
      still_in <- i_nams %in% names(trace[[length(trace)]]$a)
      if (only_exit) i_nams <- i_nams[!still_in]
      if (only_in) i_nams <- i_nams[still_in]
      dactual <- setNames(integer(length(i_nams)),i_nams)
      p <- do.call(rbind,lapply(trace,function(x) x$p))
      for (i in i_nams) {
        isin <- rownames(p)==i 
        if (sum(isin)>1) 
          dactual[i] <- sum(dist_rcpp(p[isin,][-1,],p[isin,][-sum(isin),]))
      }
      if (stat %in% c("time","distance","pdistance")) {
        inside <- dactual
        if (stat=="time") {
          if (nostop) nams <- unlist(lapply(trace,function(x){names(x$a)[x$cell!=0]}))
          for (i in i_nams) inside[i] <- sum(nams==i) 
          if (relative) inside <- dactual/inside
        } else {
          if (stat=="pdistance") inside <- dplan[names(inside)] 
          if (relative) inside <- inside/dplan[names(inside)]
        }
      }
      if (stat %in% c("mean","min") | is.numeric(stat)) {
        d <- unlist(lapply(trace,function(x){if (dim(x$p)[1]>1) get_dist(x,stat)}))
        d_nams <- unique(names(d))
        still_in <- d_nams %in% names(trace[[length(trace)]]$a)
        if (only_exit) d_nams <- d_nams[!still_in]
        if (only_in) d_nams <- d_nams[still_in]
        inside <- setNames(numeric(length(d_nams)),d_nams)
        if (is.numeric(stat)) {
          d <- d[names(d) %in% d_nams]
          n <- cbind.data.frame(names(d),as.numeric(d))
          inside <- tapply(n[,2],n[,1],sum)
          if (relative) inside <- inside/dactual[names(inside)]
        } else for (i in d_nams) inside[i] <- do.call(d_stat,list(d[names(d)==i])) 
      }
      if (stat %in% c("stop","replan","replanOK","replanFAIL","reroute")) {
        if (stat=="stop") n <- unlist(lapply(trace,function(x) setNames(x$cell==0,names(x$a)))) 
        if (stat %in% c("replan","replanOK","replanFAIL")) 
          n <- unlist(lapply(trace,function(x) attr(x$P,"replan"))) 
        if (stat=="reroute") n <- unlist(lapply(trace,function(x) attr(x$P,"reroute"))) 
        if (nostop) {
          n <- n[unlist(lapply(trace,function(y){unlist(lapply(y$P,function(x){attr(x,"stop")==0}))}))]
        }
        if (stat=="replan") n[is.na(n)] <- TRUE
        if (stat=="replanOK") n[is.na(n)] <- FALSE
        if (stat=="replanFAIL") {
          n[!is.na(n)] <- FALSE
          n[is.na(n)] <- TRUE
        }
        n <- cbind.data.frame(names(n),as.numeric(n))
        inside <- tapply(n[,2],n[,1],sum)
        if (only_exit | only_in) inside <- inside[i_nams] 
        if (relative) inside <- inside/dactual
      }
    }
  } else {
    if (do_plot==FALSE) stop("Have to plot if trace not supplied!")
    inside <- trace
  }
  
  x <- 1:length(inside)
  if (xtime & stat != "n") {
    if (stat=="n") x <- x*step else x <- first[names(inside)]*step    
  }
  if (stat=="time") if (xtime) {
    inside <- inside*step
    stat <- paste("time",time_unit)
  } else stat <- "iterations"
  if (!is.null(exclude)) {
    if (stat=="n") {
      pinside  <- inside[-c(1:exclude)] 
      x <- x[-c(1:exclude)]
    } else { # removes those who entered after
      oknams <- names(first[first>=exclude])
      ok <- names(inside) %in% oknams
      pinside <- inside[ok]
      x <- x[ok]
    }
  } else pinside <- inside
  
  # Plot 
  if (do_plot) {
    if (stat=="n") xlab="Iterations" else {
      if (increasing) {
        xlab="Increasing Order"
        pinside <- sort(pinside)
      } else if (xtime) xlab = paste("Entry Time",time_unit) else xlab="Entry Order"
      if (is.numeric(stat)) stat <- paste("Number <",stat,"m")
      if (relative) if (stat=="time") stat <- "speed" else 
        stat <- paste("Relative",stat)
    }
    if (add_line) lines(x=x,y = pinside, lty = lty, col = col) else {
      plot(x=x, y = pinside, lty = lty, col = col,type="l",
           xlab=xlab,xlim=xlim,ylim=ylim,ylab=stat,main=main)
      if (stat != "n") points(x,pinside)
    }
    if (stat != "n" & !only_exit & !increasing) 
      points(x[still_in],pinside[still_in],col="red",pch=16)
    if (mean_level) {
      abline(h=mean(pinside),col=col,lwd=lwd*3)
      print(mean(pinside))
    }
  }
  invisible(pinside)
}


get_types <- function(trace) 
  # Extracts vector of agent types  
{
  tab <- lapply(trace,function(x){
    out <-  setNames(rownames(x$pMat),names(attr(x$pMat,"type")))
  })  
  out <- unlist(tab)
  out <- out[!duplicated(out)]
  setNames(names(out),out)
}

type_means <- function(stat,trace) {
  if (is.list(trace)) mtypes <- get_types(trace) else mtypes=trace
  df <- cbind.data.frame(type=mtypes[names(stat)],dv=stat)
  tapply(df$dv,df$type,mean) 
}


plot_bad <- function(trace,stat=c("stop","replan","replanOK","replanFAIL","reroute")[1],
                      exclude_goal=TRUE,plotpoints=FALSE,plotGrid=TRUE,cex=1) {
  if (stat=="stop") n <- lapply(trace,function(x) {
    bad <- setNames(x$cell==0,names(x$a))
    if (!any(bad)) return(NULL) 
    if (exclude_goal) {
      nams <- names(bad)[bad]
      ok <- unlist(lapply(x$P,function(y) attr(y,"stop")==0))
      nams <- nams[nams %in% names(ok[ok])]
    }
    return(x$p[nams,,drop=FALSE])
  })
  if (stat %in% c("replan","replanOK","replanFAIL")) n <- lapply(trace,function(x) {
    bad <- attr(x$P,"replan")
    if (stat=="replan") bad[is.na(bad)] <- TRUE
    if (stat=="replanOK") bad[is.na(bad)] <- FALSE
    if (stat=="replanFAIL") {
      bad[!is.na(bad)] <- FALSE
      bad[is.na(bad)] <- TRUE
    }
    if (any(bad)) x$p[names(bad)[bad],,drop=FALSE] else NULL
  })
  if (stat=="reroute") n <- lapply(trace,function(x) {
    bad <- attr(x$P,"reroute")
    if (!is.null(bad) && any(bad)) x$p[names(bad)[bad],,drop=FALSE] else NULL
  })
  p <- do.call(rbind,n)
  if (is.null(p)) message("No bad points") else {
    plotSpace(attr(trace, "space")$objects,plotGrid = plotGrid)
    if (plotpoints) points(p,cex=cex) else text(p,labels=rownames(p),cex=cex)
  }
  invisible(p)
}



