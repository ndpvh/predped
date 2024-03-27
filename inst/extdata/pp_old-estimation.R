#### !!!! Estimation NOT UPDATED !!!!  ----


##### Prepare traces ----
prepareTrace <- function(trace,objects,nests,alpha,p,cores=1)
{
  prepareState <- function(state,objects)
  {
    N <- dim(state$p)[1]
    centres <- oks <- dCs <- leader <- inFront <- groups <- vector(mode="list",length=N)
    dAngle <- matrix(nrow=N,ncol=11)
    dP <- matrix(nrow=N,ncol=33)
    p_pred <- predictPed(state$p,state$v,state$a,state$cell)
    for (n in 1:N) {
      centres[[n]] <- c_vd(1:33,p1=state$p[n,],v1=state$v[n],a1=state$a[n])
      ok <- apply(centres[[n]],1,inObject,xlim=objects[[1]]$x,ylim=objects[[1]]$y,FALSE)
      if (length(objects)>1) {
        blocked <- apply(array(apply(b_vd(p1=state$p[n,],v1=state$v[n],a1=state$a[n]),2:3,
                                     inObject,xlim=objects[[2]]$x,ylim=objects[[2]]$y,outside=FALSE),dim=c(5,2,12)),c(2,3),any)
        for (i in 2:length(objects)) {
          ok <- ok & apply(centres[[n]],1,inObject,xlim=objects[[i]]$x,ylim=objects[[i]]$y)
          if (i > 2) blocked <- blocked | apply(array(apply(
            b_vd(p1=state$p[n,],v1=state$v[n],a1=state$a[n]),2:3,inObject,
            xlim=objects[[i]]$x,ylim=objects[[i]]$y,outside=FALSE),dim=c(5,2,12)),c(2,3),any)
        }
        for (i in 2:12) blocked[,i-1] <- blocked[,i-1] & blocked[,i]
        blocked <- apply(rbind(rep(FALSE,11),blocked[2:1,-12]),2,function(x) {
          if (x[3]) x[1:2] <- TRUE else if(x[2]) x[1] <- FALSE; x
        })
      } else blocked <- NULL
      ok <- t(apply(matrix(ok,ncol=3),1,function(x){
        if (!x[3]) x[1:2] <- FALSE else if(!x[2]) x[1] <- FALSE; x
      }))
      if (!is.null(blocked)) ok <- ok &!t(blocked)
      oks[[n]] <- ok
      dCs[[n]] <- predClose(n,p1=state$p[n,,drop=FALSE],a1=state$a[n],centres[[n]],p_pred, objects = objects)
      dAngle[n,] <- destinationAngle(state$a[n],state$p[n,,drop=FALSE],state$P[n,,drop=FALSE])/90
      dP[n,] <- dist1(state$P[n,],centres[[n]])
      tmp <- getLeaders(n,state)
      if (!is.null(tmp)) leader[[n]] <- tmp
      inFront[[n]] <- minAngle(state$a[n],angle2(state$p[n,,drop=FALSE],state$p[-n,,drop=FALSE])) < 85
      groups[[n]] <- getBuddy(n,group=state$group,a=state$a,p_pred,centres[[n]], state = state)
    }
    
    
    c(state,list(centres=centres,ok=oks,dC=dCs,p_pred=p_pred,
                 destAngle=dAngle,dP=dP,leader=leader,inFront=inFront,groups=groups))
  }
  
  out <- mclapply(trace,prepareState,objects=objects,mc.cores=cores)
  attr(out,"nests") <- nests
  attr(out,"alpha") <- alpha
  pMat <- do.call(rbind,lapply(trace,function(x){x$pMat}))
  attr(out,"pMat") <- pMat[unique(row.names(pMat)),]
  attr(out,"ps.names") <- dimnames(attr(out,"pMat"))
  out
}


getSubjects <- function(Ltrace) {
  
  # Converts trace to subject data structure
  getSubject <- function(LT,s) {
    isin <- row.names(LT$p)==s
    if ( any(isin) ) {
      out <- LT
      out$n <- c(1:length(isin))[isin]
      out$cell <- out$cell[isin]
      out$pMat <- LT$pMat[isin,]
      out$centres <- LT$centres[isin][[1]]
      out$ok <- LT$ok[isin][[1]]
      out$dC <- LT$dC[isin][[1]]
      out$leader <- LT$leader[isin][[1]]
      out$inFront <- LT$inFront[isin][[1]]
      out$groups <- LT$groups[isin][[1]]
      out
    } else NULL
  }
  
  snams <- unique(unlist(lapply(Ltrace,function(x){row.names(x$p)})))
  out <- vector(mode="list",length=length(snams))
  names(out) <- snams
  for (s in snams) {
    tmp <- lapply(Ltrace,getSubject,s=s)
    out[[s]] <- tmp[!unlist(lapply(tmp,is.null))]
    attr(out[[s]],"p") <- out[[s]][[1]]$pMat
    for (i in 1:length(out[[s]])) out[[s]][[i]]$pMat <- NULL
  }
  out
}

# SLtrace <- getSubjects(Ltrace)


#### Core estimation functions ----
# Model utility
utilityL <- function(p,n,state,P_n,p_pred,centres,ok,dC,GA,dP,
                     leader,groups,inFront,
                     # absolute angular difference between straight ahead and each cone
                     directionAngle=rep(c(72.5,50,32.5,20,10,0,10,20,32.5,50,72.5),times=3))
{
  
  idUtility(p,n,ID,ok,group=groups) +
    psUtility(p,state$v[n]) +
    gaUtility(p,destAngle) +
    caUtility(p) +
    flUtility(p,FL)
  
  buddyUtility(p,groups)
  
}


pCNL <- function(cell,V,muM=rep(1,length(nests)),nests,alpha,mu=1,
                 cellNest=cbind(t(matrix(c(rep(2:3,5),c(1,3),rep(2:3,5),rep(c(2,4),5),c(1,4),
                                           rep(c(2,4),5),rep(c(2,5),5),c(1,5),rep(c(2,5),5)),nrow=2)),
                                cbind(c(1:5,1,6:15,2,16:25,3,26:30),c(1:11,1:11,1:11))))
{
  
  # Probability of alternatives within nests
  pAinNest <- function(Vlist,nests,alpha,muM) {
    pim <- nests
    for (m in 1:length(nests)) { # alternatives in each nest
      tmp <- alpha[[m]]*exp(muM[m]*Vlist[[m]])  
      bad <- tmp==Inf # Overflow
      if (any(bad)) tmp <- ifelse(bad,1,0)
      if (all(tmp==0)) pim[[m]] <- tmp else pim[[m]] <- tmp/sum(tmp)
    }
    pim
  }
  
  # Probability of nest m
  pNest <- function(Vlist,nests,alpha,mu,muM) {
    mu_muM <- mu/muM
    tmp <- sapply(1:length(nests),function(m){
      (sum(alpha[[m]]*exp(muM[m]*Vlist[[m]])))^mu_muM[m]
    })
    bad <- tmp==Inf # Overflow
    if (any(bad)) tmp <- ifelse(bad,1,0)
    if (all(tmp==0)) tmp else tmp/sum(tmp)
  }
  
  # Nest probabilties
  Vlist <- lapply(nests,function(x){V[x]})
  # Set largest V to zero to avoid numerical issues
  maxV <- max(unlist(lapply(Vlist,max)))
  Vlist <- lapply(Vlist,function(x){x-maxV})
  pN <- pNest(Vlist,nests,alpha,mu,muM)
  pAN <- pAinNest(Vlist,nests,alpha,muM)
  pAN[[cellNest[[cell,1]]]][cellNest[cell,3]]*pN[cellNest[cell,1]] + 
    pAN[[cellNest[[cell,2]]]][cellNest[cell,4]]*pN[cellNest[cell,2]] 
}


#### Non-subject functions ----

# stateL <- stateLs[[1]]
likeState <- function(stateL,p,nests,alpha) 
{
  
  like1 <- function(n,p,stateL,nests,alpha) 
  {
    if ( stateL$cell[n]==0 ) 1 else {
      p <- p[names(stateL$v)[n],]
      V <- utilityL(p,n,state=stateL[c("p","v","a")],
                    P_n=stateL$P,p_pred=stateL$p_pred,centres=stateL$centres[[n]],
                    ok=stateL$ok[[n]],groups=stateL$groups[[n]],
                    destAngle=stateL$destAngle[n,],dP=stateL$dP[n,],leader=stateL$leader[[n]],
                    inFront=stateL$inFront[[n]])
      pCNL(stateL$cell[n],V,muM=getmuM(p),nests,alpha) 
    }
  }
  
  # for (i in 1:length(stateL)) like1(i,p,stateL,nests,alpha)
  sapply(1:length(stateL$v),like1,p=p,stateL=stateL,nests=nests,alpha=alpha)  
}



# stateLs <- Ltraces[[1]]
# for (i in 1:length(stateLs)) likeState(stateLs[[i]],p,nests,alpha)
likeStates <- function(stateLs,p,nests,alpha) {
  unlist(lapply(stateLs,likeState,p=p,nests=nests,alpha=alpha))
}

sumlogLike <- function(p,Ltrace,constant=NULL,cores=1,minLike=1e-10) 
{
  dimnames(p) <- attr(Ltrace,"ps.names")
  if (!is.null(constant)) {
    p <- cbind(matrix(rep(constant,each=dim(p)[1]),ncol=length(constant)),
               t(apply(p,1,toNatural)))
    colnames(p)[1:length(constant)] <- names(constant)
  } else p <- t(apply(p,1,toNatural))
  imat <- suppressWarnings(matrix(1:length(Ltrace),ncol=cores))
  imat[duplicated(imat)] <- NA
  Ltraces <- apply(imat,2,function(x){Ltrace[x[!is.na(x)]]})
  out <- mclapply(Ltraces,likeStates,p=p,nests=attr(Ltrace,"nests"),
                  alpha=attr(Ltrace,"alpha"),mc.cores=cores)
  sum(log(pmax(unlist(out),minLike)))
}


msumlogLike <- function(p,Ltrace,constant=NULL,cores=1,minLike=1e-10,mult=-1) 
  # sum log likelihood, p on reals, constant (on natural) added in 
{
  dimnames(p) <- attr(Ltrace,"ps.names")
  if (!is.null(constant)) {
    p <- cbind(matrix(rep(constant,each=dim(p)[1]),ncol=length(constant)),
               t(apply(p,1,toNatural)))
    colnames(p)[1:length(constant)] <- names(constant)
  } else t(apply(p,1,toNatural))
  imat <- suppressWarnings(matrix(1:length(Ltrace),ncol=cores))
  imat[duplicated(imat)] <- NA
  Ltraces <- apply(imat,2,function(x){Ltrace[x[!is.na(x)]]})
  out <- mclapply(Ltraces,likeStates,p=p,nests=attr(Ltrace,"nests"),
                  alpha=attr(Ltrace,"alpha"),mc.cores=cores)
  mult*sum(log(pmax(unlist(out),minLike)))
}


profilePed <- function(p.name,min.p,max.p,pMat,dat,cores=1,
                       n.point=50,digits=2,ylim=NA,verbose=FALSE) 
  # for parameter p.name in pMat for n subjects draws the profile likelihood for 
  # data in min.p (<0) to p.max (>0) around p.name column mena, and returns the 
  # maximum (on a grid of resolution n.point) 
{
  if (!(p.name %in% colnames(pMat)))
    stop("p.name not in p.vector")
  ps <- seq(min.p,max.p,length.out=n.point)
  ll <- numeric(n.point)
  for (i in 1:n.point) 
  {
    p <- pMat
    p[,p.name] <- p[,p.name] + (ps[i]-mean(p[,p.name]))  
    ll[i] <- sumlogLike(p,dat,cores=cores)
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")
  names(ll) <- round(ps,digits)
  if (any(is.na(ylim)))
    plot(ps,ll,type="l",xlab=p.name,ylab="log-likelihood") else
      plot(ps,ll,type="l",xlab=p.name,ylab="log-likelihood",ylim=ylim)
  abline(v=mean(pMat[,p.name]))
  # ll[ll==max(ll)]
  ps[ll==max(ll)]
}


#### Estimation per subject ----

# 
# stateS <- stateLs[[1]]
# Sstate <- stateS[[1]]

likeStateS <- function(stateS,nests,alpha) 
{
  
  like1 <- function(p,Sstate,nests,alpha) 
  {
    if ( Sstate$cell==0 ) 1 else {
      n <- Sstate$n
      V <- utilityL(p,n,state=Sstate[c("p","v","a")],
                    P_n=Sstate$P,p_pred=Sstate$p_pred,centres=Sstate$centres,
                    ok=Sstate$ok,dC=Sstate$dC,
                    destAngle=Sstate$destAngle[n,],dP=Sstate$dP[n,],leader=Sstate$leader,
                    inFront=Sstate$inFront,groups=Sstate$groups)
      pCNL(Sstate$cell,V,muM=getmuM(p),nests,alpha)
    }
  }
  
  p <- attr(stateS,"p")
  lapply(stateS,like1,p=p,nests=nests,alpha=alpha)  
}

# stateLs <- Ltraces[[1]]
likeStatesS <- function(stateLs,nests,alpha) {
  unlist(lapply(stateLs,likeStateS,nests=nests,alpha=alpha))
}

sumlogLikeS <- function(p,Ltrace,constant=NULL,cores=1,minLike=1e-10) 
{
  dimnames(p) <- attr(Ltrace,"ps.names")
  if (!is.null(constant)) {
    p <- cbind(matrix(rep(constant,each=dim(p)[1]),ncol=length(constant)),
               t(apply(p,1,toNatural)))
    colnames(p)[1:length(constant)] <- names(constant)
  } else p <- t(apply(p,1,toNatural))
  for (i in names(Ltrace)) attr(Ltrace[[i]],"p") <- p[i,]
  imat <- suppressWarnings(matrix(1:length(Ltrace),ncol=cores))
  imat[duplicated(imat)] <- NA
  Ltraces <- apply(imat,2,function(x){Ltrace[x[!is.na(x)]]})
  out <- mclapply(Ltraces,likeStatesS,nests=attr(Ltrace,"nests"),
                  alpha=attr(Ltrace,"alpha"),mc.cores=cores)
  sum(log(pmax(unlist(out),minLike)))
}


# p=attr(Ltrace,"pMat")
msumlogLikeS <- function(p,Ltrace,constant=NULL,cores=1,minLike=1e-10,mult=-1) 
  # sum minus log likelihood, p on reals, constant (on natural) added in 
{
  dimnames(p) <- attr(Ltrace,"ps.names")
  if (!is.null(constant)) {
    p <- cbind(matrix(rep(constant,each=dim(p)[1]),ncol=length(constant)),
               t(apply(p,1,toNatural)))
    colnames(p)[1:length(constant)] <- names(constant)
  } else t(apply(p,1,toNatural))
  for (i in names(Ltrace)) attr(Ltrace[[i]],"p") <- p[i,]
  imat <- suppressWarnings(matrix(1:length(Ltrace),ncol=cores))
  imat[duplicated(imat)] <- NA
  Ltraces <- apply(imat,2,function(x){Ltrace[x[!is.na(x)]]})
  out <- mclapply(Ltraces,likeStatesS,nests=attr(Ltrace,"nests"),
                  alpha=attr(Ltrace,"alpha"),mc.cores=cores)
  mult*sum(log(pmax(unlist(out),minLike)))
}


profilePedS <- function(p.name,min.p,max.p,pMat,dat,cores=1,
                        n.point=50,digits=2,ylim=NA,verbose=FALSE) 
  # for parameter p.name in pMat for n subjects draws the profile likelihood for 
  # data in min.p (<0) to p.max (>0) around p.name column mena, and returns the 
  # maximum (on a grid of resolution n.point) 
{
  if (!(p.name %in% colnames(pMat)))
    stop("p.name not in p.vector")
  ps <- seq(min.p,max.p,length.out=n.point)
  ll <- numeric(n.point)
  for (i in 1:n.point) 
  {
    p <- pMat
    p[,p.name] <- p[,p.name] + (ps[i]-mean(p[,p.name]))  
    ll[i] <- sumlogLikeS(p,dat,cores=cores)
    if (verbose) cat(".")
  }
  if (verbose) cat("\n")
  names(ll) <- round(ps,digits)
  if (any(is.na(ylim)))
    plot(ps,ll,type="l",xlab=p.name,ylab="log-likelihood") else
      plot(ps,ll,type="l",xlab=p.name,ylab="log-likelihood",ylim=ylim)
  abline(v=mean(pMat[,p.name]))
  # ll[ll==max(ll)]
  ps[ll==max(ll)]
}



#### Analysis ----


# Average and mimimum distance among a set of points p
avminDist <- function(p) {
  avDist <- 0; minDist <- Inf
  N <- dim(p)[1]
  for (i in 1:N) {
    d <- dist1(p[i,],p[-c(1:i),,drop=FALSE])
    avDist <- avDist + sum(d)
    minDist <- min(c(minDist,d))
  }
  avDist <- avDist/(N*(N-1)/2)
  c(av=avDist,min=minDist)
}


# Matrix of x,y,v,a for each pedestrian at each time point
harvestTrace <- function(trace) 
{
  out=do.call(rbind,lapply(trace,function(x){x$p}))
  P <- do.call(rbind,lapply(trace,function(x){x$P}))
  dimnames(P)[[2]] <- c("X","Y")
  v <- do.call(c,lapply(trace,function(x){x$v}))
  a <- do.call(c,lapply(trace,function(x){x$a}))
  cell <- do.call(c,lapply(trace,function(x){x$cell}))
  cone <- coneNum(cell)
  ring <- ringNum(cell)
  cbind.data.frame(out,P,v=v,a=a,cell=cell,cone=cone,ring=ring)
}

