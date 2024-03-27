##############################################################################-
# This file contains all model functions. 
##############################################################################-


# Nest weight
setAlpha <- function(nests) {
  unest <- unlist(nests)
  if (min(unest) == 0) {
    shift <- 1
  } else {  # zero cell
    shift <- 0
  }
  unest <- unest + 1
  alts <- sort(unique(unest))
  alphas <- numeric(length(alts))
  for (i in alts) {
    alphas[i] <- 1 / sum(i == unest)    
  }
  alpha <- nests
  for (i in 1:length(alpha)) {
    alpha[[i]] <- alphas[shift + alpha[[i]]]
  }
  alpha
}

# Crossed nested logit probabilities for each cell
pCNLs <- function(V, muM = rep(1, length(nests)), nests, alpha, mu = 1) {
  
  # Probability of alternatives within nests:
  # -> Why not as separate functions?
  pAinNest <- function(Vlist, nests, alpha, muM) {
    pim <- nests
    for (m in 1:length(nests)) {  # alternatives in each nest
      tmp <- alpha[[m]] * exp(muM[m] * Vlist[[m]])  
      bad <- tmp == Inf  # overflow
      if (any(bad)) {
        tmp <- ifelse(bad, 1, 0)
      }
      if (all(tmp == 0)) {
        pim[[m]] <- tmp 
      } else {
        pim[[m]] <- tmp / sum(tmp)
      }
    }
    pim
  }
  
  # Probability of nest m
  pNest <- function(Vlist, nests, alpha, mu, muM) {
    mu_muM <- mu / muM
    tmp <- sapply(1:length(nests), function(m) {
      (sum(alpha[[m]] * exp(muM[m] * Vlist[[m]])))^mu_muM[m]
    })
    bad <- tmp == Inf  # overflow
    if (any(bad)) {
      tmp <- ifelse(bad, 1, 0)
    }
    if (all(tmp == 0)) {
      tmp 
    } else {
      tmp / sum(tmp)
    }
  }
  
  # Nest probabilities
  if (any(unlist(nests) == 0)) {
    nests <- lapply(nests, function(x) {
      x + 1
    })
  }
  Vlist <- lapply(nests, function(x) {
    V[x]
  })
  
  # Set largest V to zero to avoid numerical issues
  # Also apply this reduction to all other V's 
  maxV <- max(unlist(lapply(Vlist, max)))
  Vlist <- lapply(Vlist, function(x) {
    x - maxV
  })
  # Compute both the probability of the nest itself as 
  # well as of the alternatives within the nest, which
  # you then multiply to get the total probability for 
  # each alternative
  pN <- pNest(Vlist, nests, alpha, mu, muM)
  pAN <- pAinNest(Vlist, nests, alpha, muM)
  for (m in 1:length(nests)) {
    pAN[[m]] <- pAN[[m]] * pN[[m]]
  }
  pA <- V
  indx <- unlist(nests)
  pAN <- unlist(pAN)
  for (i in 1:length(pA)) {
    pA[i] <- sum(pAN[indx == i])
  }
  pA
}