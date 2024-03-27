##############################################################################-
# This file contains all parameter management functions. 
##############################################################################-


# Transform nest association to precision (mu)  
getmuM <- function(p) {
  1 / (1 - p[c("Central", "NonCentral", "acc", "const", "dec")])
}

# Tranfrom parameters from real line to natural
toNatural <- function(p, gt1p = c(""), negp = c(""),                             
                      posp = c("rU",                   # utility randomness
                             "bS",                     # stand still threshold
                             "bWB", "aWB",             # walk beside
                             "bFL", "aFL", "dFL",      # follow the leader
                             "bCA", "bCAlr", "aCA",    # current direction
                             "bBA", "aBA",             # blocked angle
                             "bGA", "aGA",             # goal angle
                             "bPS", "aPS", 
                             "sPref", "sSlow",         # preferred velocity
                             "bID", "aID", "dID"),     # interpersonal distance
                      pp = c("Central", "NonCentral",  # DCM nest association
                             "acc", "const", "dec")) {
  p[gt1p] <- exp(p[gt1p]) + 1
  p[negp] <- -exp(p[negp])
  p[posp] <- exp(p[posp])
  p[pp] <- pnorm(p[pp])
  p[!is.na(p)]
}

# Transform parameters from natural to real line
toReal <- function(p, gt1p = c(""), negp = c(""),           
                   posp = c("rU",                   # utility randomness
                            "bS",                   # stand still threshold
                            "bWB", "aWB",           # walk beside
                            "bFL", "aFL", "dFL",    # follow the leader
                            "bCA", "bCAlr", "aCA",  # current direction
                            "bBA", "aBA",           # blocked angle
                            "bGA", "aGA",           # goal angle
                            "bPS", "aPS", 
                            "sPref", "sSlow",       # preferred velocity
                            "bID", "aID", "dID"),   # interpersonal distance
                   pp = c("Central", "NonCentral",  # DCM nest association
                          "acc", "const", "dec")) {
  p[gt1p] <-  log(p[gt1p] - 1)
  p[negp] <-  log(-p[negp])
  p[posp] <-  log(p[posp])
  p[pp] <- qnorm(p[pp])
  p[!is.na(p)]
}

