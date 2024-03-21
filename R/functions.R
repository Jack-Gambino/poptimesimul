# Some useful little functions

# ---------
# The first four functions are standalone functions that can be used anywhere
# ---------

# cumrange: Given a vector of sizes, where each size denotes a number of units,
# such as units in the same group or stratum, return low and high values of each
# range.

cumrange <- function(sizes) {
  H <- length(sizes)
  high <- cumsum(sizes)
  low <- c(1, high[-H]+1)
  cbind(low, high)
}

# multinorm: generate equicorrelated multivariate normal data
#
# Input: n=sample size, p=number of variables, rho=correlation
# Output: data matrix (row=unit, column=variable)
#
# This is the N(0,1) version; see multinorm.r the for general case

multinorm <- function(n, p, rho)
{
  corrmatrix <- matrix(rho, p, p)
  diag(corrmatrix) <- 1
  matrix(rnorm(n*p), n, p) %*% chol(corrmatrix)
}

# True variance of the mean under stratified SRS; see any standard
# book on the theory of survey sampling; h denotes stratum
varstratsrs <- function(Nh, nh, Sh) {
  N <- sum(Nh)  # population size
  ( sum( (Nh*Sh)^2/nh ) - sum(Nh*Sh^2) ) / N^2
}

# Intracluster (intraclass) correlation coefficient; also works for clusters of
# unequal size. See Donner-Koval (1980) paper and Snedecor and Cochran book
#
# Input: vector y of data and corresponding vector clus of cluster IDs

iccorr <- function(y, clus) {
  N <- length(y)   # total number of observations
  ybar <- mean(y)  # global mean
  ybarc <- tapply(y, clus, mean)  # cluster means
  yvarc <- tapply(y, clus, var)   # cluster variances
  n <- as.vector(table(clus))  # vector of cluster sizes
  k <- length(n)  # number of clusters
  
  MSB <- sum( n*(ybarc-ybar)^2 )  /  (k-1)  # mean square between clusters
  MSW <- sum( (n-1)*yvarc )  /  (N-k)       # mean square within clusters
  n0 <- mean(n) - var(n)/N  # equals cluster size if n1=n2=...=nk
  
  icc <- (MSB - MSW)  /  (MSB + (n0-1)*MSW)
  icc
}

# ---------
# The following functions depend on parameters from the population simulation
# ---------

# clusnum: Return the cluster number of the cth cluster in stratum h

clusnum <- function(h, c) {
  Ccum[h] + c  # a value in 1, 2, ..., Nclus
}

# persnumhc: Return the person number of the first person in the cth cluster in stratum h

persnumhc <- function(h, c) {
  clustrange[clusnum(h, c)]  # a value in 1, 2, ..., HCM
}

# persnum: Return the person number of ith person in the cth cluster of stratum h

persnum <- function(h, c, i) {
  persnumhc(h, c) + i -1
}
