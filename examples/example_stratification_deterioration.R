### Illustrate the deterioration over time of a good stratification ###
#   See also the corresponding vignette

### If necessary, load the data file:
#load("allmonths.RData")
####

# The synthetic population is used to illustrate how a good stratification of
# the population becomes less efficient over time, as the characteristics of the
# people in the population gradually change. The stratification is based on the
# income variable at time (month) 1: the first stratum has all the lowest-income
# people, the last stratum has all the highest income people, with the obvious
# gradation in between. A person's income can change from month to month, so
# some people in the first stratum will eventually have high incomes, and so on
# for other strata.
#
# The illustration is done in two ways: (1) empirically and (2) using sampling
# theory.
#
# For (1), many stratified samples are selected, with each sample producing an
# estimate of mean income for each month (we use the first month of each year as
# our sampling month). Because we have the whole population, we know the true
# mean income. Therefore we simply accumulate (estimate - true)^2 over all
# samples. Dividing by the number of samples, we get estimates of the mean
# squared errors for each month in the survey (they are actually variances since
# the estimators are unbiased).
#
# For (2), we use the formula for the variance of a stratified SRS based
# estimator (available in any good book on the theory of survey sampling).
# Because we know the true population, we can obtain *exact* variances. As a
# bonus, we can compare the results of (1) and (2) to see how well the approach
# used in (1) approximates the true variances.

source("R/functions.R")  # varstratsrs() and other useful functions

# Preliminaries: prepare the population data

month <- c(1, 13, 25, 49, 61, 73, 85, 97, 109)  # every 12th month
nmonths <- length(month)
B <- 1000  # the number of samples that will be selected

incjan <- data.frame(incmat[, month]*12)  # income in each of those Januaries

keepin <- incjan[, 1] > 0  # to remove people with zero income in month 1
incjan <- incjan[keepin, ]

HCMj <- nrow(incjan)  # the new population size after dropping zeroes
if (HCMj %% H > 0) {
  HCMj <- HCMj - HCMj %% H  # make it a multiple of H
  incjan <- incjan[1:HCMj, ]  # drop rows accordingly
}

# sort by the first-month income
incjan <- incjan[order(incjan$X1), ]
# cor(incjan) shows how correlation with month 1 falls off over time

# create the stratum variable
# each stratum will have the same number of units (not optimal)
strsize <- round(HCMj/H)
HMj <- rep(strsize, H)  # redefine HM
stratumj <- rep(1:H, each=strsize)  # assumes HCM is a multiple of H

incjan$stratumj <- stratumj  # add stratum to the dataframe
ncolX <-ncol(incjan)

nexpected <- 1000  # desired overall sample size

# Neyman allocation (based on data from first month): decide how much
# sample to select in each stratum (with total fixed at nexpected)

Sh <- as.numeric(by(incjan$X1, incjan$stratumj, sd))
NhSh <- Sh*HMj
nstr <- round(NhSh/sum(NhSh) * nexpected)  # sample size for each stratum
# note the variation in stratum allocation

nall <- sum(nstr)  # overall sample size (may be off due to rounding)

# the truth
meanincs <- apply(incjan[, -10], 2, mean)  # the series we want to estimate

stratrangej <- cumrange(rep(strsize, H))  # H x 2 matrix, 1st and last units

vars <- rep(0, nmonths)  # initialize the vector of variances
wstr <- rep(strsize/HCMj, H)  # relative weight of each stratum

####################################
#   Method 1: Empirical approach   #
####################################

for (b in 1:B) {

  sample_h <- numeric()  # initialize vector of sampled units

  # Select a sample (build it one stratum at a time); to keep it simple,
  # the same sample will be used for each month
  for (h in 1:H) {
    sample_h <- c(sample_h, sample(stratrangej[h, 1]:stratrangej[h, 2], nstr[h]))
  }

  Xsample <- incjan[sample_h, ]  # keep just the sampled units

  # the series of estimates for this sample using stratified SRS estimate

  # stratum estimates
  stratestimates <-
    simplify2array( by(Xsample[, -ncolX], Xsample[, ncolX], colMeans) )

  # roll up the stratum estimates to the population level (weighted row sums)
  estimates <- apply(stratestimates %*% diag(wstr), 1, sum)

  vars <- vars + (estimates-meanincs)^2  # build up Monte Carlo variances
  # note that all years are handled simultaneously
}

vars <- vars/B  # the variances
CV <- sqrt(vars)/meanincs  # coefficient of variation (not used)
plot(vars)

#############################################################
#   Method 2: Approach using results from sampling theory   #
#############################################################

varstropttruem <- varstrtruem <- numeric(nmonths)  # initialize

for (m in 1:nmonths) {
  incm <- incjan[, m]
  Shm <- as.numeric(by(incm, incjan$stratumj, sd))
  NhShm <- Shm*HMj
  nstrm <- round(NhShm/sum(NhShm) * nexpected)  # sample allocation
  # variances:
  varstropttruem[m] <- varstratsrs(HMj, nstrm, Shm)  # ideal sample allocation
  varstrtruem[m] <- varstratsrs(HMj, nstr, Shm)      # actual sample allocation
}

plot(vars/1000, pch="+", xlab="Year", ylab="Variance (in thousands)")
title("Variance of mean income estimate over time")
legend("bottomright", legend=c("+: empirical", "blue: true", "red: optimal"),
       text.col=c("black", "blue", "red"))
lines(varstrtruem/1000, col='blue')  # true variances
lines(varstropttruem/1000, col="red")  # best variances using Neyman allocation

# Show the relative variance (compared to restratifying each month) increases
degrade <- varstrtruem/varstropttruem  # degradation over time
#plot(degrade, type='l')

rm(HMj, HCMj, stratumj, stratrangej) # clean up
