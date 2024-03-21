### Important: Make sure the month-1 data file is loaded:
load("onemonth.RData")
source("R/functions.R")

# Illustrate the benefit of stratification: Compare a sample of, say, 1000
# units from the whole population against a sample allocated to each stratum
# by some method (see below), with the same total sample size. Many (B) samples
# are selected, and the estimates are collected and used to get a Monte
# Carlo approximation of the true variance. (In these simple cases, the
# true variances can by obtained exactly; these are included as a check.)

# See also the income version of this program and its corresponding vignette.

nexpected <- 1000  # desired overall sample size

# sample size in each stratum: uncomment one choice

# (1) proportional allocation
#nstr <- round(HM/HCM*nexpected)

# (2) equal allocation
#nstr <- rep(round(nexpected/H), H)

# (3) Neyman allocation
NhSh <- as.numeric(by(emp, stratum, sd))*HM
nstr <- round(NhSh/sum(NhSh) * nexpected)

nall <- sum(nstr)  # overall sample size (may be off due to rounding)

meanemp <- mean(emp)  # the value we are trying to estimate

# stratified SRS

B <- 10000  # number of samples to select

# simple random sampling ignoring strata

varsrs <- 0  # will contain the (Monte Carlo) SRS variance

for (b in 1:B) {
  varsrs <- varsrs + (mean(emp[sample(1:HCM, nall)]) - meanemp)^2
}

varsrs <- varsrs/B

# Compare to the theoretical value to make sure this is correct:
varsrstrue <- (1-nall/HCM) * var(emp) / nall
print(1-varsrs/varsrstrue)

# true means by stratum  -- not needed
empbystr <- as.numeric(by(emp, stratum, mean))

samp_h <- numeric(H)  # will contain estimates of stratum means
varstr <- 0  # will contain the (Monte Carlo) stratified SRS variance

stratrange <- cumrange(HM)  # cumrange() is defined in functions.R

for (b in 1:B) {

   # estimated means by stratum
   for (h in 1:H) {
     samp_h[h] <-
       mean(emp[stratrange[h, 1]:stratrange[h, 2]] |> sample(size=nstr[h]))
   }

   emphat_str <- sum(HM * samp_h)/HCM # stratified SRS estimate of mean employment
   varstr <- varstr + (emphat_str-meanemp)^2

}

varstr <- varstr/B  # approximate variance, stratified case

# Compare to the theoretical value to make sure this is correct:
NhShsq <- as.numeric(by(emp, stratum, var))*HM
varstropttrue <- ((sum(NhSh))^2/nall - sum(NhShsq)) / HCM^2
print(1-varstr/varstropttrue)
