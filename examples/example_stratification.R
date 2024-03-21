# Illustrate the benefit of stratification: Compare a sample of, say, 1000
# units from the whole population against a sample allocated to each stratum
# by some method (see below), with the same total sample size. Many (B) samples
# are selected, and the estimates are collected and used to get a Monte
# Carlo approximation of the true variance. (In these simple cases, the
# true variances can by obtained exactly; these are included as a check.)

# See also the corresponding vignette.

# Output:
# varsrs: MC variance of SRS
# varsrstrue: exact variance of SRS
# varstr: MC variance of stratified SRS
# varstropttrue: exact variance of stratified SRS

# To see the impact of an excellent (unrealistic) stratification,
# uncomment the line below. Do not use inc because it has many
# zeros; use inc00.
#
# It is informative to look at the sample size allocated to each
# stratum (nstr).
#
# Uncommenting the line below overwrites inc
# If you want to restore it, run "inc <- inctemp" at the end:
# inctemp <- inc; inc <- sort(inc00)

### If necessary, load the month-1 data file:
# load("onemonth.RData")

source("R/functions.R")  # some useful little functions

nexpected <- 1000  # desired overall sample size

# sample size in each stratum: *** uncomment one of these three choices ***

# (1) proportional allocation
#nstr <- round(HM/HCM*nexpected)

# (2) equal allocation
#nstr <- rep(round(nexpected/H), H)

# (3) Neyman allocation
NhSh <- as.numeric(by(inc, stratum, sd))*HM
nstr <- round(NhSh/sum(NhSh) * nexpected)  # sample allocation

nall <- sum(nstr)  # overall sample size (may be off due to rounding)

meaninc <- mean(inc)  # the value we are trying to estimate

# stratified SRS

B <- 10000  # number of samples to select; increase to get simul. closer to true

# simple random sampling ignoring strata

varsrs <- 0  # will contain the (Monte Carlo) SRS variance

for (b in 1:B) {
  varsrs <- varsrs + (mean(inc[sample(1:HCM, nall)]) - meaninc)^2
}

varsrs <- varsrs/B  # approximate variance, no stratification

# Compare to the theoretical value to make sure this is correct:
varsrstrue <- (1-nall/HCM) * var(inc) / nall
cat("Relative difference of true and simulation-based SRS variance:",
    1-varsrs/varsrstrue, "\n")

# true means by stratum  -- not needed
incbystr <- as.numeric(by(inc, stratum, mean))

samp_h <- numeric(H)  # will contain estimates of stratum means
varstr <- 0  # will contain the (Monte Carlo) stratified SRS variance

stratrange <- cumrange(HM)  # cumrange() is defined in functions.R

for (b in 1:B) {

   # estimated means by stratum
   for (h in 1:H) {
     samp_h[h] <-
       mean(inc[stratrange[h, 1]:stratrange[h, 2]] |> sample(size=nstr[h]))
   }

   inchat_str <- sum(HM * samp_h)/HCM # stratified SRS estimate of mean income
   varstr <- varstr + (inchat_str-meaninc)^2

}

varstr <- varstr/B  # approximate variance, stratified case

# Compare to the theoretical value to make sure this is correct
# B must be big (10000 or more) to get a good approximation
NhShsq <- as.numeric(by(inc, stratum, var))*HM
# The following is from Cochran p. 99; or use varstratsrs()
varstropttrue <- ((sum(NhSh))^2/nall - sum(NhShsq)) / HCM^2
cat("Relative difference of true and simulation-based StratSRS variance:",
    1-varstr/varstropttrue, "\n")  # should be close to 0

###   Stratified SRS versus SRS   ###
# with defaults, we get 0.96; in extreme case based on sort(inc00): 0.0315
cat(" -------------------------------------------------------------\n",
    "Ratio of StratSRS variance to SRS variance (true): ",
    varstropttrue/varsrstrue, "\n")
cat(" -------------------------------------------------------------------\n",
    "Ratio of StratSRS variance to SRS variance (simulation): ",
    varstr/varsrs, "\n",
    "-------------------------------------------------------------------\n")
