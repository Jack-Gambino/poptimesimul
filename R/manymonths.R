source("R/makemonth.R")

# This version has a monthly income (rather than an annualized monthly income);
# later the monthly incomes are summed to produce an annual income for each
# person, one per year (see inc_annual below)

# Recall that low-numbered strata are lower in emp, inc, educ than
# high-numbered strata

# Some preliminaries to compute monthly factors for transition probabilities
# so that the time series behave like an actual LFS series. Here we use
# actual LFS estimates from the period from 2000 to 2009 inclusive.

realLFS <-
  read.table("extdata/LFS_EUP_2000_2009_notSA.txt", header = TRUE)

me2e0 <-
  mean(e2e0)  # 'typical' or base E-to-E transition probability
me2u0 <- mean(e2u0)  # E-to_U
mu2e0 <- mean(u2e0)  # etc.
mu2u0 <- mean(u2u0)
mn2e0 <- mean(n2e0)
mn2u0 <- mean(n2u0)

### factors for E and U (adjustments of transition probabilities)

# Think of fe as observed employment at time 2 divided by "predicted"
# employment at time 2 based on the base transition probabilities;
# see the documentation.
fe <- realLFS$EmpPop[2:120] /
  (realLFS$EmpPop[1:119] * me2e0 +
     realLFS$UnempPop[1:119] * mu2e0 +
     realLFS$NILFPop[1:119] * mn2e0)

fu <- realLFS$UnempPop[2:120] /
  (realLFS$EmpPop[1:119] * me2u0 +
     realLFS$UnempPop[1:119] * mu2u0 +
     realLFS$NILFPop[1:119] * mn2u0)

# Transition probabilities are defined at the stratum level in parameters.R.
# Here we assign the probabilities to each person:

e2e <- rep(e2e0, HM)
e2u <- rep(e2u0, HM)
u2e <- rep(u2e0, HM)
u2u <- rep(u2u0, HM)
n2e <- rep(n2e0, HM)
n2u <- rep(n2u0, HM)

# empmat is the matrix of employment indicators for all L months, so each row
# has the whole employment history of one person. Similarly for unempmat and
# incmat. In all three matrices, each column corresponds to one month.

# Next we fill in the month 1 column of each matrix

empmat <- matrix(ncol = L, nrow = HCM)  # initialize
empmat[, 1] <- emp # month 1 employment status

unempmat <- matrix(ncol = L, nrow = HCM) # same for unemployment
unempmat[, 1] <- unemp # month 1 unemployment status

incmat <-
  matrix(ncol = L, nrow = HCM) # same for income variable
incmat[, 1] <- inc # month 1 income

# We do not need a matrix for not-in-labour-force because NILF = 1-emp-unemp

# makemonth() will "age" the population for one month.
# The default e2n etc. values can be modified in the calls; example:
#   makemonth(month=21, e2e=e2e, e2u=e2u-0.002, u2e=u2e+0.002,
#                       u2u=u2u-0.001, n2e=n2e+0.001, n2u=n2u,
#                       ...)
# See the for loop below for an example.


# The basic transition probabilities defined above will be modified
# (using fe anf fu) to mimic real LFS time series

for (i in 2:L) {
  # i is the month
  empold <- empmat[, i - 1]
  unempold <- unempmat[, i - 1]
  incold <- incmat[, i - 1]
  newvectors <- makemonth(
    month = i,
    e2e = e2e * fe[i - 1],
    e2u = e2u * fu[i - 1],
    u2e = u2e * fe[i - 1],
    u2u = u2u * fu[i - 1],
    n2e = n2e * fe[i - 1],
    n2u = n2u * fu[i - 1],
    emp = empold,
    unemp = unempold,
    inc = incold)
  # now fill in month i
  empmat[, i] <- newvectors[[1]]
  unempmat[, i] <- newvectors[[2]]
  incmat[, i] <- newvectors[[3]]
}

###############################################
#  End of longitudinal part                   #
###############################################

# A true annual income (==sum of 12 monthly incomes) is of interest,
# so let us compute it

# block-diagonal matrix to compute annual incomes for each person
totmat <-
  matrix(rep(c(rep(1, 12), rep(0, L)), L / 12)[1:(L * L / 12)], nrow = L)

inc_annual <- incmat %*% totmat # annual income for each person

# varlist is the set of variables we want to keep
varlist <- c(
  "H",
  "C",
  "Nclus",
  "M",
  "Ccum",
  "HM",
  "HCM",
  "clustrange",
  "L",
  "cluster",
  "stratum",
  "age",
  "sex",
  "agegroup",
  "educ",
  "empmat",
  "unempmat",
  "incmat",
  "inc00",
  "y_icc",
  "inc_annual"
)

if (savefile) {
  save(list = varlist, file = "allmonths.RData")
}

rm(list=setdiff(ls(), varlist))
