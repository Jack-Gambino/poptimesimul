# This file is sourced() by makepop(). It sets up the
# global parameters used when creating a population. Some of these
# parameters can be changed when calling the functions that use them.

#--------------------------
# Basic design parameters
#--------------------------

#L <- 120  # number of time points (months) is set in a function call

M0 <- 400  # mean cluster size - expected number of units (people) per cluster

H <-  10  # number of strata

# each stratum h has C[h] clusters
C <- c(75, 75, 75, 75, 80, 80, 80, 75, 75, 100)

#--------------------------
# Age-sex probabilities
#--------------------------

# Based on U.S. 2010 census data
# 0=female, 1=male
# ages 0, 1, ..., 99, 100; age 100 means 100+
# prob_age is P(age) (unconditional probability)
# prob_M_given_age is P(sex=M | age)

agesexprobs <- read.table(file="extdata/agesexdata.txt", header=TRUE)
prob_M_given_age <- agesexprobs$prob_M_given_age
prob_age <- agesexprobs$prob_age
rm(agesexprobs)

#--------------------------
# Labour force parameters
#--------------------------

# probabilities from 2021 Canadian LFS for age groups
# 15-19, 20-24, 25-54, 55-64, 65+, for males and females
# Note that the unemployment rates are multiplied by 0.6 to
# convert them into unemployed-to-population ratios.
# Make sure pempM + punempM < 1; same for F.

pempM <- c(.34, .56, .81, .64, .17) * 1.02
punempM <- c(.16, .20, .09, .07, .02) * 0.6 * 0.90
# last factor is to make means match up to starting estimates from real data

pempF <- c(.35, .52, .74, .52, .09) * 1.02
punempF <- c(.13, .18, .09, .07, .01) * 0.6 * 0.90

#--------------------------
# Education parameters
#--------------------------

# Levels: 1 = no HS, 2 = HS, 3 = community college, 4 = degree

# We want the following proportions in levels 1, 2, 3, 4:
#    .20, .33, .29, .18 for "low" strata
#    . . .
#    .05, .20, .35, .40 for "high" strata
#
# Check that s1+s2+s3+s4 == (1, ..., 1).

s1 <- seq(20,  5, length.out = H) / 100 # H is  the number of strata
s2 <- seq(33, 20, length.out = H) / 100
s3 <- seq(29, 35, length.out = H) / 100
s4 <- seq(18, 40, length.out = H) / 100

#--------------------------
# Income parameters
#--------------------------

# The following is a default value that can be changed

meaninc <- 50  # overall mean annual income in thousands of dollars

# Adjustment factors to make mean income increase with stratum number; these
# are multiplied by meaninc to produce a mean income per stratum.

incadj <- seq(0.8, 1.2, length=H)

# cluster means (clusmean) in a stratum are generated using rnorm()
# with standard deviation csd:

csd <- 3

shp <- 2  # parameter for the gamma distribution used to generate incomes

# Person-level incomes are generated using rgamma() with parameters
# shape = shp, scale = clusmean[i]/shp for cluster i.
# The clusmean vector is defined in makepop.R

# Other: About half the people who are not employed (emp==0) but have a
# positive income will have their income set to 0 -- see "runif(HCM) < 0.5"
# in the source code. The 0.5 value can be changed there.

#--------------------------
# iccvar parameters
#--------------------------

# Most of these parameters are similar to the income ones. For example,
# iccadj is identical to incadj by default.

meaniccvar <- 50  # like meaninc above

# Correlation used when generating multinormal data; the default rho=0.8
# results in a moderate intra-cluster correlation:

rho <- 0.8

iccadj <- seq(0.8, 1.2, length=H)

# Cluster means (clusmean) in a stratum are generated using rnorm() with
# standard deviation csd (defined above). csd is a parameter in the income
# and iccvar functions, so they need not use the same value of csd.

# iccvar values begin as correlated N(0, 1) deviates and are then adjusted to
# have mean clusmean and standard deviation clusmean/2 (e.g., N(50, 25^2)).

# Other: By default, negative iccvar values are set to 0. A line in the
# source code can be commented out to retain negative values.

#--------------------------
# Aging the population
#--------------------------

# These parameters are default transition probabilities used when the
# population is "aged" month by month. See makepop.R, which
# assigns these probabilities to people (e2e, e2u, etc.).

# Transition probabilities:

# P(emp -> emp) for each stratum (1, ..., H)
e2e0 <- seq(from=0.96, to=0.97, length=H)

# P(emp -> unemp)
e2u0 <- seq(from=0.014, to=0.012, length=H)

# P(unemp -> unemp)
u2u0 <- seq(from=0.61, to=0.59, length=H)

# P(unemp -> emp)
u2e0 <- seq(from=0.21, to=0.23, length=H)

# P(NILF -> unemp)
n2u0 <- seq(from=0.030, to=0.032, length=H)

# P(NILF -> emp)
n2e0 <- seq(from=0.033, to=0.035, length=H)

