# -----------------------------------
# Simulate a labour force population
# -----------------------------------

# Key parameters are defined in parameters.R
# Some useful little functions are defined in functions.R

savefile = TRUE
L <- 120
# set savefile to TRUE to create file allmonths.RData (if L>1)
# or onemonth.RData (if L==1)

source("R/parameters.R")  # key parameters
source("R/functions.R")   # some useful little functions

###############################################
#  Create the initial (month 1) population    #
###############################################

stratum0 <- 1:H  # stratum codes

Nclus <- sum(C)  # total number of clusters in the region

Ccum <- c(0, cumsum(C))
# (Ccum[h]+1):Ccum[h+1] is the range of clusters in stratum h for h = 1 to H
# Ccum[H+1] is the end of the last stratum, H, so Ccum[H+1] == Nclus

M <- round(rnorm(n = Nclus, mean = M0, sd = 20))  # cluster sizes
HM <-
  rep(0, H)  # HM will contain the number of people in each stratum
for (h in stratum0)
  HM[h] <- sum(M[(Ccum[h] + 1):Ccum[h + 1]])
HCM <-
  sum(HM)  # population size (number of people in the whole populations)

# create stratum code for each person
stratum <- rep(stratum0, HM)

# create cluster code for each person
cluster <- integer()
for (h in stratum0)
  cluster <- c(cluster, 1:C[h])  # cluster codes in each stratum
cluster <- rep(cluster, M)  # cluster codes at person level

# compute stratum ranges (person level)
stratrange <- cumrange(HM)  # cumrange() is defined in functions.R
# stratum h starts at stratrange[h, 1] and ends at stratrange[h, 2],
# so stratrange[h, 1]:stratrange[h, 2] is the range of people in h

# compute cluster ranges (person level)
clustrange <- cumrange(M)
# the range of people in cluster c is clustrange[c, 1]:clustrange[c, 2],
# where c = 1, 2, ..., Nclus

#-------- Age and sex --------

# Generate an age for each person, from age 15 to age 100;
# in the U.S. census data, age 100 means 100+.
# See parameters.R for the original probabilities.

# Keep ages 15+ (drop children); age 15 is 16th element, etc.
prob_age         <- prob_age[16:101] / sum(prob_age[16:101])
prob_M_given_age <- prob_M_given_age[16:101]

agerange <-
  c(0, cumsum(prob_age))  # 0, then prob age<=15, <=16, ...
agerange[length(agerange)] <- 1.0  # to avoid 0.999...

# age of each person: 15, 16, ..., 99, 100, generated at random
age <- findInterval(runif(HCM), sort(agerange)) + 14

# sex of each person: 0=female, 1=male
sex <- as.integer(runif(HCM) < prob_M_given_age[age - 14])

# Create an age group variable for age groups
# 15-19, 20-24, 25-54, 55-64, 65+
agegroupranges <- c(14.1, 19.1, 24.1, 54.1, 64.1)
agegroup <- findInterval(age, agegroupranges)

#-------- Employment and unemployment --------#

# Labour force status indicator: generate employment and unemployment
# variables (emp, unemp). Anyone who is neither employed (i.e., emp=0)
# nor unemployed (i.e., unemp=0) is not in the labour force (emp+unemp=0).
#
# Employment rates vary by stratum; e.g., by default, they go from 90% of the
# population average for stratum 1 to 110% of the average for stratum H. See
# the variable adjstrat defined below.
#
# Probabilities of being employed and unemployed were defined in
# parameters.R: pempM, pempF, punempM, punempF

# P(emp) for stratum h will be adjusted by adjstrat[h]
adjstrat <- seq(0.9, 1.1, length.out = H)
adjstrat <- rep(adjstrat, HM)  # apply at the person level
# Same for P(unemp)
adjstratu <-
  seq(1.1, 0.9, length.out = H)  # reverse order for unemployment
adjstratu <- rep(adjstratu, HM)

# create data frames to make merging easier; order0 is defined
# so the original order can be restored following merge()
df1 <- data.frame(order0 = 1:HCM,
                  agegroup = agegroup,
                  sex = sex)
df2 <- data.frame(
  pr = c(pempM, pempF),
  pru = c(punempM, punempF),
  agegroup = c(1:5, 1:5),
  sex = c(rep(1, 5), rep(0, 5))
)

df12 <- merge(df1, df2)  # merge by age group and sex
df12 <- df12[order(df12$order0),]  # restore original order

# adjust each person's P(emp) and P(unemp) depending on their stratum
pemp <- df12$pr * adjstrat
punemp <- df12$pru * adjstratu

# to make the P(emp) values vary, value p will be replaced
# by a random value between p*(1-delta) and p*(1+delta)
delta <- 0.05
u <- runif(HCM, min = 1 - delta, max = 1 + delta)
pemp <- pemp * u  # so each person has a different probability
# do the same for P(unemp)
u <- runif(HCM, min = 1 - delta, max = 1 + delta)
punemp <- punemp * u
# fix rare cases where pemp+punemp>1
pemp[pemp + punemp > 1] <- 1 - punemp[pemp + punemp > 1]

# employment indicator
u <- runif(HCM)
emp <- as.integer(u < pemp)

# unemployment indicator
unemp <- as.integer(u > 1 - punemp)

#-------- Education --------#

# Add an "education level" variable to the population
# Interpretation: 1 = no HS,
#                 2 = HS,
#                 3 = community college or equivalent,
#                 4 = degree
# Alternative interpretation: 1 = lowest propensity to respond, ...,
#                             4 = highest propensity to respond

s <- cbind(s1, s2, s3, s4)  # put s1-s4 in a matrix s of H rows;
# row h of s corresponds to stratum h;
# s1+s2+s3+s4 equals (1, ..., 1), i.e.,
# s %*% matrix(1, nrow=4, ncol=1)==(1,...,1)'

# Assign education level to each person; probabilities depend on stratum,
# so this is done one stratum at a time

educ <- integer(HCM)  # initialize

for (h in 1:H) {
  educ[stratrange[h, 1]:stratrange[h, 2]] <-
    sample(1:4,
           size = HM[h],
           prob = s[h,],
           replace = TRUE)
}

educ[age < 18] <- 1  # unlikely to have finished high school
educ[age < 21 & educ == 4] <- 3  # unlikely to have a degree

#-------- Income --------#

# Create income variable, saved as integer in thousands of dollars;
# start with an annual income, later converted to a monthly income.
#
# Mean *cluster* incomes are normally distributed
# and *personal* incomes within a cluster follow a gamma distribution.
#
# The values of meaninc, shp, csd and incadj are defined in parameters.R

# incadj is a sequence of H numbers to make mean income increase
# with stratum number (defined in parameters.R)
meaninc <- meaninc * incadj  # mean income at the stratum level

clusmean <- numeric()  # initialize the vector of cluster means
inc <- numeric()  # initialize the person-level income variable

# mean income for each cluster
for (h in 1:H) {
  clusmean <- c(clusmean, rnorm(C[h], mean = meaninc[h], sd = csd))
}

# income for each person
for (c in 1:Nclus) {
  inc <- c(inc, rgamma(M[c], shape = shp, scale = clusmean[c] / shp))
}

# Above income was in thousands; multiply by 1000; also divide by 12 to
# convert to monthly income. This is needed because income will evolve over
# time and changes (job promotion, job loss) can occur in any month. Income
# will be stored as one big vector (instead of a matrix).

inc <- 1000 * inc / 12
inc00 <-
  inc # keep this version to use as donor (imputed) values in
# later months for people who go from emp=0 to emp=1

minc <- mean(inc)  # actual mean income, will be used below

# Make some incomes equal 0 for people with emp=0 (e.g., students)
ifactor <- pmax(emp, runif(HCM) < 0.5)
inc <- inc * ifactor

# The above reduces overall mean income so restore the mean
inc <-  inc * minc / mean(inc)

inc <- as.integer(round(inc))  # convert to an integer

#-------- Variable with ICC --------#

# Generate an income-like variable that has a significant intra-cluster
# correlation (ICC). This variable is useful for studying clustered sampling
# designs (e.g., the impact of design effects on variances). The result is
# stored as an integer vector.
#
# This is similar to the income variable, but uses a multivariate normal
# distribution instead of a gamma distribution to generate values.
#
# The default rho=0.8 gives a moderate intra-cluster correlation.
#
# The values of rho, csd, meaniccvar and iccadj are defined in parameters.R

meaniccvar <-
  meaniccvar * iccadj  # mean iccvar at the stratum level

clusmean <- numeric()  # will contain means for all Nclus clusters
y_icctemp <- numeric() # temporary version of y_icc

for (h in 1:H) {
  clusmean <- c(clusmean, rnorm(C[h], mean = meaniccvar[h], sd = csd))
}

# cluster level
for (c in 1:Nclus) {
  y_icctemp <- c(y_icctemp, as.vector(t(multinorm(1, M[c], rho))))
}

# person level: replicate cluster means at the person level
clusmean <- rep(clusmean, M)

# All units in cluster c will have the same mean (from clusmean),
# and the standard deviation will be half the mean (e.g., N(50, 25))
y_icc <- as.integer(round(y_icctemp * clusmean / 2 + clusmean))

# iccvar can be negative so make negative values equal 0;
# this has a small impact on the ICC
y_icc[y_icc < 0] <- 0L

### end of common code (i.e., for L=1 and L>1)

if (L > 1) {
  ##### We want more than one month of data
  source("R/manymonths.R")   # end of L>1 case
} else {
  ##### L=1 case: we want just one month of data #####
  source("R/onemonth.R")
}
