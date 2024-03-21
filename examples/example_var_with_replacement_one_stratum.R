# Compare the variance estimate assuming *with replacement* sampling to the true
# variance. Both stages of sampling are done using SRSWOR. For convenience, many
# surveys pretend that the first stage units (clusters in our case) are selected
# *with* replacement. This is reasonable if the first-stage sampling fraction is
# small. The program below compares that variance to a Monte Carlo approximation
# of the true variance. By increasing the number nc of clusters selected in a
# stratum, it can be used to show the impact of non-negligible sampling
# fractions.
#
# Note: This version uses just one stratum as the population.

### If necessary, load the month-1 data file:
# load("onemonth.RData")

source("R/functions.R")  # some useful functions

nc <- 6  # number of clusters to select in each stratum
nu <- 20  # number of units (people) to select in each cluster
nall <-  nc * nu  # ultimate sample size

B <- 10000  # number of iterations (samples) to select

h <- 5  # which stratum to use as the population; try different ones

# put everything in a data frame
popframe <- data.frame(
  cluster = cluster,
  income = inc*12
)[stratum == h, ]

# the value we want to estimate
meanincpop <- mean(popframe$income)

Mh <- as.numeric(table(popframe$cluster))  # number of people in each cluster
clustrangeh <- cumrange(Mh)  # start and end of each cluster

# sampling weights
wh <- C[h]/nc  # wh is weight due to SRS of nc clusters in stratum h

wu2 <- Mh/nu # second stage weight for each cluster

wu <- rep(wh*wu2, Mh)  # overall weight, assign to each person in the population

wu2 <- rep(wu2, Mh)  # assign stage 2 weight to each person in the population

popframe <- cbind(popframe, wght = wu, wu2 = wu2)

popincomest <- varmeanWR <- numeric(B)  # initialize

for (b in 1:B) {
  # Select a SRS of nc clusters and then
  #        a SRS of nu people in each cluster

  # clusamp is the sample of clusters
  clusamp <- sort(sample(C[h], nc))

  sampleu <- numeric()  # will contain the final sample of people

  for (c in clusamp) {  # select people in each cluster
    s_temp <- sample(clustrangeh[c, 1]:clustrangeh[c, 2], nu)
    sampleu <- c(sampleu, s_temp)
  }

  sampframe <- popframe[sampleu, ]  # keep only the sampled people

  # estimate of mean income
  popincomest[b] <- sum(sampframe$income*sampframe$wght)/HM[h]

  # Variance assuming with-replacement sampling

  Yhathi <- aggregate(sampframe$income*sampframe$wu2,  # cluster-level estimates
            list(clu=as.factor(sampframe$cluster)),
            sum)
  Yhathi <- Yhathi[order(Yhathi$clu), ]  # sort them

  ssqh <- var(Yhathi$x)  # s^2 of cluster totals

  vartotWR <- sum(C[h]^2*ssqh/nc)  # variance estimate for total income
  varmeanWR[b] <- vartotWR/HM[h]^2  # same but for mean income
}

mseinc <- sum((popincomest-meanincpop)^2)/B  # approximate true MSE (variance)

print(c(mseinc, mean(varmeanWR)))
# ratio of var based on WR assumption to (approximate) true variance
print(mean(varmeanWR)/mseinc)

# checks
sum(sampframe$wght); HM[h]  # should be close
meanincpop - mean(popincomest)
