# Compare the variance estimate assuming *with replacement* sampling to the true
# variance. Both stages of sampling are done using SRSWOR. For convenience, many
# surveys pretend that the first stage units (clusters in our case) are selected
# *with* replacement. This is reasonable if the first-stage sampling fraction is
# small. The program below compares that variance to a Monte Carlo approximation
# of the true variance. By increasing the number nc of clusters selected in each
# stratum, it can be used to show the impact of non-negligible sampling
# fractions.
#
# See also the one-stratum version of this program

### If necessary, load the month-1 data file:
# load("onemonth.RData")

nc <- 10  # number of clusters to select in each stratum
nu <- 20  # number of units (people) to select in each cluster
ncall <- H * nc  # total number of clusters to be selected
nall <-  ncall * nu  # ultimate sample size
stratfrac <- HM/HCM  # proportion of the population in each stratum

B <- 10000  # number of iterations (samples) to select

# sampling weights
wh <- C/nc  # wh[h] is weight due to SRS of nc clusters in stratum h

wu2 <- M/nu  # second stage weight for each cluster

wu <- rep(wh, C) * wu2  # overall weight
wu <- rep(wu, M)  # assign to each person in the population

wu2 <- rep(wu2, M)  # assign stage 2 weight to each person in the population

# the variable of interest
income <- inc * 12  # we only need income from month 1
meanincpop <- mean(income)  # the value we want to estimate

popincomest <- varmeanWR <- numeric(B)  # initialize

for (b in 1:B) {

  # Select a SRS of nc clusters in each stratum and then a SRS of
  # nu people in each cluster

  clusamp <- numeric()  # clusamp is the sample of clusters

  for (h in 1:H)
    clusamp <- c(clusamp, sample(C[h], nc) + Ccum[h])

  sampleu <- numeric()  # will contain the final sample of people

  for (c in 1:ncall) {
    s_temp <- sample(M[clusamp[c]], nu) + clustrange[clusamp[c], 1] - 1
    sampleu <- c(sampleu, s_temp)
  }

  sampdata <- data.frame(
    stratum = stratum,
    cluster = cluster,
    income = income,
    wght = wu,
    wu2 <- wu2
  )[sampleu, ]

  stratincest <-
    as.vector(by(sampdata$income*sampdata$wght, sampdata$stratum, sum))/HM
  # HM should be close to as.vector(by(sampdata$wght, sampdata$stratum, sum))

  popincomest[b] <-
    sum(stratfrac * stratincest)  # overall estimate of mean income

  # Variance assuming with-replacement sampling

  Yhathi <- aggregate(sampdata$income*sampdata$wu2,  # cluster-level estimates
            list(str=as.factor(sampdata$stratum),
                 clu=as.factor(sampdata$cluster)),
            sum)
  Yhathi <- Yhathi[order(Yhathi$str, Yhathi$clu), ]  # sort them

  ssqh <- as.vector(by(Yhathi$x, Yhathi$str, var))  # s^2 for each stratum

  vartotWR <- sum(C^2*ssqh/nc)  # variance estimate for total income
  varmeanWR[b] <- vartotWR/HCM^2  # same but for mean income


}

mseinc <- sum((popincomest-meanincpop)^2)/B  # approximate true MSE (variance)

print(c(mseinc, mean(varmeanWR)))
# ratio of var based on WR assumption to (approximate) true variance
print(mean(varmeanWR)/mseinc)

# checks
#popincomest - meanincpop
sum(sampdata$wght); HCM
