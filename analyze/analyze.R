# Do some analysis of the month 1 population
# to see if the variables look reasonable

### If necessary, load the month-1 data file:
# load("onemonth.RData")
####

source("R/functions.R")   # some useful little functions

# compute stratum ranges (person level)
stratrange <- cumrange(HM)  # cumrange() is defined in functions.R

# The population starts with all the people in stratum 1,
# followed by all those in stratum 2, etc. The following
# shows where each stratum starts and ends.
cat("Start and end of each stratum:\n")
print(stratrange)

# Age-sex probabilities/proportion (including children)

agesexprobs <- read.table(file="extdata/agesexdata.txt", header=TRUE)
prob_M_given_age <- agesexprobs$prob_M_given_age
prob_age <- agesexprobs$prob_age

prob_F_given_age <- 1 - prob_M_given_age

plot(prob_F_given_age, ylim=c(0,1), pch='f', col='pink')
points(prob_M_given_age, pch='m', col='lightblue')

probM <- sum(prob_M_given_age*prob_age)  # proportion of males
probF <- 1-probM
print(c(probM, probF))

prob_age_given_M <-  prob_M_given_age*prob_age/probM
prob_age_given_F <-  prob_F_given_age*prob_age/probF
plot(prob_age_given_F, pch='f', col='pink')
points(prob_age_given_M, pch='m', col='lightblue')
points(prob_age, pch='*', col='green')

#===
# Labour force variables

# Employed person has emp==1, unemployed person has unemp==1
table(emp, stratum)[2,]/HM  # note difference between stratum 1 and stratum H
table(unemp, stratum)[2,]/HM  # these are ratios of unemployment to pop15+,
                              # as opposed to unemployment rates

# compare to input parameters (from parameters.R)
punempM <- c(.16, .20, .09, .07, .02) * 0.6 * 0.90
punempF <- c(.13, .18, .09, .07, .01) * 0.6 * 0.90
for (i in 1:5) {
  print(i)
  print(signif(c(mean(unemp[agegroup==i & sex==1]), punempM[i])), digits=2)
  print(signif(c(mean(unemp[agegroup==i & sex==0]), punempF[i])), digits=2)
}

tbe <- table(emp, stratum)[2,]/HM  # employment rate by stratum
plot(1:H, tbe, type='b')

tbu <- table(unemp, stratum)[2,]/HM  # unemployment
plot(1:H, tbu, type='b')

#===
# education variable

table(educ[1:HM[1]])/HM[1]  # lowest stratum
table(educ[(HCM-HM[H]+1):HCM])/HM[H]  # highest stratum

# for labour force population, the following should be in the same
# ballpark as Canadian LFS proportions: .16/.25/.32/.27
table(educ)/HCM

# mean educ level in lowest and highest stratum
mean(educ[stratrange[1,1] : stratrange[1,2]])
mean(educ[stratrange[H,1] : stratrange[H,2]])

#===
# look at some means (income, employment, education)

# income
mincstrat <- by(inc, stratum, mean)
plot(mincstrat)

# employment
mempstrat <- by(emp, stratum, mean)
plot(mempstrat)

cor(mincstrat, mempstrat)
plot(mincstrat, mempstrat)

# education
table(educ, stratum) # education by stratum
table(educ, stratum)/rep(HM, each=4) # proportions for each stratum
matplot(table(educ, stratum)/rep(HM, each=4), type='b')

#===

plot(by(inc, educ, mean), type='b') # income increases with education
plot(by(emp, educ, mean), type='b') # as does employment

#===
# Income

# The income variable (gamma-based) does not have a noticeable
# intracluster correlation:
iccorr(inc[1:HM[1]], cluster[1:HM[1]]) # first stratum
iccorr(inc[stratrange[H,1]:stratrange[H,2]],
       cluster[ stratrange[H,1] : stratrange[H,2] ]) # last stratum

# But the Gaussian income-like variable does have a noticeable
# intracluster correlation:
# modest ICC across the whole region ...
iccorr(y_icc, cluster)
# ... but higher ICC within each stratum:
iccorr(y_icc[1:HM[1]], cluster[1:HM[1]]) # first stratum
iccorr(y_icc[stratrange[H,1]:stratrange[H,2]],
       cluster[stratrange[H,1]:stratrange[H,2]]) # last stratum

#===
sum(inc==0)/HCM  # proportion of zero-income cases
sum(y_icc==0)/HCM

# look at the distribution of inc and y_icc
hist(inc*12, breaks=60)
hist(y_icc, breaks=60)
hist(12*inc[inc>0], breaks=60)  # exclude zero-income people
hist(y_icc[y_icc>0], breaks=60)

#===
