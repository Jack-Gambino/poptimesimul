# Look at the full (aged) population

### If necessary, load the data file:
load("allmonths.RData")
####

emprate <- apply(empmat, 2, mean) # employment-to-pop ratio
plot(emprate, type='l', ylab="Employment/Population", xlab="Month")

emptot <- apply(empmat, 2, sum) # total employment (same plot, different axis)
plot(emptot, type='l' ,ylab="Employment", xlab="Month")

unempratio <- apply(unempmat, 2, mean) # unemployment-to-pop ratio
unemprate <- unempratio/(unempratio+emprate) # unemployment rate
plot(unemprate, type='l', ylab="Unemployment Rate", xlab="Month")

incmean <- apply(incmat, 2, mean) # mean monthly income each month
plot(incmean, type='l', ylab="Monthly Income", xlab="Month")

print(summary(inc_annual))  # annual income

mean_inc_annual <- apply(inc_annual, 2, mean)
plot(mean_inc_annual, type='l')

# time series analysis
empts <- ts(emptot, frequency=12)  # employment
empsa <- decompose(empts, type="additive")
plot(empsa, ann=FALSE)
title("Decomposition of Employment time series")

incts <- ts(incmean, frequency=12)  # income
incsa <- decompose(incts, type='additive')
plot(incsa, ann=FALSE)
title("Decomposition of Income (monthly) time series")

# a different time series package
empsa_v2 <- stl(empts, s.window="periodic")
plot(empsa_v2)
