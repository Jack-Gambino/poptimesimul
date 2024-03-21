# makemonth() will "age" the population for one month.
# The default e2n etc. values can be modified in the calls; example:
#   makemonth(month=21, e2e=e2e, e2u=e2u-0.002, u2e=u2e+0.002,
#                       u2u=u2u-0.001, n2e=n2e+0.001, n2u=n2u,
#                       emp, unemp, inc)

makemonth <- function(month, e2e, e2u, u2e, u2u, n2e, n2u,
                      emp, unemp, inc) {

  ###
  # Labour force status section
  ###

  # Update emp and unemp values

  U <- runif(n=HCM)           # a random number for each person
  empold <- emp               # this vector will be updated

  # random Boolean vectors
  tst1 <- emp == 1 & U < e2u  # employed become unemployed
  tst2 <- emp == 1 & U > e2u + e2e  # employed become not-in-labour-force
  tst3 <- unemp == 1 & U < u2e  # unemployed become employed
  tst4 <- unemp == 1 & U > u2e + u2u  # unemployed become NILF
  tst5 <- emp + unemp == 0 & U < n2u  # NILF become unemployed
  tst6 <- emp + unemp == 0 & U > 1 - n2e  # NILF become employed

  # employed become unemployed
  emp[tst1] <- 0L
  unemp[tst1] <- 1L

  # employed become not-in-labour-force
  emp[tst2] <- 0L  # unemp is already 0 and does not change

  # unemployed become employed
  emp[tst3] <- 1L
  unemp[tst3] <- 0L

  # unemployed become NILF
  unemp[tst4] <- 0L  # emp is already 0 and does not change

  # NILF become unemployed
  unemp[tst5] <- 1L

  # NILF become employed
  emp[tst6] <- 1L

  ###
  # Income section
  ###

  # Give some people (n of them) a raise due to a promotion
  # (generate an adjustment factor to be applied below)


  n <- round(HCM/100) # 1% of population
  sampinc <-
    sample(HCM, size = n)  # people whose income may change
  incadj <- rep(1., HCM)  # initialize income adjustment to 1
  U <- runif(n, min=0.05, max=0.20)  # increase due to a "promotion"
  # only 1% of the population is eligible for a promotion each month
  # and only employed people can get promoted (emp[sampinc]==1):
  incadj[sampinc] <- incadj[sampinc] + U * emp[sampinc]

  # Next, adjustment factor due to change in employment status

  z <- inc  # last month's income, to be updated

  # change in employment status
  chemp <- emp - empold  # change is -1, 0 or 1

  # for people who went from emp=1 to emp=0 (the -1 case)
  U <-
    runif(n=HCM, min = 0.2, max = 0.6)  # potential decrease in income
  incadj2 <- rep(1., HCM)  # initialize adjustment to 1.
  incadj2[chemp == -1] <- 1. - U[chemp == -1]

  # people who became employed this month (emp=0 became emp=1) are
  # assigned an income; for inc00 see makepop.R
  positiveinc <- z>0  # identify people with a positive income
  newemppositiveincome <- chemp==1 & positiveinc # newly employed
  newempzeroincome <- chemp==1 & !positiveinc # newly employed, zero income
  z[newemppositiveincome] <- round(1.55 * z[newemppositiveincome])
  z[newempzeroincome] <- round((1.+month/L) * inc00[newempzeroincome])

  incadj3 <- 1.00  # increase income due to inflation (monthly)

  # apply income adjustments and store in the income matrix
  # keep income as an integer vector to save space
  inc <-
    as.integer(round(z * incadj * incadj2 * incadj3))

  return(list(emp, unemp, inc))  # return updated vectors as a list
}
