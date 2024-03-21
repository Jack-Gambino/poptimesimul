varlist <-
  c(
    "H",
    "C",
    "Nclus",
    "M",
    "Ccum",
    "HM",
    "HCM",
    "clustrange",
    "cluster",
    "stratum",
    "age",
    "sex",
    "agegroup",
    "educ",
    "emp",
    "unemp",
    "inc",
    "inc00",
    "y_icc"
  )

if (savefile)
  save(list = varlist, file = "onemonth.RData")

rm(list=setdiff(ls(), varlist))
