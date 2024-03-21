# Ratio of variance assuming with-replacement sampling and
# (true) variance which is based on without-replacement sampling

# All ten strata
H_all <- matrix(
  c(
    2, 1.0031,
    2, 1.0067,
    6, 1.0240,
    6, 1.0215,
    12, 1.0202,
    12, 1.0202,
    20, 1.0502,
    40, 1.0900,
    60, 1.1428
  ),
  ncol = 2, byrow = TRUE
)

# Just one stratum (h=5)
H_one <- matrix(
  c(2,	1.0082,
    2,	1.0043,
    10,	1.0214,
    20,	1.0455,
    40,	1.0953),
  ncol = 2,   byrow = TRUE
)

plot(H_all, pch="*", col="red", xlab="Number of clusters selected per stratum", 
     ylab="Variance ratio WR/WOR")
title("Ratio of variance assuming WR sampling to true variance")
legend("bottomright", legend=c("*: Pop = all strata", "+: Pop = one stratum"),
       text.col=c("red", "blue"))
points(H_one, pch="+", col="blue")
