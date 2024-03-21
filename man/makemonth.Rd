\name{makemonth}
\alias{makemonth}
\title{Make the population age by one month}
\description{
Given data for month \code{t-1}, use transition probabilities to produce data for month \code{t}
}
\usage{makemonth(month, e2e, e2u, u2e, u2u, n2e, n2u,
                 emp, unemp, inc)}
\arguments{
  \item{month}{a value from 2 to L}
  \item{e2e, e2u, u2e, u2u, n2e, n2u}{vectors of transition probabilities for each person in the population (employment-to-unemployment, etc.)}
  \item{emp, unemp, inc}{population vectors of employment and unemployment status and income for month \code{t-1}}
}
\details{
\code{makemonth()} is called by \code{makepop.R}, once for each month, where month = 2, 3, ..., L. Each call produces a column in each of the matrices \code{empmat}, \code{unempmat} and \code{incmat}.
}
\value{
Updated columns in the matrices \code{empmat}, \code{unempmat} and \code{incmat} as described in \code{\link{makepop}}.
}
\seealso{
  \code{\link{parameters}}, \code{\link{makepop}}
}
