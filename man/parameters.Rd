\name{parameters}
\alias{parameters}
\title{
Set parameters for a synthetic population
}
\description{
The parameters used by the \code{poptimesimul} programs are set in this file.
}
\details{
The global parameters used when creating a population are set in the file \file{parameters.R}. This file is \code{source}d by the main program \code{makepop.R}. The default values of the parameters are in parentheses.

\itemize{
  \item M0: average number of people per cluster (400)
  \item H: number of strata (10)
  \item C: a vector of length H indicating the number of clusters for
        each stratum (75, 75, 75, 75, 80, 80, 80, 75, 75, 100)
  \item L: the number of months of data (120); the cases \code{L=1} and \code{L>1} are different
  \item age-sex probabilities based on U.S. census data
  \item employment and unemployment ratios by age-sex, based on Canadian
        survey data
  \item education level: 1 = no high school diploma, ..., 4 = university degree,
        based on Canadian survey data
  \item meaninc: overall mean annual income in thousands of dollars (50)
  \item meaniccvar: overall mean for an income-like variable (50)
  \item transition probabilities for change from employed to unemployed,
        etc. (based on Canadian survey data; only used if
        \code{L>1})
}
}

\value{This program defines many variables that are then used by \code{makepop.R}.
}
\seealso{
  \code{\link{makepop}}
}

