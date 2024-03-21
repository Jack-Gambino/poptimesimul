\name{makepop}
\alias{makepop}
\title{Create a synthetic population and, optionally, extend it over many months}
\description{
Simulate a population based on parameters in \file{parameters.R} and, optionally, make it evolve month by month for \code{L} months.
}
\arguments{
  \item{savefile}{If \code{savefile=TRUE}, then an \code{.RData} file is saved. See the Value section below.}
  \item{L}{The number of months of data to generate. The outputs for \code{L=1} and \code{L>1} are different.}
  }
\details{
This program creates a population and makes it evolve over many months in a realistic way using transition probabilities defined in \file{parameters.R}. The resulting time series for key variables (such as employment status) is made to follow an actual time series; the default time series are Canadian employment and unemployment series, but other series can be used. If \code{L>1}, then the function \code{makemonth()} is called for each month 2, 3, ..., L.
}
\value{
The output depends on whether the number of months \code{L=1} or \code{L>1}. When \code{L=1} (i.e., a single month of data) and \code{savefile=TRUE}, the file \file{onemonth.RData} is created and it contains a population with characteristics age, sex, education, labour force status and income. The population is grouped into clusters which are themselves grouped into strata. If \code{L>1} and \code{savefile=TRUE}, then the file \file{allmonths.RData} is created. The key outputs in that file are three matrices: \code{empmat}, \code{unempmat} and \code{incmat}. Each row in a matrix corresponds to a person and each column to a month. Thus the value in row r, column c of \code{incmat} is the income for person r in month c. In addition, numerous other variables are included. In both cases, if  \code{savefile=FALSE}, an \code{.RData} is not created but the same variables appear in the current environment.
}
\seealso{
  \code{\link{parameters}}, \code{\link{makemonth}}
}
