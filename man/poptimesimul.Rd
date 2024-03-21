\name{poptimesimul}
\alias{poptimesimul}
\docType{package}
\title{
Generate a synthetic longitudinal population useful for sample survey methods research and education
}
\description{
\pkg{poptimesimul} generates a synthetic population with enough structure to make it possible for survey statisticians and students to use the population
to (1) illustrate existing sampling and estimation methods and (2) try
out new methods in a realistic setting. The population includes some basic static variables (age, sex, education level) and both discrete (labour force status) and continuous (income) analysis variables that change over time. The population evolves over many months in a way that is sufficiently realistic for the above two goals.
}
\details{
The main program is \code{makepop.R}, which creates an initial population (a snapshot of the population at month 1) and then optionally makes the population evolve over \code{L} months (\code{L=120} by default). Users who are not interested in longitudinal data can set \code{L=1}. Conversely, users who need many monthly snapshots of the population should set  \code{L} to a value greater than \code{1}. By default, \code{L=120}, i.e., ten years of monthly data. If the user wants \code{L>120}, then some changes are required since the program depends on an auxiliary file containing real data and the one in the package has 120 months of data.
}
\value{
The output of \code{makepop.R} when \code{L=1} is an .RData file containing a population with characteristics age, sex, education, labour force status and income. The population is grouped into clusters which are themselves grouped into strata. The output of \code{makepop.R} when \code{L>1} is a population file that is extended over \code{L}  months; see \link{makepop} for more information about how the data is stored.
}
\author{
Jack Gambino
}
\references{
The article \emph{poptimesimul: R Programs for Generating a Longitudinal Synthetic Population for Educational and Research Purposes} explains in detail the populations and how they can be used. See \code{poptimesimul_article.pdf} in the doc directory.}
\seealso{
  \code{\link{makepop}}
}
