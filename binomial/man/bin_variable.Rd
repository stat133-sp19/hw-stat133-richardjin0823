% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main-functions-and-methods.R
\name{bin_variable}
\alias{bin_variable}
\title{Binomial List Function}
\usage{
bin_variable(trials, prob)
}
\arguments{
\item{trials}{the specified number of trials for this binomial distribution list (numeric)}

\item{prob}{the specificed probability of success for this binomial distribution list (numeric)}
}
\value{
A named list that details the number of trials and probability parameters of a binomial distribution
}
\description{
Creates a list that details the trials and probability of a binomial distribution based on the trials and probability inputs after checking the validity of the inputs
}
\examples{
bin_variable(5, 0.5)
}
