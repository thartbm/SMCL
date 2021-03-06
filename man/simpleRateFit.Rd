% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simpleLearningRates.R
\name{simpleRateFit}
\alias{simpleRateFit}
\title{Fit the simple learning rate model to reach deviations.}
\usage{
simpleRateFit(schedule, reaches, gridpoints = 10, gridfits = 4)
}
\arguments{
\item{schedule}{A vector of length N with the perturbation schedule.}

\item{reaches}{A vector of length N with reach deviation data.}
}
\value{
A named numeric vector with the optimal parameter that fits a simple
rate model to the data as best as possible, with these elements:
- LR: the learning rate
}
\description{
This function is part of a set of functions to fit and 
evaluate a simple learning rate model of motor learning.
}
\details{
?
}
\examples{
# write example!
}
