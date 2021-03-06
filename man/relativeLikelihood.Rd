% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twoRateModel.R
\name{relativeLikelihood}
\alias{relativeLikelihood}
\title{Calculate relative likelihood of models based on information criteria}
\usage{
relativeLikelihood(crit)
}
\arguments{
\item{crit}{a numeric vector with the same informations criterion, for
several models (fit on the same data).}
}
\value{
Returns the relative likelihoods of the models
}
\description{
This function is part of a set of functions to fit and 
evaluate the two-rate model of motor learning.
}
\details{
This function returns the relative likelihood of a series of models based on
their scores on an information criterion (e.g. AIC or BIC). The best model 
will have a relative likelihood of 1, and models that have relative 
likelihoods between 1 and 0.05 are also good, while those below 0.05 can be 
considered worse than the best model.
}
\examples{
?
}
