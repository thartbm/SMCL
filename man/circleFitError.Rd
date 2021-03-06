% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circleFit.R
\name{circleFitError}
\alias{circleFitError}
\title{Errors between coordinates' radial distance to a point and a given 
radius}
\usage{
circleFitError(par, coords, radius)
}
\arguments{
\item{par}{A vector with elements x and y denoting the central point's 
coordinates.}

\item{radius}{The radius the points in the data should have relative to a 
central point.}

\item{data}{A data frame with columns x and y.}
}
\value{
The mean difference between the radius and the distance between all
coordinates in the data and the central point.
}
\description{
This utility function can be used to see how well a set of 2D 
cartesian functions fit on a circle. The criterion it uses is radial 
distance to a given point. First, the distances of all the data to this 
point is calculated. Then the target radius is subtracted, resulting in a
vector of errors. The mean of the squared errors is returned. This number 
should always be positive and lower is better.
This function is used to find the optimal central point.
}
\details{
?
}
\examples{
?
}
