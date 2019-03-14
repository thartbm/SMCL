# I can't help but think that this is faster:
# ask for precision (-5 to 10 digits or so)
# multiply y values by 10^precision, and round to 0 digits
# repeat each original x value the resulting y, and concatenate
# now calculate the mean and sd over that vector

# no scale or offset!



#' @title Bootstrap the 95% confidence interval of a normal function.
#' @param par A vector with elements x and y denoting the central point's 
#' coordinates.
#' @param pos A numeric vector with N data positions.
#' @param values A numeric matrix with N rows and M cols giving values 
#' (densities) from M observations at N positions.
#' @param bootstraps The number of bootstrapped samples (M columns) to fit
#' a normal function to.
#' @return Named numeric vector, with the 2.5%, 50% and 97.5% percentile of the
#' locations of peaks of a normal function fitted to samples of the data. These
#' are the upper and lower bound of the 95% confidence interval as well as the
#' median.
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
bootstrapNormalPeakCI <- function(pos,values,bootstraps=1000) {
  
  # need to get values as a matrix
  values <- as.matrix(values)
  
  # generate median curves
  
  # first re-sample the participants:
  # sample() gets a bunch of participant column indices
  # the array() call puts it in a form that gives us a bunch of resamples of the total data
  # in the shape: targets, participants, bootstraps  
  resamples <- array( values[, sample(x=c(1:ncol(values)), 
                                      size=ncol(values)*bootstraps, 
                                      replace=TRUE)], 
                      dim=c(length(pos), ncol(values), bootstraps))
  
  # get the means for each target in each bootstrap sample
  BSmeans <- apply(resamples,FUN=rowMeans,MARGIN=c(3))
  
  # now we need to get the peak location of a fitted curve, for which we will use another function
  muFits <- apply(BSmeans,FUN=fitNormal,MARGIN=c(2),pos)
  
  return(quantile(muFits,probs=c(.025,.50,.975)))
  
  
}

#' @title Fit a normal function to a set of values at given positions.
#' @param values A numeric vector of data values.
#' @param pos A numeric vector with N data positions.
#' @param output The kind of output wanted (see Value).
#' @return By default, this function only returns the location of the
#' peak of the normal function fitted to the data, but there are other
#' options:
#' - mu: the mean
#' - sigma: the standard deviation
#' - offset: and offset added to the function
#' - scale: a largely irrelevant scale parameter
#' - par: all four parameters as a named, numeric vector
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
fitNormal <- function(values,pos,output='mu') {
  
  # initial estimate for the mean:
  mu <- pos[which.max(values)]
  # initial estimate for sigma:
  sigma <- abs(mu - pos[which.min(abs(values-(max(values)/2)))])
  # initial estimate for offset:
  offset <- 0
  # initial estimate for scale:
  scale <- (1/dnorm(mu,mean=mu,sd=sigma)) * max(values)
  # combine all of them:
  par <- c('mu'=mu, 'sigma'=sigma, 'scale'=scale, 'offset'=offset)
  
  # make the data suitable for the error function:
  data <- data.frame(x=points, y=values)
  
  # minimize the error function
  fit <- optim(par=par,fn=normalErrors,data=data)
  
  # if people want all parameters:
  if (output == 'par') {
    return(fit$par)
  }
  
  # otherwise, return only one parameter:
  return(fit$par[output])
  
}

#' @title Calculate the MSE between a normal function and data.
#' @param par A named numeric vector with parameters mu (mean), sigma (standard
#' deviation), offset (a number added everywhere) and scale (multiplication
#' factor), that describe a normal function.
#' @param pos A data frame with columns X and y data points to compare to the
#' normal function described by the parameters. 
#' @return A numeric value: the mean squared error between the data and the
#' evaluated values of the function at the same x-coordinates.
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
normalErrors <- function(par, data) {
  
  return( mean( (data$y - (par['offset']+(par['scale']*normal(par, data$x))))^2, na.rm=TRUE ) )
  
}

#' @title Evaluates a normal function at specific locations.
#' @param par A named, numeric vector describing a normal function by its mean
#' ('mu'), standard deviation ('sigma'), an offset ('offset') and a
#' multiplication factor ('scale').
#' @param x A numeric vector with locations to evaluate the normal function at.
#' @return A numeric vector with the value of the normal function described by
#' the parameters `par`, as evaluated at the positions in `x`.
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
normal <- function(par,x) {
  
  y <- (1/(par['sigma']*sqrt(2*pi)))*exp(-0.5*(((x-par['mu'])/par['sigma']))^2)
  
  return(y)
  
}