



#' @title Fit a skewed normal function to x and y coordinates.
#' @param data A numeric vector with columns x and y.
#' @return This function returns five parameters describing a skewed normal
#' function:
#' - mu: the mean
#' - sigma: the standard deviation
#' - lambda: the skew 
#' - scale: a largely irrelevant scale parameter
#' - offset: and offset added to the function
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
fitSkewNormal <- function(data,verbosity=0) {
  
  # initial estimate for the mean:
  mu <- data$x[which.max(data$y)]
  # initial estimate for sigma:
  sigma <- abs(mu - data$x[which.min(abs(data$y-(max(data$y)/2)))])
  # initial estimate for lambda:
  lambda <- 0
  # initial estimate for scale:
  scale <- (1/dnorm(x=mu,mean=mu,sd=sigma)) * max(data$y)
  # initial estimate for offset:
  offset <- 0
  
  # combine all of them:
  par <- c('mu'=mu, 'sigma'=sigma, 'lambda'=lambda, 'scale'=scale, 'offset'=offset)
  
  if (verbosity > 0) {
    print(par)
  }
  
  # minimize the error function
  fit <- optim(     par=par,
                    fn=SMCL::skewNormalErrors,
                    data=data )
  
  # return fitted function:
  return(fit$par)
  
}



#' @title Calculate the MSE between a skewed normal function and data.
#' @param par A named numeric vector with parameters mu (mean), sigma (standard
#' deviation), a skew (lambda), a scale (multiplication factor), and an offset
#' (a number added everywhere) that describe a normal function.
#' The scale is applied before the offset.
#' @param data A data frame with columns X and y data points to compare to the
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
skewNormalErrors <- function(par, data) {
  
  return( mean( (data$y - skewNormal(par, x=data$x) )^2, na.rm=TRUE ) )
  
}



#' @title Evaluates a normal function at specific locations.
#' @param par A named, numeric vector describing a skewed normal function by 
#' its mean ('mu'), standard deviation ('sigma'), skew ('lambda') an offset 
#' that is added ('offset') and a multiplication factor ('scale').
#' @param x A numeric vector with locations to evaluate the skewed normal 
#' function at.
#' @return A numeric vector with the value of the skewed normal function 
#' described by the parameters `par`, as evaluated at the positions in `x`.
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
skewNormal <- function(par,x) {
  
  # here's an attempt using fGarch:
  #y <- par['offset'] + (par['scale'] * fGarch::dsnorm(x, mean=par['mu'], sd=par['sigma'], xi=par['lambda']) )
  # VGAM: won't work - deinstalled
  # sn: might work
  y <- par['offset'] + (par['scale'] * sn::dsn(x=x, xi=par['mu'], omega=par['sigma'], alpha=par['lambda']))
  
  return(y)
  
}
