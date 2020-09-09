  #' @title Get origin for points that should fall on a circle
  #' @param x The x-coordinates of the data.
  #' @param y The y-coordinates of the data. 
  #' @param radius The radius of the circle the data should fall on. Usually we 
  #' have data in centimeters, and movements of 12 centimeters.
  #' @return A vector with an x and y coordinate best fitting the circle.
  #' @description This utility function can be used to see used to find the 
  #' optimal central point, where optimal means finding the location where
  #' the mean distance of each data point to the central point matches the given
  #' radius best.
  #' @details 
  #' ?
  #' @examples
  #' 
  #' data("localization_aligned")
  #' data("localization_unaligned")
  #' 
  #' localization_aligned <- convert2cm(localization_aligned, from='r')
  #' localization_aligned <- convert2cm(localization_aligned, from='t')
  #' 
  #' par(mfrow=c(1,2))
  #' 
  #' plot(localization_aligned$tapx_cm, localization_aligned$tapy_cm, main='aligned',xlab='cm', ylab='cm', asp=1,bty='n', xlim=c(-2,12),ylim=c(-2,12),col='blue')
  #' segments(localization_aligned$tapx_cm, localization_aligned$tapy_cm, localization_aligned$handx_cm, localization_aligned$handy_cm, col='blue')
  #' lines(c(-1,11),c(0,0),col='gray')
  #' lines(c(0,0),c(-1,11),col='gray')
  #' lines(cos(seq(0,pi/2,pi/200))*10,sin(seq(0,pi/2,pi/200))*10, col='gray')
  #' centre <- circleFit(localization_aligned$tapx_cm, localization_aligned$tapy_cm, radius=10)
  #' points(centre$x, centre$y, col='red')
  #' lines( (cos(seq(0,pi/2,pi/200))*10) + centre$x, (sin(seq(0,pi/2,pi/200))*10) + centre$y, col='red', lty=2)
  #' points(localization_aligned$tapx_cm - centre$x, localization_aligned$tapy_cm - centre$y, col='green')
  #' segments(localization_aligned$tapx_cm - centre$x, localization_aligned$tapy_cm - centre$y, localization_aligned$handx_cm, localization_aligned$handy_cm,, col='green')
  #' 
  #' 
  #' localization_unaligned <- convert2cm(localization_unaligned, from='r')
  #' localization_unaligned <- convert2cm(localization_unaligned, from='t')
  #' 
  #' plot(localization_unaligned$tapx_cm, localization_unaligned$tapy_cm, main='unaligned',xlab='cm', ylab='cm', asp=1,bty='n', xlim=c(-2,12),ylim=c(-2,12),col='blue')
  #' segments(localization_unaligned$tapx_cm, localization_unaligned$tapy_cm, localization_unaligned$handx_cm, localization_unaligned$handy_cm, col='blue')
  #' lines(c(-1,11),c(0,0),col='gray')
  #' lines(c(0,0),c(-1,11),col='gray')
  #' lines(cos(seq(0,pi/2,pi/200))*10,sin(seq(0,pi/2,pi/200))*10, col='gray')
  #' centre <- circleFit(localization_unaligned$tapx_cm, localization_unaligned$tapy_cm, radius=10)
  #' points(centre$x, centre$y, col='red')
  #' lines( (cos(seq(0,pi/2,pi/200))*10) + centre$x, (sin(seq(0,pi/2,pi/200))*10) + centre$y, col='red', lty=2)
  #' points(localization_unaligned$tapx_cm - centre$x, localization_unaligned$tapy_cm - centre$y, col='green')
  #' segments(localization_unaligned$tapx_cm - centre$x, localization_unaligned$tapy_cm - centre$y, localization_unaligned$handx_cm, localization_unaligned$handy_cm,, col='green')
  #' @export
  circleFit <- function(x, y, radius=12, verbosity=0) {
    
    coords  <- data.frame(x, y)
    
    if ('optimx' %in% installed.packages()) {
      
      library(optimx)
      
      lower <- c(min(coords$x)-radius,min(coords$y)-radius)
      upper <- c(max(coords$x)+radius,max(coords$y)+radius)
      
      circlefit <- optimx(par=c('x'=0, 'y'=0), SMCL::circleFitError, gr = NULL, method='L-BFGS-B', lower=lower, upper=upper, coords=coords, radius=radius)
      
      return(list('x'=circlefit$x, 'y'=circlefit$y))
      
    } else {
  
      if (verbosity > 0) cat('optimx not installed, falling back on optim\n')
      
      circlefit <-  optim(par = c('x'=0, 'y'=0), SMCL::circleFitError, gr = NULL, coords=coords, radius=radius)
      
      return(list('x'=circlefit$par['x'],'y'=circlefit$par['y']))
      
    }
    
  }
  
  
  #' @title Errors between coordinates' radial distance to a point and a given 
  #' radius
  #' @param par A vector with elements x and y denoting the central point's 
  #' coordinates.
  #' @param data A data frame with columns x and y.
  #' @param radius The radius the points in the data should have relative to a 
  #' central point.
  #' @return The mean difference between the radius and the distance between all
  #' coordinates in the data and the central point.
  #' @description This utility function can be used to see how well a set of 2D 
  #' cartesian functions fit on a circle. The criterion it uses is radial 
  #' distance to a given point. First, the distances of all the data to this 
  #' point is calculated. Then the target radius is subtracted, resulting in a
  #' vector of errors. The mean of the squared errors is returned. This number 
  #' should always be positive and lower is better.
  #' This function is used to find the optimal central point.
  #' @details 
  #' ?
  #' @examples
  #' ?
  #' @export
  circleFitError <- function(par, coords, radius){
    
    return(mean((sqrt((coords$x - par['x'])^2 + (coords$y - par['y'])^2) - radius)^2, na.rm=TRUE))
    
  }
