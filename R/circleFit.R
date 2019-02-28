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
#' ?
#' @export
circleFit <- function(x, y, radius=12) {
  
  circlefit <-  optim(par = c('x' = 0, 'y' = 0), circleFitError, gr = NULL, data.frame(x, y), radius)
  
  return(circlefit$par)
  
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
circleFitError <- function(par, data, radius){
  
  return(mean((sqrt((data$x - par['x'])^2 + (data$y - par['y'])^2) - radius)^2, na.rm=TRUE))
  
}
