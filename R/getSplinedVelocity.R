

#' @title Get velocity profile after spline interpolation
#' @param X X-coordinates of a trajectory
#' @param Y Y-coordinates of a trajectory
#' @param t Timestamps of the X and Y coordinates
#' @param spar Smoothing parameter for the spline, default: 0.01 (0.00-1.00)
#' @return This function returns a data frame with spline interpolated velocity
#' and time for the trajectory given by X, Y and t.
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
getSplinedVelocity <- function(X, Y, t, spar=0.01) {
  
  spl.Xt <- smooth.spline(t, X, spar=spar, keep.data=F) 
  spl.Yt <- smooth.spline(t, Y, spar=spar, keep.data=F) 
  
  tt <- seq(min(t),max(t),length.out = length(t))
  
  XX <- predict(spl.Xt, tt)$y
  YY <- predict(spl.Yt, tt)$y
  
  # velocity on spline interpolated data
  V <- sqrt(diff(XX)^2 + diff(YY)^2) / diff(tt)
  
  return(data.frame('velocity'=V, 'time'=tt))
  
}