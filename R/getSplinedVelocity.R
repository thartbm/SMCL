
#' @title Get smooth splined coordinate interpolation over time
#' @param x X-coordinates of a trajectory
#' @param y Y-coordinates of a trajectory
#' @param t Timestamps of the X and Y coordinates
#' @param spar Smoothing parameter for the spline, default: 0.01 (0.00-1.00)
#' @return This function returns a data frame with smooth spline interpolated
#' trajectory given by x, y and t.
#' @description 
#' (Should one day allow data frame or matrix input as well...)
#' @details 
#' ?
#' @examples
#' ?
#' @export
getSplinedTrajectory <- function(x, y, t, length.out=length(t), spar=0.01) {
  
  spl.Xt <- smooth.spline(t, x, spar=spar, keep.data=F) 
  spl.Yt <- smooth.spline(t, y, spar=spar, keep.data=F) 
  
  tt <- seq(min(t), max(t), length.out = length.out)
  
  XX <- predict(spl.Xt, tt)$y
  YY <- predict(spl.Yt, tt)$y
  
  return(data.frame('x'=XX, 'y'=YY, 't'=tt))
  
}

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
getSplinedVelocity <- function(x, y, t, spar=0.01) {
  
  # spline interpolate the X and Y coordinates over time:
  # (separately... no multi-dimensional splining in base R)
  ST <- SMCL::getSplinedTrajectory(x, y, t, length.out=length(t), spar=spar)

  # velocity on spline interpolated data
  V <- sqrt(diff(ST$x)^2 + diff(ST$y)^2) / diff(ST$t)
  
  return(data.frame('velocity'=V, 'time'=ST$t))
  
}