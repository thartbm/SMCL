
#' @title Rotate 2D trajectory
#' @param df A dataframe or matrix with two columns: X and Y coordinates.
#' @param angle An angle in degrees to rotate the trajectory by.
#' @param origin A vector with the coordinates to rotate around. Default (0,0)
#' @return Data frame with the rotated trajectory coordinates.
#' @description Rotate a trajectory of X,Y coordinates.
#' @details Not yet.
#' @examples
#' Not yet.
#' @export
rotateCoordinates <- function(df,angle,origin=c(0,0)) {
  
  df.names <- names(df)
  
  # create rotation matrix to rotate the X,Y coordinates
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix, and subtract origin
  coordinates <- sweep(as.matrix(df), 2, origin)
  
  # rotate the coordinates, add the origin back in
  df <- as.data.frame(sweep(coordinates %*% R, 2, origin*-1))
  
  # restore column names
  names(df) <- df.names
  
  # return the rotated coordinates
  return(df)
  
}