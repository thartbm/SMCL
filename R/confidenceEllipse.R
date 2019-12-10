

#' @title Get coordinates of a confidence ellipse.
#' @param X Vector of X-coordinates or a Mx2 matrix with M sets of X and Y 
#' coordinates.
#' @param Y If X is not a matrix, but a vector this has to be a vector of 
#' equal size with Y coordinates.
#' @param interval The proportion of data that should fall in the confidence
#' ellipse. Default: 0.95.
#' @param vectors Number of points on the ellipse, equally spaced angularly,
#' not in absolute distance.
#' @return A list with four entries:
#' - `XY` data frame with `vectors` rows, and 2 columns: `x` and `y` with
#' coordinates of an ellipse containing `interval` proportion of the data
#' - `major` semi-major-axis length (half the length of the ellipse)
#' - `minor` semi-minor-axis length (half the width of the ellipse)
#' - `angle` orientation of the ellipse in degrees
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
confidenceEllipse <- function(x, y=NA, interval=.95, vectors=100) {
  
  # get the square root of the chi-squared value for the specified confidence interval:
  chisq.val <- sqrt(qchisq(p=interval, df=2))
  
  # get the covariance matrix of the data:
  if (is.matrix(x)) {
    covmat <- cov( x )
  } else {
    x <- matrix(c(x,y), ncol = 2, byrow = FALSE)
    covmat <- cov( x )
  }
  
  # get the centre of the ellipse:
  centre <- colMeans(x)
  
  # get the eigen decomposition of the covariance matrix
  ev <- eigen(covmat)
  
  # get eigenvalues and -vectors separately:
  eigenvalues <- ev$values
  eigenvectors <- ev$vectors
  
  # determine which is the maximum eigenvalue and -vector:
  max.EigVal.ind <- which.max(eigenvalues)
  
  max.EigVal <- eigenvalues[max.EigVal.ind]
  max.EigVec <- eigenvectors[,max.EigVal.ind]
  
  # and which are the minimum eigenvalue and -vector:
  min.EigVal.ind <- which.min(eigenvalues)
  min.EigVal <- eigenvalues[min.EigVal.ind]
  min.EigVec <- eigenvectors[,min.EigVal.ind]
  
  # calculate the angle of the largest eigen vector:
  phi = ( ( atan2(max.EigVec[2], max.EigVec[1]) %% (2*pi) ) / pi ) * 180;
  
  # ellipse angles:
  thetas <- seq(0,2*pi,length.out=vectors)
  
  # the "radii":
  a <- chisq.val*sqrt(max.EigVal);
  b <- chisq.val*sqrt(min.EigVal);
  
  # get X and Y coordinates for the flat ellipse:
  X <- a*cos( thetas );
  Y <- b*sin( thetas );
  
  # rotate the ellipse:
  ellipse <- rotateCoordinates(df=data.frame(x=X, y=Y),angle=phi,origin=c(0,0))
  
  # re-centre:
  circumference$x <- ellipse$x + centre[1]
  circumference$y <- ellipse$y + centre[2]
  
  ellipse <- list()
  ellipse[['XY']] <- circumference
  ellipse[['major']] <- a
  ellipse[['minor']] <- b
  ellipse[['angle']] <- phi
  
  return(ellipse)
  
}