
#' @title Calibrate coordinates in one reference frame to another.
#' @param Mcoords A data frame with columns of x and y measurements
#' @param Tcoords A data frame with target coordinates in columns x and y.
#' @return Parameter weights for a 12 term linear model.
#' @description Adapted from some old Python code.
#' This will return a list of 12 parameters that can be used to convert
#' new measuerements in the same coordinate system used in Mcoords to
#' coordinates in the coordinate system used in Tcoords. This is use in some
#' eye-trackers, but can also be used for old fashioned touch screens that
#' sometimes return warped.
#' @details Not yet.
#' @examples
#' data("localization")
#' str(localization_aligned)
#' 
#' # let's see if we can convert the touch screen pixels to (centi)meters
#' train <- localization_aligned[c(1:16),]
#' test <- localization_aligned[c(17:24),]
#' 
#' trainTaps <- train[,which(names(train) %in% c('tapx_px', 'tapy_px'))]
#' names(trainTaps) <- c('x','y')
#' 
#' trainHand <- train[,which(names(train) %in% c('handx_m', 'handy_m'))]
#' names(trainHand) <- c('x','y')
#' 
#' calibrationParameters(trainTaps,trainHand)
#' @export
calibrationParameters <- function(Mcoords,Tcoords) {
  
  xW <- SMCL::coordW(Mcoords,Tcoords$x)
  yW <- SMCL::coordW(Mcoords,Tcoords$y)
  
  return( c(as.vector(xW), as.vector(yW)) )

}


#' @title Apply calibration polynomial to new measurements.
#' @param P Vector of parameters fit by calibrationParameters() on other data.
#' @param Mcoords A data frame with measured coordinates in columns x and y.
#' @return A data frame with estimated coordinates in target reference frame.
#' @description Not yet.
#' @details Not yet.
#' @examples
#' data("localization")
#' str(localization_aligned)
#' 
#' # let's see if we can convert the touch screen pixels to (centi)meters
#' train <- localization_aligned[c(1:16),]
#' test <- localization_aligned[c(17:24),]
#' 
#' # get parameters fit onto the training data:
#' trainTaps <- train[,which(names(train) %in% c('tapx_px', 'tapy_px'))]
#' names(trainTaps) <- c('x','y')
#' trainHand <- train[,which(names(train) %in% c('handx_m', 'handy_m'))]
#' names(trainHand) <- c('x','y')
#' params <- calibrationParameters(trainTaps,trainHand)
#' 
#' # get test data ready
#' testTaps <- test[,which(names(train) %in% c('tapx_px', 'tapy_px'))]
#' names(testTaps) <- c('x','y')
#' 
#' calibrationPolynomial(params,testTaps)
#' @export
calibrationPolynomial <- function(P,Mcoords) {
  
  # P has 12 values, the first 6 for X, second for Y
  # estimate CM from touch coordinates
  
  x <- Mcoords$x
  y <- Mcoords$y

  esX = P[1] + (P[2]*x) + (P[3]*y) + (P[4]*x^2) + (P[5]*y^2) + (P[6]*x*y)
  esY = P[7] + (P[8]*x) + (P[9]*y) + (P[10]*x^2) + (P[11]*y^2) + (P[12]*x*y)

  return( data.frame('x'=esX, 'y'=esY ) )

}


#' @title Get parameter weights for converting between reference frames.
#' @param Mcoords A data frame with x and y columns of measurements
#' @param Tcoords A vector with target coordinates along one axis.
#' @return Parameter weights.
#' @description Adapted from some old Python code. Used by calibrate().
#' No need to understand or use this
#' @details Not yet.
#' @examples
#' Not yet.
#' @export
coordW <- function(Mcoords,Tcoords) {
  
  x <- Mcoords$x
  y <- Mcoords$y
  
  thi <- c( rep(1,length(x)), x, y, (x*x), (y*y), (x*y))
  thi <- t(matrix( thi, nrow=6, ncol=length(x), byrow = TRUE ))

  return(SMCL::ginv(thi) %*% as.matrix(Tcoords))
  
}
