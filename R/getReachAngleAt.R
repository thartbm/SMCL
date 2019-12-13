
#' @title Angular deviation of a reach from target at a specific point. 
#' @param trialdf Data frame representing the reach.
#' @param location Specifies which location to use.
#' @return Matrix with 1 row, 2 columns. First column has the angular deviation
#' of the reach from tha target angle at the specified location in degrees. The
#' second column has the angle of the reach in degrees.
#' @description
#' ?
#' @details
#' ?
#' @examples
#' ?
#' @export
getReachAngleAt <- function(trialdf, location='maxvel') {
  
  
  # location (string) determines where the angle of thereach is determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # cmX: the last sample before this distance from home, where X is replaced by a numeral
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=2)
  
  # if the trial was rejected, return empty matrix now
  if (trialdf[1,'trialselected'] == 0) {
    
    return(reachangle);
    
  }
  
  # extract the relevant reach information
  X <- trialdf[trialdf$sampleselected == 1,'Xrobot']
  Y <- trialdf[trialdf$sampleselected == 1,'Yrobot']
  MV <- trialdf[trialdf$sampleselected == 1,'maxvelocity']
  angle <- trialdf[1,'target']
  
  # print(X)
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  # cutoff in centimers, the last sample before this cutoff distance is reached
  # this assumes that people don't go back, or that there is only one movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(X^2 + Y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # if we don't have a valid location, we can't calculate an angle to return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  reachangle[1,2] <- angle
  
  return(reachangle)
  
}
