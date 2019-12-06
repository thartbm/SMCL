



splineIntervalExtremeCI <- function(pos, values, range=c(min(pos, rm.na=TRUE), max(pos, rm.na=TRUE)), FUN=max.which, Npoints=1000, spar=NULL) {
  
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
  BSextreme <- apply(resamples,FUN=splineIntervalExtreme,MARGIN=c(3))
  
  
}

splineIntervalExtreme <- function(data, range=c( min(data$x, na.rm=TRUE), max(data$x, na.rm=TRUE) ), FUN=max.which, Npoints=1000, spar=NULL) {
  
  smspl <- smooth.spline(x=data$x, y=data$y, spar=spar)
  ix <- seq(range[1],range[2],Npoints-1)
  return(ix[FUN(predict(smspl,ix))$y])
  
}