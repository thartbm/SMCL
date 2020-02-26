

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
#' and time for the trajectory given by x, y and t. The first velocity sample
#' will always be zero.
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
  # add velocity = 0 for first sample:
  V <- c(0, V)
  
  return(data.frame('velocity'=V, 'time'=ST$t))
  
}


#' @title Angular deviation of a reach from target at a specific point. 
#' @param trialdf Data frame representing the reach.
#' @param location String specifying which location to use.
#' @param posunit String with the unit of x,y coordinates (pix, cm, ...)
#' @param timeunit String with the unit of time (s, ms, ...)
#' @param device String saying what position to use (hand, mouse, robot...)
#' @param holdvelocity Maximum velocity for a hold.
#' @param holdduration Minimum duration for a hold.
#' @return Matrix with 1 row, 5 columns:
#' 1: angular deviation
#' 2: target angle
#' 3: x position of location
#' 4: y position of location
#' 5: time of location
#' The idea is to combine this into a larger matrix (or data frame) with
#' multiple others, and then do further processing on that.
#' @description
#' ?
#' @details
#' ?
#' @examples
#' ?
#' @export
getReachAngleAt <- function(trialdf, location='pr0.33333', posunit='pix', timeunit='ms', device='hand', holdvelocity=NA, holdduration=NA) {
  
  # location (string) determines where the angle of the reach is determined, it is one of:
  
  # maxvel: maximum velocity
  # endpoint: end of the reach (only makes sense after selection)
  # cmX: the last sample before this distance from home, where X is replaced by a numeral (deprecated: use prX)
  # prX: first sample at or beyond a proportion of distance from home to target, given by X (e.g. 'pr0.333333')
  # hold: at a hold point; also specify how long and at what maximum velocity the hold has to be in other arguments
  # smvX: first velocity peak in spline-smoothed trajectory, beyond a percentage distance from home to target given by X (e.g. 'smv0.10')
  
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=5)
  colnames(reachangle) <- c( 'reachdeviation_deg', 
                             'targetangle_deg', 
                             sprintf('%sx_%s',device,posunit), 
                             sprintf('%sy_%s',device,posunit), 
                             sprintf('time_%s',timeunit) )
  
  
  # extract the relevant reach information
  x <- trialdf[,sprintf('%sx_%s',device,posunit)]
  y <- trialdf[,sprintf('%sy_%s',device,posunit)]
  t <- trialdf[,sprintf('time_%s',timeunit)]
  
  angle <- trialdf[1,'targetangle_deg']
  target <- as.numeric(trialdf[1,c(sprintf('targetx_%s',posunit),sprintf('targety_%s',posunit))])
  
  # always return the target angle?
  reachangle[1,2] <- angle
  
  # rotate the trajectory:
  # - avoids problems with atan2 angles around 180 / -180
  # - puts the target at 0, so angular deviation is easy to get
  trajectory <- SMCL::rotateCoordinates(x,y,-1*angle)
  x <- trajectory[,1]
  y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be a column in the data...
  # this only happens with preprocessing or manual screening
  # use 'smv' if this is not the case...
  if (location == 'maxvel') {
    MV <- trialdf[,'maxvel']
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
  
  # end point, just the last point in the reach
  if (location == 'endpoint') {
    rown <- length(x)
    invalidlocation <- FALSE
  }
  
  # cutoff in centimers, the last sample before this cutoff distance is reached
  # this assumes that people don't go back, or that there is only one movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(x^2 + y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # cutoff at a percentage from home to target in whatever unit is used
  if (substring(location,1,2) == 'pr') {
    distance <- as.numeric(substring(location, 3))
    #distance <- distance * sqrt(trialdf$targetx_pix[1]^2 + trialdf$targety_pix[1]^2)
    distance <- distance * sqrt(sum(target^2))
    
    # get the distance from home:
    dist <- sqrt(x^2 + y^2)
    
    # if there are no selected samples above 3 cm: return NAs
    if (length(which(dist > distance)) == 0) {
      return(reachangle)
    }
    
    # find the first sample, where dist > X
    rown <- min(which(dist > distance))
    invalidlocation <- FALSE
  }
  
  # find the first 'hold':
  if (substring(location,1,4) == 'hold') {
    holddistance <- as.numeric(substring(location, 5))
    #holdvelocity
    #holdduration # in timeunit: 's' or 'ms'?
    
    
    
  }
  
  # use smooth-splined trajectory to get angle at *first* velocity peak:
  if (substring(location,1,3) == 'smv') {
    
    # how far does the vleocity peak have to be away from the home position
    # (as percentage of home-target distance)
    if (nchar(location) > 3) {
      distance <- as.numeric(substring(location, 4))
    } else {
      distance <- 0.05
    }
    distance <- distance * sqrt(sum(target^2))
    
    dist <- sqrt(x^2 + y^2)
    
    VT <- getSplinedVelocity(x, y, t, spar=0.20)
    v <- c(0, 0, VT$velocity)
    
    peaks <- which(diff(sign(diff(v))) == -2 & dist > distance)
    if (length(peaks) > 0) {
      rown <- peaks[1]
      invalidlocation <- FALSE
    }
    
  }
  
  
  
  # if we don't have a valid location, we can't calculate an angle
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(y[rown],x[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  reachangle[1,2] <- angle
  reachangle[1,3] <- trialdf[rown,sprintf('%sx_%s',device,posunit)]
  reachangle[1,4] <- trialdf[rown,sprintf('%sy_%s',device,posunit)]
  reachangle[1,5] <- t[rown]
  
  return(reachangle)
  
}


#' @title Trim a data frame describing a reach to the interesting part. 
#' @param trialdf Data frame representing the reach with variables in columns
#' (named consistent with settings below) and samples in rows.
#' @param homeStart If the participant has to get to the home position before
#' starting the out-and-back reach, this part could be trimmed. Set `homeStart`
#' to a numeric value expressing how close the `device` has to be to the home
#' position, specified in the same unit as `posunit`. The start of the trial
#' will be trimmed (not returned) up to when the device is that close to the 
#' start/home position. By default this is NA, so that this part is not 
#' trimmed. If this is a character variable, starting with "pr" and ending with
#' numbers, those numbers should indicate a proportion of the home-to-target
#' distance to use as cutoff value for trimming the first part of the reach.
#' @param targetReached If the return movement is represented in the data, this
#' may have to be trimmed as well. This parameter sets the *distance* at which 
#' the target is considered reached, and data after this point is trimmed.
#' The target position should be in columns named "targetx_[posunit]" and 
#' "targety_[posunit]". This argument is a numeric variable given in the 
#' position unit specified later on.
#' @param velocity Very slow movement is usually not diagnostic. _After_ the
#' other parts of the data are trimmed, the instantaneous velocity is used as
#' a cut-off criterion: the first part of the reach that is under the velocity
#' criterion as a fraction of the maximum velocity in the whole reach, is 
#' trimmed. And either the final part that is below the velocity criterion is
#' trimmed, or everything after the first dip below the velocity criterion, 
#' depending on the `firstMove` parameter. Set to `NA` for no velocity 
#' criterion. By default this is set conservatively to 0.05.
#' 
#' May give unexpected results if used with `untilHold`.
#' @param firstMove Only used if the `velocity` parameter is not `NA`. If set 
#' to TRUE, the first part of the trajectory up to where it dips below the 
#' velocity criterion is kept (the rest is trimmed). If FALSE, only the final
#' part of the trajectory that goes below the velocity criterion is trimmed.
#' 
#' May give unexpected results if used in combination with `untilHold`.
#' @param untilHold Not used if set to `NA` (default). Otherwise, this should
#' be a list with four named entries:
#' 
#' "kind": character setting one of (currently) two ways to determine a hold
#' 
#' "mindist": numeric: minimum distance from home that the hold has to occur 
#' at, given in position units as set below
#' 
#' "threshold": numeric setting maximum velocity or distance in position and
#' time units as set below
#'   
#' "epoch": numeric duration of the hold in time units specified below
#' 
#' `kind` can be one of "sample-velocity" or "epoch-distance". When it is
#' "sample-velocity", a sequence of samples spanning the hold epoch all should
#' have velocity below the threshold value. When it is "epoch-distance" the 
#' total distance moved during the epoch should be beloc the threshold value.
#' 
#' All data _after_ the hold is trimmed, but the hold itself is not.
#' 
#' May give unexpected results if used in combination with `firstMove` or any
#' `velocity` criterion.
#' @param device The position columns to use are given by "[device]x_[posunit]"
#' in the `trialdf`, and similar for y. Can be something like 'hand', 'cursor',
#' 'mouse', 'stylus' or 'robot'.
#' @param posunit The unit used for the x and y position data. Could be "pix"
#' or "cm", or whatever is used in the data. Default: "pix".
#' @param timeunit The unit used for the time stamps of each sample. The column
#' names is "time_[timeunit]". Default: "ms"
#' @param homepos The coordinates of the home position. Default is (0,0).
#' @return Data frame describing the reach, minus the trimmed parts.
#' @description
#' ?
#' @details
#' ?
#' @examples
#' ?
#' @export
trimReach <- function(trialdf, homeStart=NA, targetReached=NA, velocity=0.05, firstMove=FALSE, holdHome=NA, untilHold=NA, device='hand', posunit='pix', timeunit='ms', homepos=c(0,0)) {
  
  targetposition <- as.numeric( trialdf[ 1, c( sprintf('targetx_%s', posunit ), sprintf( 'targety_%s', posunit ) ) ] )
  targetposition <- targetposition - homepos
  targetdistance <- sqrt( sum( targetposition^2 ) )
  
  nsamples <- dim(trialdf)[1]
  #cat(sprintf('** start with %d samples\n',nsamples))
  # cat('-----\n')
  
  if (!is.na(homeStart)) {
    
    # we need the device position, relative to the home position
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    
    if (is.numeric(homeStart)) {
      cutoff <- homeStart
    }
    
    if (is.character((homeStart))) {
      
      # cutoff at a percentage from home to target in whatever unit is used
      if (substring(homeStart,1,2) == 'pr') {
        
        cutoff <- as.numeric(substring(homeStart, 3))
        cutoff <- cutoff * targetdistance
        
      }
      
    }
    
    # get the distance from home:
    devicedist <- sqrt(x^2 + y^2)
    
    if (devicedist[1] > cutoff) {
      
      # cat('first sample too far from home\n')
      
      # find the first sample, where device is closer to home than the cutoff:
      if (any(devicedist < cutoff)) {
        rown <- max(1, min(which(devicedist < cutoff))-1) # why the minus one?
        trialdf <- trialdf[c(rown:dim(trialdf)[1]),]
      }
      
    }
    
    # if (dim(trialdf)[1]<nsamples) {
    #   newsamples <- dim(trialdf)[1]
    #   cat(sprintf('homeStart cuts %d samples\n', nsamples-newsamples))
    # }
    
  }
  
  if (!is.na(targetReached) && is.numeric(targetReached)) {
    
    # we need the trajectroy and device position, relative to the home position
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    targetx <- trialdf[1,sprintf('targetx_%s',posunit)] - homepos[1]
    targety <- trialdf[1,sprintf('targety_%s',posunit)] - homepos[1]
    
    # distance to target for every sample:
    dist <- sqrt((x - targetx)^2 + (y - targety)^2)
    
    crit <- which(dist < targetReached)
    # we only trim if the target is actually reached:
    if (length(crit) > 0) {
      trialdf <- trialdf[c(1:crit[1]),]
    }
    
  }
  
  if (!is.na(holdHome) && is.list(holdHome) && (length(holdHome) == 2)) {
    epoch <- holdHome$epoch
    distance <- holdHome$distance
    
    # first we get the necessary variables from the data frame:
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    sample_time <- trialdf[,sprintf('time_%s',timeunit)]
    
    # only look within distance:
    didx <- which(sqrt(x^2 + y^2) < distance)
    
    for (sample.idx in didx) {
      tidx <- which((sample_time[c(1:sample.idx)]-sample_time[sample.idx]) > (-epoch))
      
      if (all(sqrt(x[tidx]^2 + y[tidx]^2) < distance)) {
        # the trial now includes the hold period:
        trialdf <- trialdf[c(min(tidx):dim(trialdf)[1]),]
        break() # break out of the for-loop
      }
    }
    
  }
  
  if (!is.na(untilHold) && is.list(untilHold) && (length(untilHold) == 4)) {
    
    # here we use sample-to-sample velocity as used during the experiment
    # (so no smoothed / splined trajectory)
    
    # first we get the necessary variables from the data frame:
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    sample_time <- trialdf[,sprintf('time_%s',timeunit)]
    
    if (untilHold$kind == 'sample-velocity') {
      # calculate instantaneous velocity:
      velocity <- c(0,sqrt(diff(x)^2 + diff(y)^2) / diff(sample_time))
      print(velocity)
      # which samples are below the velocity criterion:
      belowcriterion <- which(velocity < untilHold$threshold)
      
      # this might be helpful for non-averaged hold criterion algorithms:
      bc_runs <- rle(belowcriterion) # no idea how to continue...
      
      # STUFF: NOT COMPLETE!
      cat('sample-velocity method of determining a hold is not complete:\nnot trimming\n')
      
    }
    
    if (untilHold$kind == 'epoch-distance') {
      
      # only look beyond mindist:
      didx <- which(sqrt(x^2 + y^2) > untilHold$mindist)
      
      for (sample.idx in didx) {
        tidx <- which((sample_time[c(1:sample.idx)]-sample_time[sample.idx]) > (-untilHold$epoch))
        # print(sum(sqrt(diff(x[tidx])^2 + diff(y[tidx])^2)))
        if (sum(sqrt(diff(x[tidx])^2 + diff(y[tidx])^2)) < untilHold$threshold) {
          # the trial now includes the hold period:
          trialdf <- trialdf[c(1:max(tidx)),]
          break() # break out of the for-loop
        }
      }
      
    }
    
  }
  
  if (!is.na(velocity)) {
    
    # here we use a spline smoothed velocity signal:
    x <- trialdf[,sprintf('%sx_%s',device,posunit)] - homepos[1]
    y <- trialdf[,sprintf('%sy_%s',device,posunit)] - homepos[2]
    sample_time <- trialdf[,sprintf('time_%s',timeunit)]
    SplVel <- getSplinedVelocity(x=x, y=y, t=sample_time)
    velocity <- SplVel$velocity
    spline_time <- SplVel$time
    
    # determine numeric velocity threshold
    
    # see which samples are below this threshold
    if (!is.na(velocity)) {
      if (is.logical(firstMove) && firstMove) {
        
        # trim after first dip below threshold (after first going above it)
        
      } else {
        
        # trim after last dip below threshold (after first going above it) 
        
      }
      cat('velocity trimming is not completed\nnot trimming\n')
    }
    
  }
  
  return(trialdf)
  
}
