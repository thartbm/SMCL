
#' @title Evaluate the two-rate model given parameters and a reach deviation
#' schedule.
#' @param par A named vector with the four model parameters (see details).
#' @param schedule A vector of length N with the perturbation schedule.
#' @return A data frame with three columns: `slow`, `fast` and `total` and N 
#' rows, so that each row has the output of the slow and fast process on each
#' trials as well as the total system output.
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details The `par` argument is a named numeric vector that should have the
#' following elements:
#' - Ls: the slow learning rate
#' - Lf: the fast learning rate
#' - Rs: the slow retention rate
#' - Rf: the fast retention rate 
#' @examples
#' ?
#' @export
twoRateModel <- function(par, schedule) {
  
  # thse values should be zero at the start of the loop:
  Et <- 0 # previous error: none
  St <- 0 # state of the slow process: aligned
  Ft <- 0 # state of the fast process: aligned
  
  # we'll store what happens on each trial in these vectors:
  slow <- c()
  fast <- c()
  total <- c()
  
  # now we loop through the perturbations in the schedule:
  for (t in c(1:length(schedule))) {
    
    # first we calculate what the model does
    # this happens before we get visual feedback about potential errors
    St <- (par['Rs'] * St) - (par['Ls'] * Et)
    Ft <- (par['Rf'] * Ft) - (par['Lf'] * Et)
    Xt <- St + Ft
    
    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Xt + schedule[t]
    }
    
    # at this point we save the states in our vectors:
    slow <- c(slow, St)
    fast <- c(fast, Ft)
    total <- c(total, Xt)
    
  }
  
  # after we loop through all trials, we return the model output:
  return(data.frame(slow,fast,total))
  
}







#' @title Get the MSE for how well the two-rate model fits reaches.
#' @param par A named numeric vector with the four model parameters (see 
#' twoRateModel).
#' @param schedule A numeric vector of length N with the perturbation schedule.
#' @param reaches A numeric vector of length N with reach deviations matching
#' the perturbation schedule.
#' @return A float: the mean squared error between the total model output and
#' the reach deviations.
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details The `par` argument is a named numeric vector that should have the
#' following elements:
#' - Ls: the slow learning rate
#' - Lf: the fast learning rate
#' - Rs: the slow retention rate
#' - Rf: the fast retention rate 
#' @examples
#' ?
#' @export
twoRateMSE <- function(par, schedule, reaches) {
  
  bigError <- mean(schedule^2, na.rm=TRUE) * 10
  
  # learning and retention rates of the fast and slow process are constrained:
  if (par['Ls'] > par['Lf']) {
    return(bigError)
  }
  if (par['Rs'] < par['Rf']) {
    return(bigError)
  }
  
  return( mean((twoRateModel(par, schedule)$total - reaches)^2, na.rm=TRUE) )
  
}



#' @title Fit the two-rate model to reach deviations.
#' @param schedule A vector of length N with the perturbation schedule.
#' @param reaches A vector of length N with reach deviation data.
#' @return A named numeric vector with the optimal parameters that fit the two
#' rate model to the data as best as possible, with these elements:
#' - Ls: the slow learning rate
#' - Lf: the fast learning rate
#' - Rs: the slow retention rate
#' - Rf: the fast retention rate 
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details
#' ?
#' @examples
#' # there is example data in this package:
#' data("tworatedata")
#' 
#' # first we baseline it, and get a median for every trial:
#' baseline <- function(reachvector,blidx) reachvector - mean(reachvector[blidx], na.rm=TRUE)
#' tworatedata[,4:ncol(tworatedata)] <- apply(tworatedata[,4:ncol(tworatedata)], FUN=baseline, MARGIN=c(2), blidx=c(17:32))
#' reaches <- apply(tworatedata[4:ncol(tworatedata)], FUN=median, MARGIN=c(1), na.rm=TRUE)
#' 
#' # and we extract the schedule:
#' schedule <- tworatedata$schedule
#' 
#' # now we can fit the model to the reaches, given the schedule:
#' par = twoRateFit(schedule, reaches)
#' 
#' # and plot that:
#' model <- twoRateModel(par=par, schedule=schedule)
#' plot(reaches,type='l',col='#333333',xlab='trial',ylab='reach deviation [deg]',xlim=c(0,165),ylim=c(-35,35),bty='n',ax=F)
#' lines(c(1,33,33,133,133,145,145),c(0,0,30,30,-30,-30,0),col='#AAAAAA')
#' lines(c(145,164),c(0,0),col='#AAAAAA',lty=2)
#' lines(model$slow,col='blue')
#' lines(model$fast,col='red')
#' lines(model$total,col='purple')
#' axis(1,c(1,32,132,144,164),las=2)
#' axis(2,c(-30,-15,0,15,30))
#' 
#' @export
twoRateFit <- function(schedule, reaches, gridpoints=6, gridfits=6) {
  
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  searchgrid <- expand.grid('Ls'=parvals,
                            'Lf'=parvals,
                            'Rs'=parvals,
                            'Rf'=parvals)
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=twoRateMSE, MARGIN=c(1), schedule=schedule, reaches=reaches)
  
  optimxInstalled <- require("optimx")
  
  if (optimxInstalled) {
    
    # run optimx on the best starting positions:
    allfits <- do.call("rbind",
                       apply( searchgrid[order(MSE)[1:gridfits],],
                              MARGIN=c(1),
                              FUN=optimx::optimx,
                              fn=twoRateMSE,
                              method='L-BFGS-B',
                              lower=c(0,0,0,0),
                              upper=c(1,1,1,1),
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(allfits$value)[1],]
    
    # return the best parameters:
    return(unlist(win[1:4]))
    
  } else {
    
    cat('(consider installing optimx, falling back on optim now)\n')
    
    # use optim with Nelder-Mead after all:
    allfits <- do.call("rbind",
                       apply( searchgrid[order(MSE)[1:gridfits],],
                              MARGIN=c(1),
                              FUN=optim,
                              fn=twoRateMSE,
                              method='Nelder-Mead',
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
    
    # return the best parameters:
    return(win$par)
    
  }
  
}



#' @title Evaluate a one-rate model given parameters and a reach deviation
#' schedule.
#' @param par A named vector with the two model parameters (see details).
#' @param schedule A vector of length N with the perturbation schedule.
#' @return A data frame with one column, and N rows, so that each row has the 
#' output of the process on each trial as well as the total system output.
#' @description This function is part of a set of functions to fit and 
#' evaluate the one-rate model of motor learning.
#' @details The `par` argument is a named numeric vector that should have the
#' following elements:
#' - L: the learning rate
#' - R: the retention rate
#' @examples
#' ?
#' @export
oneRateModel <- function(par, schedule) {
  
  # thse values should be zero at the start of the loop:
  Et <- 0 # previous error: none
  Pt <- 0 # state of slow process: aligned

  # we'll store what happens on each trial in these vectors:
  process <- c()

  # now we loop through the perturbations in the schedule:
  for (t in c(1:length(schedule))) {
    
    # first we calculate what the model does
    # this happens before we get visual feedback about potential errors
    Pt <- (par['R'] * Pt) - (par['L'] * Et)

    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Pt + schedule[t]
    }
    
    # at this point we save the states in our vectors:
    process <- c(process, Pt)

  }
  
  # after we loop through all trials, we return the model output:
  return(data.frame(process))
  
}







#' @title Get the MSE for how well the one-rate model fits reaches.
#' @param par A named numeric vector with the two model parameters (see 
#' details).
#' @param schedule A numeric vector of length N with the perturbation schedule.
#' @param reaches A numeric vector of length N with reach deviations matching
#' the perturbation schedule.
#' @return A float: the mean squared error between the total model output and
#' the reach deviations.
#' @description This function is part of a set of functions to fit and 
#' evaluate the one-rate model of motor learning.
#' @details The `par` argument is a named numeric vector that should have the
#' following elements:
#' - L: the learning rate
#' - R: the retention rate
#' @examples
#' ?
#' @export
oneRateMSE <- function(par, schedule, reaches) {
  
  bigError <- mean(schedule^2, na.rm=TRUE) * 10
  
  return( mean((oneRateModel(par, schedule)$process - reaches)^2, na.rm=TRUE) )
  
}



#' @title Fit the one-rate model to reach deviations.
#' @param schedule A vector of length N with the perturbation schedule.
#' @param reaches A vector of length N with reach deviation data.
#' @return A named numeric vector with the optimal parameters that fit the two
#' rate model to the data as best as possible, with these elements:
#' - L: the learning rate
#' - R: the retention rate
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details
#' ?
#' @examples
#' # there is example data in this package:
#' data("tworatedata")
#' 
#' # first we baseline it, and get a median for every trial:
#' baseline <- function(reachvector,blidx) reachvector - mean(reachvector[blidx], na.rm=TRUE)
#' tworatedata[,4:ncol(tworatedata)] <- apply(tworatedata[,4:ncol(tworatedata)], FUN=baseline, MARGIN=c(2), blidx=c(17:32))
#' reaches <- apply(tworatedata[4:ncol(tworatedata)], FUN=median, MARGIN=c(1), na.rm=TRUE)
#' 
#' # and we extract the schedule:
#' schedule <- tworatedata$schedule
#' 
#' # now we can fit the model to the reaches, given the schedule:
#' par = oneRateFit(schedule, reaches)
#' 
#' # and plot that:
#' model <- oneRateModel(par=par, schedule=schedule)
#' plot(reaches,type='l',col='#333333',xlab='trial',ylab='reach deviation [deg]',xlim=c(0,165),ylim=c(-35,35),bty='n',ax=F)
#' lines(c(1,33,33,133,133,145,145),c(0,0,30,30,-30,-30,0),col='#AAAAAA')
#' lines(c(145,164),c(0,0),col='#AAAAAA',lty=2)
#' lines(model$process,col='purple')
#' axis(1,c(1,32,132,144,164),las=2)
#' axis(2,c(-30,-15,0,15,30))
#' 
#' @export
oneRateFit <- function(schedule, reaches, gridpoints=6, gridfits=6) {
  
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  searchgrid <- expand.grid('L'=parvals,
                            'R'=parvals)
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=oneRateMSE, MARGIN=c(1), schedule=schedule, reaches=reaches)
  
  optimxInstalled <- require("optimx")
  
  if (optimxInstalled) {
    
    # run optimx on the best starting positions:
    allfits <- do.call("rbind",
                       apply( searchgrid[order(MSE)[1:gridfits],],
                              MARGIN=c(1),
                              FUN=optimx::optimx,
                              fn=oneRateMSE,
                              method='L-BFGS-B',
                              lower=c(0,0),
                              upper=c(1,1),
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(allfits$value)[1],]
    
    # return the best parameters:
    return(unlist(win[1:2]))
    
  } else {
    
    cat('(consider installing optimx, falling back on optim now)\n')
    
    # use optim with Nelder-Mead after all:
    allfits <- do.call("rbind",
                       apply( searchgrid[order(MSE)[1:gridfits],],
                              MARGIN=c(1),
                              FUN=optim,
                              fn=oneRateMSE,
                              method='Nelder-Mead',
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
    
    # return the best parameters:
    return(win$par)
    
  }
  
}


#' @title Evaluate two-rate model fits.
#' @param groupdata A named vector with data for each group. Each element
#' should be a list (or data frame) with 2 named entries (or columns):
#' 
#' "schedule": the perturbation for each trial, used for fitting models
#' 
#' "reaches": the actual reaches made by this group on each trial, also used
#' for fitting models (if estN='aclag' reaches can't contain missing values)
#' 
#' The labels in the vector should correspond to group names. They will be used
#' in the output, and to match specifications of N to the correct data.
#' @param estN This is either a string, specifying a method to estimate the
#' number of observations for calculating an AIC. Alternatively, this is a 
#' vector with named entries where the names are equal to the groups in 
#' `groupdata` and the value is the N to use for that group.
#' 
#' Named methods are the same as in `seriesEffectiveSampleSize`.
#' @param compOne A boolean specifying whether or not to compare the two-rate
#' model fit to a one-rate model fit for each group.
#' @return A data frame with a row for each group. This will give the estimated
#' parameter values, an MSE, N and AIC. It will list relative log likelihoods 
#' too, comparing the groups two-rate AICs, as well as within each group the 
#' AICs for a one- and two-rate model fit, if requested (see `compOne`).
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details
#' ?
#' @examples
#' ?
#' @export
twoRates <- function(groupdata, estN='ac_one', compOne=FALSE) {
  
  groupnames <- names(groupdata)
  
  if (is.numeric(estN)) {
    
    if (!all(groupnames %in% names(estN))) {
      stop('Not all groups in the data have an N specified.\n')
    } else {
      # all is good
      # we put this in a vector that aclag will create:
      observations <- estN
    }


    # all should be good now...
  } else if (is.character(estN)) {
    
    observations <- c()
    
    # determine number of independent observations for each group:
    for (group in groupnames) {
      
      # use reaches to determine number of independent observations:
      reaches <- groupdata[group]$reaches
      
      observations[group] <- seriesEffectiveSampleSize(reaches, method=estN)
    
    }
    
  } else {
    stop('Either specify "estN" as a numeric vector of N or a character naming an available method.\n')
  }
  
  # now do the actual modelling on all datasets?
  
  
  
  
}

#' @title Estimate number of independent samples in a time series.
#' @param series numeric vector with the time series data.
#' @param method character specifying the method to find the number of 
#' independent samples in the time series data vector.
#' 
#' "ac_one": this uses the auto-correlation at lag 1 as ρ, and then gets
#' n_eff = n * ( (1−ρ)/(1+ρ) )
#' where n is the number of trials in the sequence (default)
#' 
#' "ac_lag.10": this divides the number of trials by the lag at which the auto-
#' correlation is about to go below 0.10:
#' n_eff = n / lag
#' (if AC is higher than 0.10, increase lag and redo, else use previous n_eff)
#' 
#' "ac_lag95%CI": this is similar to "ac_lag.10" but divides the number of 
#' trials by the lag at which the autocorrelation is about to be within the 95%
#' confidence interval, as determined by bootstrapping (with 1000 re-samples)
#' 
#' @return numeric value with the estimated number of independent observation
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details
#' ?
#' @examples
#' ?
#' @export
seriesEffectiveSampleSize <- function(series, method='ac_one') {
  
  # https://dsp.stackexchange.com/questions/47560/finding-number-of-independent-samples-using-autocorrelation
  # The "Effective" Number of Independent Observations in an An′ = n * ( (1−ρ)/(1+ρ) )utocorrelated Time Series is a defined statistical term - https://www.jstor.org/stable/2983560?seq=1#page_scan_tab_contents
  # The number of independent observations n' of n observations with a constant variance but having a lag 1 autocorrelation ρ equals
  #
  # n′ = n * ( (1−ρ)/(1+ρ) )
  #
  # Also note this is an approximation valid for large n, [1] (reference provided by Ed V) equation 7 is more accurate for small n.
  # [1] N.F. Zhang, "Calculation of the uncertainty of the mean of autocorrelated measurements", Metrologia 43 (2006) S276-S281.
  
  
  
  # Maybe also relevant:
  # https://www.researchgate.net/post/How_do_you_choose_the_optimal_laglength_in_a_time_series
  
  
  
  if (method == 'ac_one') {
  
    # create empty vector for number of independent observations per group:
    observations <- c()
  
    rho <- acf(series, lag.max=1, plot=FALSE)$acf[2]
    
    return( length(series) *  ((1-rho)/(1+rho)) )
    
  } else if (method == 'ac_lag.10') {
  
    critlag <- which(acf(series, lag.max=length(series)-1, plot=FALSE)$acf < 0.1)
    
    if (length(critlag) == 0) {
      
      # autocorrelation is high throughout: we have 1 observation?
      
      return(1)
      
    } else {
      
      # the sequence of reaches doesn't autocorrelate well throughout,
      # so it can be split into (more or less) independent observations
      # (but we have at least 1)
      
      return( max( ( length(series) / (critlag[1]-2) ), 1) )
      
    }
    
  } else if (method == 'ac_lag95%CI') {
    
    Neff_found <- FALSE
    
    critlag <- 1
    
    Neff <- length(series)
    
    while (!Neff_found) {
      
      lagpoints <- length(series) - critlag
      
      point_one <- series[c(1:lagpoints)]
      point_two <- series[c((critlag+1):length(series))]
      
      lag_cor <- cor(point_one, point_two)
      
      shuffle_cor <- rep(NA, 1000)
      
      for (bootstrap in c(1:1000)) {
        
        shuffle_cor[bootstrap] <- cor(point_one, sample(point_two))
        
      }
      
      upperlimit <- quantile(shuffle_cor, probs=0.95)
      
      if (lag_cor < upperlimit) {
        
        return( length(series) / max((critlag - 1), 1) )
        
      }
      
      critlag <- critlag + 1
      
      # lag can only go up to a certain value, determined by the length of the sequence
      if (critlag > (length(series) - 2)) {
        
        return( length(series) ) # or length(reaches) - 1?
        
      }
      
    }
    
  }
  
  stop('Unrecognized method for determining effective sample size.\nUse one of: ac_one, ac_lag.10 or ac_lag95%CI\n')
  
}

#' @title Calculate model evaluation criteria (mainly AIC).
#' @param MSE numeric: The mean squared error between the model and data. This
#' can be a vector if multiple models are to be evaluated. If this vector has
#' named entries, they will be used as row names in the returned data frame.
#' @param k numeric: The number of parameters in the model. If MSE is a vector
#' k needs to be a vector of the same length.
#' @param N numeric: the number of independent observations (see the function:
#' `seriesEffectiveSampleSize` for some options. (Not a vector.)
#' @param n numeric: the number of observation. For a two-rate model this would
#' be the number of trials in the sequence. Necessary for calculating AICc, but
#' can be left NA to skip AICc.
#' @return data frame with values for several model evaluation criteria
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details
#' This function allows estimating model criteria based on the RMSE. This is
#' useful for models where no method exists to extract the log-likelihood. When
#' log-likelihood can be used, see `AIC`, `logLik` and `nobs` from {stats}.
#' 
#' If possible fit models by maximizing their likelihood.
#' 
#' The function calculates two model evaluation criteria:
#' 
#' "AIC": Akaike's Information Criterion
#' 
#' "AICc": The AIC, but with a correction for small sample sizes.
#' 
#' If there are 2 or more models evaluated, the relative likelihoods of the 
#' models, based on each criterion are also returned.
#' 
#' @examples
#' # get example data:
#' data("tworatedata")
#' 
#' # first we baseline it, and get a median for every trial:
#' baseline <- function(reachvector,blidx) reachvector - mean(reachvector[blidx], na.rm=TRUE)
#' tworatedata[,4:ncol(tworatedata)] <- apply(tworatedata[,4:ncol(tworatedata)], FUN=baseline, MARGIN=c(2), blidx=c(17:32))
#' reaches <- apply(tworatedata[4:ncol(tworatedata)], FUN=median, MARGIN=c(1), na.rm=TRUE)
#' 
#' # and we extract the schedule:
#' schedule <- tworatedata$schedule
#' 
#' # now we can fit the model to the reaches, given the schedule:
#' par <- twoRateFit(schedule, reaches)
#' MSE2 <- twoRateMSE(par, schedule, reaches)
#' 
#' # we do the same for the one-rate model:
#' par <- oneRateFit(schedule, reaches)
#' MSE1 <- oneRateMSE(par, schedule, reaches)
#' 
#' # effective N was calculated with "seriesEffectiveSampleSize" as: 6.833
#' 
#' modelCriteriaMSE(MSE=c('one-rate'=MSE1, 'two-rate'=MSE2), k=c(2,4), N=6.833, n=164)
#' 
#' @export
modelCriteriaMSE <- function(MSE, k, N, n=NA) {
  
  # MSE : our goodness of fit measure in lieu of actual likelihood
  # k   : number of parameters
  # N   : number of independent observations
  # n   : number of observations (number of trials for two-rate models)

  if (length(MSE) != length(k)) {
    stop('Arguments MSE and k need to be of the same length.\n')
  }
  if (any(c(length(MSE), length(k), length(N)) < 1)) {
    stop('All arguments must be at least of length 1.\n')
  }
  if (!is.numeric(MSE) | !is.numeric(k)) {
    stop('MSE and k need to be numeric.\n')
  }
  if (length(N) > 1 | !is.numeric(N)) {
    stop('N has to be a single numeric value.\n')
  }
  if (!is.na(n) && (length(n) > 1 | !is.numeric(n))) {
    stop('n has to be NA, or a single numeric value.\n')
  }
  
  # maximum likelihood:
  # L <- (-(n/2) * log(2*pi)) - ((n/2)*log(MSE)) - (1/(2*MSE)*(MSE*n))
  # but this sometimes results in negative likelihoods
  # the log_e of which causes problems later on 
  
  # n <- N
  
  # without the "constant" that Wikipedia mentions:
  # this is simpler, and I might replace the constant
  # L <- -(n/2) * log(MSE)
  
  # sometimes we now get inf or nan output, 
  # so we replace the constant to avoid this:
  # if (any(L < 1)) {
  #   L <- (L - min(L)) + 1
  # }
  
  #-- AIC --# 
  
  # Thomas calculation:
  # C <- N*(log(2*pi)+1) # what is this for? a penalty for large number of observations?
  # AIC <- (2 * k) + N*log(MSE) + C
  
  AIC <- (N * log(MSE)) + (2 * k)
  # AIC <- (2 * k) - (N * log(L))
  
  #-- AICc --#
  
  if (!is.na(n)) {
    # correction for low N (compared to k):
    AICc <- AIC + ( (2 * k^2) / (n - k - 1) )
  }
  
  #-- BIC --#
  
  #BIC <- log(N)*k - (2 * log(L))
  
  #-- Hannan-Quinn --#
  
  #HQC <- (-2 * L) + (2 * k * log(log(N)))
  
  if (length(MSE) == 1) {
    
    # return(data.frame('AIC'=AIC, 'AICc'=AICc, 'BIC'=BIC, 'HQC'=HQC))
    if (is.na(n)) {
      return(data.frame('AIC'=AIC))
    } else {
      return(data.frame('AIC'=AIC, 'AICc'=AICc))
    }
    
  } else {
    
    AIC.rl  <- relativeLikelihood( AIC  ) # exp( ( min( AIC  ) - AIC  ) / 2 )
    
    if (is.na(n)) {
      return(data.frame('AIC'=AIC,'AIC.rl'=AIC.rl))
    } else {
      AICc.rl <- relativeLikelihood( AICc )
      return(data.frame('AIC'=AIC,'AIC.rl'=AIC.rl, 'AICc'=AICc, 'AICc.rl'=AICc.rl))
    }
    # BIC.rl  <- relativeLikelihood( BIC  )
    # HQC.rl  <- relativeLikelihood( HQC  )
    
    # return(data.frame('AIC'=AIC,'AIC.rl'=AIC.rl, 'AICc'=AICc, 'AICc.rl'=AICc.rl, 'BIC'=BIC, 'BIC.rl'=BIC.rl, 'HQC'=HQC, 'HQC.rl'=HQC.rl))
    
  }
  
}

#' @title Calculate relative likelihood of models based on information criteria
#' @param crit a numeric vector with the same informations criterion, for
#' several models (fit on the same data).
#' @return Returns the relative likelihoods of the models
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details
#' This function returns the relative likelihood of a series of models based on
#' their scores on an information criterion (e.g. AIC or BIC). The best model 
#' will have a relative likelihood of 1, and models that have relative 
#' likelihoods between 1 and 0.05 are also good, while those below 0.05 can be 
#' considered worse than the best model.
#' @examples
#' ?
#' @export
relativeLikelihood <- function(crit) {
  
  return( exp( ( min( crit  ) - crit  ) / 2 ) )
  
}