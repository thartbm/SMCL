
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
