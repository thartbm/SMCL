

#' @title Execute a simple model given parameters and a reach 
#' deviation schedule.
#' @param par A named vector with the model parameter (see details).
#' @param schedule A vector of length N with the perturbation schedule.
#' @return A data frame with one column: `output`, and N rows, so that each row
#' has the output of the modeled process on each trials.
#' @description This function is part of a set of functions to fit and 
#' evaluate an exponential decay model with asymptote.
#' @details The `par` argument is a named numeric vector that should have the
#' following element:
#' - lambda: learning rate
#' - N0: asymptote
#' 
#' The schedule usually consists of a sequence of ones. It will be multiplied
#' by the asymptote.
#' @examples
#' ?
#' @export
asymptoticDecayModel <- function(par, schedule) {
  
  # the process and error states are initialized at 0:
  Pt <- 0
  Et <- 0
  
  # the total output is stored here:
  output <- c()
  
  for (t in c(1:length(schedule))) {
    
    Pt <- Pt - (par['lambda'] * Et)
    
    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Pt + (schedule[t] * par['N0'])
    }
    
    # at this point we save the process state in our vector:
    output <- c(output, Pt)
    
  }
  
  return(data.frame(output))
  
}

#' @title Get the MSE for how well an asymptotic decay model fits reaches.
#' @param par A named numeric vector with the model parameter (see 
#' asymptoticDecayModel).
#' @param schedule A numeric vector of length N with the perturbation schedule.
#' @param reaches A numeric vector of length N with reach deviations matching
#' the perturbation schedule.
#' @return A float: the mean squared error between the total model output and
#' the reach deviations.
#' @description This function is part of a set of functions to fit and 
#' evaluate exponential decay model with asymptote..
#' @details The `par` argument is a named numeric vector that should have the
#' following element:
#' - lambda: the learning rate
#' - N0: the asymptote
#' 
#' The schedule is usually a sequence of ones, which is multiplied by the 
#' asymptote in the function.
#' @examples
#' ?
#' @export
asymptoticDecayMSE <- function(par, schedule, signal) {
  
  MSE <- mean((asymptoticDecayModel(par, schedule)$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}

#' @title Fit an asymptotic decay model to reach deviations.
#' @param schedule A vector of length N with the perturbation schedule.
#' @param reaches A vector of length N with reach deviation data.
#' @return A named numeric vector with the optimal parameter that fits a simple
#' rate model to the data as best as possible, with these elements:
#' - lambda: the rate of change
#' - N0: the asymptote 
#' @description This function is part of a set of functions to fit and 
#' evaluate a simple learning rate model of motor learning.
#' 
#' The schedule should usually be a sequence of ones. The reach deviations have 
#' to be baselined (but the baseline is cut from the data).
#' @details
#' ?
#' @examples
#' # write example!
#' @export
asymptoticDecayFit <- function(schedule, signal, gridpoints=11, gridfits=10) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  maxAsymptote <- 2*max(abs(signal), na.rm=TRUE)
  
  # define the search grid:
  searchgrid <- expand.grid('lambda' = parvals, 
                            'N0'     = parvals * maxAsymptote)
  
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=asymptoticDecayMSE, MARGIN=c(1), schedule=schedule, signal=signal)
  
  # testing if optimx is installed and making it available it so:
  optimxInstalled <- require("optimx")
  
  if (optimxInstalled) {
    
    # run optimx on the best starting positions:
    allfits <- do.call("rbind",
                       apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                              MARGIN=c(1),
                              FUN=optimx::optimx,
                              fn=asymptoticDecayMSE,
                              method='L-BFGS-B',
                              lower=c(0,0),
                              upper=c(1,maxAsymptote),
                              schedule=schedule,
                              signal=signal ) )
    
    # pick the best fit:
    win <- allfits[order(allfits$value)[1],]
    
    # return the best parameters:
    return(unlist(win[1:2]))
    
  } else {
    
    cat('(consider installing optimx, falling back on optim now)\n')
    
    # use optim with Nelder-Mead after all:
    allfits <- do.call("rbind",
                       apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                              MARGIN=c(1),
                              FUN=optim,
                              fn=asymptoticDecayMSE,
                              method='Nelder-Mead',
                              schedule=schedule,
                              signal=signal ) )
    
    # pick the best fit:
    win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
    
    # return the best parameters:
    return(win$par)
    
  }
  
}
