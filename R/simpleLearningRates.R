


#' @title Evaluate a simple learning rate model given parameters and a reach 
#' deviation schedule.
#' @param par A named vector with the model parameter (see details).
#' @param schedule A vector of length N with the perturbation schedule.
#' @return A data frame with one column: `output`, and N rows, so that each row
#' has the output of the modeled process on each trials.
#' @description This function is part of a set of functions to fit and 
#' evaluate a simple learning rate model of motor learning.
#' @details The `par` argument is a named numeric vector that should have the
#' following element:
#' - LR: the learning rate
#' @examples
#' ?
#' @export
simpleRateModel <- function(par, schedule) {
  
  # the process and error states are initialized at 0:
  Pt <- 0
  Et <- 0
  
  # the total output is stored here:
  output <- c()
  
  for (t in c(1:length(schedule))) {
    
    Pt <- Pt - (par['LR'] * Et)
    
    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Pt + schedule[t]
    }
    
    # at this point we save the process state in our vector:
    output <- c(output, Pt)
    
  }
  
  return(data.frame(output))
  
}


#' @title Get the MSE for how well a simple learning rate model fits reaches.
#' @param par A named numeric vector with the model parameter (see 
#' simpleRateModel).
#' @param schedule A numeric vector of length N with the perturbation schedule.
#' @param reaches A numeric vector of length N with reach deviations matching
#' the perturbation schedule.
#' @return A float: the mean squared error between the total model output and
#' the reach deviations.
#' @description This function is part of a set of functions to fit and 
#' evaluate a simple learning rate model of motor learning.
#' @details The `par` argument is a named numeric vector that should have the
#' following element:
#' - LR: the learning rate
#' @examples
#' ?
#' @export
simpleRateMSE <- function(par, schedule, reaches) {
  
  #bigError <- mean(schedule^2, na.rm=TRUE) * 2
  
  return( mean((simpleRateModel(par, schedule)$output - reaches)^2, na.rm=TRUE) )
  
}


#' @title Fit the simple learning rate model to reach deviations.
#' @param schedule A vector of length N with the perturbation schedule.
#' @param reaches A vector of length N with reach deviation data.
#' @return A named numeric vector with the optimal parameter that fits a simple
#' rate model to the data as best as possible, with these elements:
#' - LR: the learning rate
#' @description This function is part of a set of functions to fit and 
#' evaluate a simple learning rate model of motor learning.
#' @details
#' ?
#' @examples
#' # write example!
#' @export
simpleRateFit <- function(schedule, reaches, gridpoints=10, gridfits=4) {
  
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  searchgrid <- expand.grid('LR'=parvals)
  
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=simpleRateMSE, MARGIN=c(1), schedule=schedule, reaches=reaches)
  
  optimxInstalled <- require("optimx")
  
  if (optimxInstalled) {
    
    # run optimx on the best starting positions:
    allfits <- do.call("rbind",
                       apply( data.frame('LR'=searchgrid[order(MSE)[1:gridfits],]),
                              MARGIN=c(1),
                              FUN=optimx::optimx,
                              fn=simpleRateMSE,
                              method='L-BFGS-B',
                              lower=c(0),
                              upper=c(1),
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(allfits$value)[1],]
    
    # return the best parameters:
    return(unlist(win[1]))
    
  } else {
    
    cat('(consider installing optimx, falling back on optim now)\n')
    
    # use optim with Nelder-Mead after all:
    allfits <- do.call("rbind",
                       apply( data.frame('LR'=searchgrid[order(MSE)[1:gridfits],]),
                              MARGIN=c(1),
                              FUN=optim,
                              fn=simpleRateMSE,
                              method='Nelder-Mead',
                              schedule=schedule,
                              reaches=reaches ) )
    
    # pick the best fit:
    win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
    
    # return the best parameters:
    return(win$par)
    
  }
  
}