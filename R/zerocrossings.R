#' @title Where does a signal cross zero? 
#' @param v A numeric vector containing the signal.
#' @param bounces A boolean: do bounces (a zero without sign change) count?
#' @return A vector with indices into `v` where the signal will cross zero.
#' @description This is a first attempt at a generic function to find zero
#' crossings in arbitrary signals. The indices that are returned, are the 
#' samples right before the signal crosses zero, and if bounces are allowed,
#' also the first sample of every bounce. Supposedly.
#' @details
#' @examples
#' ?
#' @export
zerocrossings <- function(v, bounces=FALSE) {
  
  output <- c()
  
  if (bounces) {
    
    zidx <- which(v==0) # bounces are only about samples that are exactly 0
    zidx <- zidx[which(zidx > 1 & zidx < length(v))] # first and last sample is not bounces
    
    bounce.idx <- zidx[which(diff(c(0,zidx)) > 1)] # we only want the first sample's index of every bounce
    
    output <- c(output, bounce.idx)
    
  }
  
  didx <- which(v!=0) # crossings detected on all non-zero samples
  signs <- sign(v[didx]) # get a vector of the signs of the non-zero samples
  cross.idx <- didx[which(abs(diff(signs)) == 2)] # whenever the abs diff between two of the sign entries equals 2
  # zero has been crossed
  
  output <- sort(c(output, cross.idx))
  
  return(output)
  
}