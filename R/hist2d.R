
#' @title Get frequencies for a 2D histogram
#' @param X Vector of X-coordinates, or a Mx2 matrix with M sets of X and Y 
#' coordinates, or data frame with x and y column.
#' @param Y If X is not a matrix or data frame, but a vector this has to be a 
#' vector of equal length with Y coordinates.
#' @param nbins A numeric vector with two integers: the number of bins for X 
#' and for Y coordinates, defaults to c(25, 25).
#' @param edges A list with the edges for the X and Y coordinates respectively.
#' This allows irregular bin sizes. Note that the number of edges is the number
#' of bins plus 1. If `edges` is given it overrides `nbins`.
#' @return A list with three entries:
#' - `freq2D` numeric matrix with counts for each bin
#' - `x.edges` numeric vector with the edges used for x coordinates
#' - `y.edges` numeric vector with the edges used for y coordinates
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
hist2d <- function(x, y=NA, nbins=c(25,25), edges=NA) {
  
  if (is.data.frame(x)) {
    # check shape of x?
    df <- x
  } else if (is.matrix(x)) {
    # check shape of x?
    df <- as.data.frame(x)
  } else {
    df <- data.frame('x'=x, 'y'=y)
  }
  
  # http://stackoverflow.com/questions/18089752/r-generate-2d-histogram-from-raw-data
  
  if (is.numeric(nbins)) {
    x.edges <- seq(floor(min(df[,1])), ceiling(max(df[,1])), length=nbins[1])
    y.edges <- seq(floor(min(df[,2])), ceiling(max(df[,2])), length=nbins[2])
  }
  
  if (is.list(edges)) {
    x.edges <- edges[[1]]
    y.edges <- edges[[2]]
  }
  
  freq <-  as.data.frame(table(findInterval(df[,1], x.edges),findInterval(df[,2], y.edges)))
  freq[,1] <- as.numeric(freq[,1])
  freq[,2] <- as.numeric(freq[,2])
  
  freq2D <- matrix(data=0, ncol=length(y.edges), nrow=length(x.edges))
  freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]
  
  return(list('freq2D'=freq2D, 'x.edges'=x.edges, 'y.edges'=y.edges))
  
}