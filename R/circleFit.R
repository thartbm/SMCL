#' Shift localization responses so they are centred on the origin
#' 
#' @param df Data frame with localization coordinates (X,Y).
#' @param unit Unit of the coordinates (default: 'cm')
#' @param vrbl Variable of coordinates (default: 'tap')
#' @param r Radius of the circle the coordinates should be on (default: 1).
#' @param fitr (boolean) Should radius be fit? (default: FALSE)
#' @return The data frame with corrected \code{tapx_cm} and \code{tapy_cm} 
#' columns. The corrected localization responses fall closest to a circle with
#' radius \code{r} (in \code{unit}) and origin (0,0). Only response with
#' \code{df$selected == 1} are used for this correction.
#' @details The parameters \code{vrbl} and \code{unit} are combined with a lower
#' case \code{x} and \code{y}: \code{tapx_cm} and \code{tapy_cm} with default
#' settings. These should be columns in the data frame (\code{df}).
#' @examples
#' 
#' data("localization_aligned")
#' data("localization_unaligned")
#' 
#' localization_aligned <- convert2cm(localization_aligned, from='r')
#' localization_aligned <- convert2cm(localization_aligned, from='t')
#' 
#' par(mfrow=c(1,2))
#' 
#' plot(localization_aligned$tapx_cm, localization_aligned$tapy_cm, 
#'      main='aligned',xlab='cm', ylab='cm', 
#'      asp=1,bty='n', xlim=c(-2,12),ylim=c(-2,12),col='blue')
#' segments(localization_aligned$tapx_cm, localization_aligned$tapy_cm, 
#'          localization_aligned$handx_cm, localization_aligned$handy_cm, col='blue')
#' lines(c(-1,11),c(0,0),col='gray')
#' lines(c(0,0),c(-1,11),col='gray')
#' lines(cos(seq(0,pi/2,pi/200))*10,sin(seq(0,pi/2,pi/200))*10, col='gray')
#' centre <- circleFit(localization_aligned$tapx_cm, localization_aligned$tapy_cm, r=10)$par
#' points(centre['xc'], centre['yc'], col='red')
#' lines( (cos(seq(0,pi/2,pi/200))*10) + centre['xc'], (sin(seq(0,pi/2,pi/200))*10) + centre['yc'], col='red', lty=2)
#' points(localization_aligned$tapx_cm - centre['xc'], localization_aligned$tapy_cm - centre['yc'], col='green')
#' segments(localization_aligned$tapx_cm - centre['xc'], localization_aligned$tapy_cm - centre['yc'], 
#'          localization_aligned$handx_cm, localization_aligned$handy_cm, col='green')
#' 
#' 
#' localization_unaligned <- convert2cm(localization_unaligned, from='r')
#' localization_unaligned <- convert2cm(localization_unaligned, from='t')
#' 
#' plot(localization_unaligned$tapx_cm, localization_unaligned$tapy_cm, 
#'      main='unaligned',xlab='cm', ylab='cm', 
#'      asp=1,bty='n', xlim=c(-2,12),ylim=c(-2,12),col='blue')
#' segments(localization_unaligned$tapx_cm, localization_unaligned$tapy_cm, 
#'          localization_unaligned$handx_cm, localization_unaligned$handy_cm, col='blue')
#' lines(c(-1,11),c(0,0),col='gray')
#' lines(c(0,0),c(-1,11),col='gray')
#' lines(cos(seq(0,pi/2,pi/200))*10,sin(seq(0,pi/2,pi/200))*10, col='gray')
#' centre <- circleFit(localization_unaligned$tapx_cm, localization_unaligned$tapy_cm, r=10)$par
#' points(centre['xc'], centre['yc'], col='red')
#' lines( (cos(seq(0,pi/2,pi/200))*10) + centre['xc'], (sin(seq(0,pi/2,pi/200))*10) + centre['yc'], col='red', lty=2)
#' points(localization_unaligned$tapx_cm - centre['xc'], localization_unaligned$tapy_cm - centre['yc'], col='green')
#' segments(localization_unaligned$tapx_cm - centre['xc'], localization_unaligned$tapy_cm - centre['yc'], 
#'          localization_unaligned$handx_cm, localization_unaligned$handy_cm,, col='green')
#' @export
circleCorrect <- function(df, unit='cm', vrbl='tap', r=1, fitr=FALSE) {
  
  if ('selected' %in% names(df)) {
    idx <- which(df$selected == 1)
  } else {
    idx <- seq(1,dim(df)[1])
  }
  
  tapx <- df[idx,sprintf('%sx_%s',vrbl,unit)]
  tapy <- df[idx,sprintf('%sy_%s',vrbl,unit)]
  
  sol <- circleFit(X=tapx,
                   Y=tapy,
                   r=r,
                   fitr=fitr)
  
  # this also corrects the non-selected trials:
  df[,sprintf('%sx_%s',vrbl,unit)] <- df[,sprintf('%sx_%s',vrbl,unit)] - sol$par[['xc']]
  df[,sprintf('%sy_%s',vrbl,unit)] <- df[,sprintf('%sy_%s',vrbl,unit)] - sol$par[['yc']]
  
  return(df)
  
}

#' Fit a circle to data
#'
#' @param X Vector of X coordinates
#' @param Y Vector of Y coordinates
#' @param r Radius of the circle the coordinates should be on (default: 1).
#' @param fitr (boolean) Should radius be fit? (default: FALSE)
circleFit <- function(X, Y, r=1, fitr=FALSE) {
  
  control <- list('maxit'=10000, 'ndeps'=1e-9 )
  
  freepar <- c('xc'=0,'yc'=0)
  # we can set the starting values for the optimization to 0 
  # because the data is be pretty close to zero...
  # but that is not always true for other data sets!
  if (fitr) {
    freepar <- c(freepar, 'r'=r)
    setpar <-  c()
  } else {
    setpar  <- c('r'=r)
  }
  
  # do we want to use optimx here at some point?
  sol <- stats::optim(par=freepar, 
                      circleErrors, 
                      gr=NULL, 
                      X, 
                      Y, 
                      setpar=setpar, 
                      control=control)
  
  return(sol)
  
}

#' Get mean squared error between coordinates and a circle
#' 
#' @param freepar Vector with xc and yc parameters: x and y of the circle's middle.
#' Optionally also includes \code{r}: the radius of the circle.
#' @param X Vector of X coordinates
#' @param Y Vector of Y coordinates
#' @param setpar Vector that can have a fixed radius \code{r}, or can be empty.
#' @return The mean squared error between the distances of \code{X} and 
#' \code{Y} from the position in par and the radius \code{r}.
#' @export
circleErrors <- function(freepar,X,Y,setpar=c()) {
  
  par <- c(freepar, setpar)
  
  return(mean((sqrt((X-par[['xc']])^2+(Y-par[['yc']])^2)-par['r'])^2))
  
}
