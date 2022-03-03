#' @title Linearly mix two colors given some weights.
#' @param a an RGB color string
#' @param b an RGB color string
#' @param balance two weights, one for each color, default: c(1,1)
#' @return An RGB color string.
#' @description Returns a weighed mixed average of the two input colors.
#' Adds an alpha channel if not already present.
#' @examples
#' ?
#' @export
mixCol <- function(a='#41ffc9', b='#1d7791', balance=c(1,1)) {
  
  a <- col2rgb(a,alpha=TRUE)/255
  b <- col2rgb(b,alpha=TRUE)/255
  
  w <- balance / sum(balance)
  
  R <- (a[1]*w[1]) + (b[1]*w[2])
  G <- (a[2]*w[1]) + (b[2]*w[2])
  B <- (a[3]*w[1]) + (b[3]*w[2])
  A <- (a[4]*w[1]) + (b[4]*w[2])
  
  return(rgb(red=R, green=G, blue=B, alpha=A))
  
}

#' @title Modify a color's saturation
#' @param col an RGB color
#' @param sat.mult saturation multiplier
#' @return An RGB color string.
#' @description Converts the RGB color to values in HSV space, then multiplies
#' the saturation component (S) by the given multiplier, leaving the hue (H)
#' and value (V) components as they were. The results is converted back to 
#' RGB strings before they are returned.
#' @examples
#' ?
#' @export
satCol <- function(col='#41ffc9', sat.mult=1.25) {
  
  inter <- rgb2hsv(col2rgb(col))
  
  sat.mult  <- max(0, sat.mult) 
  inter[2,] <- min(1, inter[2,] * sat.mult)
  
  return(hsv(inter[1,], inter[2,], inter[3,]))
  
}

## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

#' @title Set a color's alpha to a percentage.
#' @param color an RGB color
#' @param percent percentage transparency
#' @param name optional name for the color
#' @return An RGB color string.
#' @description Sets the alpha value of the RGB color to match the given percentage
#' of transparency as closely as possible.
#' @examples
#' ?
#' @export
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  #invisible(t.col)
  return(t.col)
}
## END


#' @title Create linear palette in RGB color space
#' @param from first RGB color
#' @param to last RGB color
#' @param n number colors in output
#' @param alpha optional: if not NULL, alpha (transparency) value for all colors
#' @return A palette of colors of length `n` where the first and last color are
#' equal to `from` and `to`, and the R, G and B values are linearly interpolated
#' between those in `from` and `to`.
#' @description Create a color gradient to use for plotting continuous values.
#' @examples
#' ?
#' @export
linPal <- function(from='#FFFFFF', to='#E51636', n=256, alpha=NULL) {
  
  from <- col2rgb(from,alpha=TRUE)/255
  to   <- col2rgb(to,alpha=TRUE)/255
  
  R <- seq(from[1],to[1],length.out = n)
  G <- seq(from[2],to[2],length.out = n)
  B <- seq(from[3],to[3],length.out = n)
  A <- seq(from[4],to[4],length.out = n)
  
  if (!is.null(alpha)) {
    if (length(alpha) == 1) {
      A <- rep(alpha,n)
    }
    if (length(alpha) == 2) {
      A <- seq(alpha[1],alpha[2],length.out = n)
    }
  }
  
  return(rgb(red=R, green=G, blue=B, alpha=A))
  
}
