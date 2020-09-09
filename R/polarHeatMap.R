
#' @title Create a plot with a polar heatmap.
#' @param x Vector of M+1 angular bin edges.
#' @param y Vector of N+1 distance bin edges.
#' @param z Matrix (MxN) with two-dimensional histogram (frequency data), for 
#' example from SMCL::hist2d.
#' @param mincol Vector of length 3 with RGB values [0-1] for the minimum value
#' in z.
#' @param maxcol Vector of length 3 with RGB values [0-1] for the maximum value
#' in z. Other values in z are scaled, and each of the R, G and B values is 
#' linearly interpolated. No support for colormaps (yet). 
#' @param xlim Vector with values limiting the x axis of the resulting plot.
#' @param ylim Vector with values limiting the y axis of the resulting plot. By
#' default, the is set to c(-1,1) for both x and y, as this will include the 
#' full polar figure. Only change these values when the data do not cover a
#' circular data set.
#' @param xunit The angular unit (of the x-coordinates). Can be 'degrees' or
#' 'radians'. Will be converted to 'radians' if set in degrees.
#' @param border Draw borders around the parts of the heatmap. By default this
#' is set to NA, which means no border is drawn. If set to a numeric value, it
#' is used as the linewidth of the borders.
#' @param bordercol The color of the borders drawn around parts of the heatmap.
#' Default is 'white', can be any color accepted by base R graphics.
#' @param resolution The cicrular parts of the heatmap are approximated as a 
#' polygon, with at maximum this number of degrees between vectors. Default: 1.
#' @param alpha The alpha value of the heatmap. Does not apply to the border.
#' @param overlay Whether to put the polar heatmap into the active plot, or 
#' create a new one. Default is FALSE: create new plot.
#' @param origin The coordinates of the centre of the polar heatmap. Useful 
#' when overlay=TRUE. Defaults to (0,0).
#' @param scale The size of the polar heatmap. Useful when overlay=TRUE.
#' Defaults to 1.
#' @param main Main title when creating a new plot window.
#' @return 
#' ?
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
polarHeatMap <- function(x,y,z,mincol=c(0.94,0.98,0.99),maxcol=c(0.06,0.82,0.88),xlim=NA,ylim=NA,xunit='degrees',border=NA,bordercol='white',resolution=1,alpha=1,overlay=FALSE,origin=c(0,0),scale=1,main='') {
  
  # x: area edges in some form of angles (degrees [default] or radians)
  # y: area edges in distances from the origin
  # z: matrix with values in regions defined by x & y edges (NA to not plot the area)
  
  # maybe x/y/z can be a data frame?
  
  # mincol: color for the minimum values in z
  # maxcol: color for the maximum values in z
  
  if (any(is.na(xlim))) {
    # set xlim to fit all possible data:
    #xlim <- rep(max(abs(y)),2) * c(-1,1)
    xlim <- c(-1,1)
  }
  if (any(is.na(ylim))) {
    # set xlim to fit all possible data:
    #ylim <- rep(max(abs(y)),2) * c(-1,1)
    ylim <- c(-1,1)
  }
  
  resolution <- (resolution/180)*pi
  
  #par(mar=c(4, 4, 2, 2), pin=c(width,height))
  
  if (overlay == FALSE) {
  
    plot.new() # is this allowed... yes, but then everything else has to be manually added?
    
    graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)
    graphics::title(main = main)
    
  }
  # use `rgb()`
  
  # scale z to fit into 0-1 range:
  z <- z - min(z)
  z <- z / max(z)
  
  # convert to RGB values within the min/max colors:
  R <- (z * (maxcol[1]-mincol[1])) + mincol[1]
  G <- (z * (maxcol[2]-mincol[2])) + mincol[2]
  B <- (z * (maxcol[3]-mincol[3])) + mincol[3]
  
  # scale y to fit into the figure circle:
  y <- y / max(y)
  
  # get x to be in radians:
  if (xunit == 'degrees') {
    x <- (x / 180) * pi
  }
  
  # print(dim(z))
  # print(length(x))
  # print(length(y))
  
  allPolygons <- list()
  # create all the polygons
  for (xi in seq(dim(z)[1])) {
    for (yi in seq(dim(z)[2])) {
      if (!is.na(z[xi,yi])) {
        x1 <- x[xi]
        x2 <- x[xi+1]
        y1 <- y[yi]
        y2 <- y[yi+1]
        #print(c(x1,x2))
        xs <- seq(x1,x2,length.out=ceiling(abs(diff(c(x1,x2)))/resolution))
        X <- (c(cos(xs)*y1, rev(cos(xs)*y2)) * scale) + origin[1]
        Y <- (c(sin(xs)*y1, rev(sin(xs)*y2)) * scale) + origin[2]
        allPolygons[[length(allPolygons)+1]] <- list('x'=X, 'y'=Y, 'col'=grDevices::rgb(R[xi,yi],G[xi,yi],B[xi,yi],alpha=alpha))
      }
    }
  }
  
  for (p in allPolygons) {
    graphics::polygon(x=p$x, y=p$y, col=p$col, border=NA)
  }
  
  if (!is.na(border)) {
    for (p in allPolygons) {
      graphics::polygon(x=p$x, y=p$y, col=NA, border=bordercol, lwd=border)
    }
  }
  
  
}