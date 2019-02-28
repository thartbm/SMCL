
convert2cm <- function(df, scale=NULL, home=NULL, from, replace=TRUE) {
  
  if (from %in% c('tap','touchscreen','t')) {
    # when the data comes from a touch screen / localization task
    coln <- list('origin'=c('tapx_px','tapy_px'),
                 'target'=c('tapx_cm','tapy_cm'))
    
    # set default scale / home, if undefined:
    # (might get them from somewhere else instead of the function definition at some point)
    if (is.null(scale)) {
      scale <- c(36.2885, 30.2038)
    }
    if (is.null(home)) {
      home <- c(17.8, 7.6)
    }
  }
  
  if (from %in% c('robot','r')) {
    # when the data comes from the robot
    if ('handx_m' %in% names(df)) {
      coln <- list('origin'=c('handx_m','handy_m'),
                   'target'=c('handx_cm','handy_cm'))
    } else if ('robotx_m' %in% names(df)) {
      coln <- list('origin'=c('robotx_m','roboty_m'),
                   'target'=c('robotx_cm','roboty_cm'))
    }
    
    # set default scale / home, if undefined:
    
    if (is.null(scale)) {
      scale <- c(0.01, 0.01)
    }
    if (is.null(home)) {
      home <- c(0.0, -8.5)
    }
  }
  
  if (replace) {
   
    df[coln[['origin']][1]] <- df[coln[['origin']][1]] / scale[1]
    df[coln[['origin']][2]] <- df[coln[['origin']][2]] / scale[2]
    
    df[coln[['origin']][1]] <- df[coln[['origin']][1]] - home[1]
    df[coln[['origin']][2]] <- df[coln[['origin']][2]] - home[2]
    
    names(df)[names(df) == coln[['origin']][1]] <- coln[['target']][1]
    names(df)[names(df) == coln[['origin']][2]] <- coln[['target']][2]

  } else {
    
    df[coln[['target']][1]] <- df[coln[['origin']][1]] / scale[1]
    df[coln[['target']][2]] <- df[coln[['origin']][2]] / scale[2]
    
    df[coln[['target']][1]] <- df[coln[['target']][1]] - home[1]
    df[coln[['target']][2]] <- df[coln[['target']][2]] - home[2]
    
  }

  return(df)
  
}
