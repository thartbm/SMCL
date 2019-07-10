

#' @title Get an eta-squared effect size to accompany a t-test.
#' @param g1 A numeric vector (scores for group 1).
#' @param g2 A numeric vector (scores for group 2).
#' @param mu A number for a single-sample t-test (default: 0).
#' @param na.rm Boolean: remove NA's from g1 and g2 (default: TRUE).
#' @return A number: the eta squared (effect size).
#' @description Calculate or bootstrap a confidence interval.
#' @details Eta-squared is a measure of effect size that expresses the 
#' propportion of variance explained by an effect. For a t-test, that 'effect'
#' is the separation of the data into two groups.
#' There is a way to do this for two groups, as well as for a single sample,
#' comparing the mean to a given number. There is no way (now) to do this for a
#' two-sample t-test comparing the difference in means to a hypothetical value,
#' nor does this function differentiate between single-sided or two-sided 
#' t-tests.
#' @examples
#' show example?
#' @export
etaSquaredTtest <- function(g1,g2=NA,mu=0,na.rm=TRUE) {
  
  doOneSample <- FALSE
  doTwoSample <- FALSE
  
  if (length(g2) == 1) {
    if (is.na(g2)) {
      doOneSample <- TRUE
    } else {
      # set mu to the single value in g2 and do a one sample one anyway?
    }
  } else {
    doTwoSample <- TRUE
  }
  
  if (doOneSample) {
    
    # compare group 1 mean with mu as explanation
    SStotal <- sum((g1-mean(g1,na.rm=na.rm))^2)
    SSeffect <- sum(((mean(g1, na.rm=na.rm) - mu)^2)*length(g1))
    # 
    # 
    return(SSeffect / SStotal)
    
  }
  
  if (doTwoSample) {
    
    overallmean <- mean(c(g1,g2),na.rm=na.rm)
    # compare overall mean with group means as explanation
    SStotal <- sum((c(g1,g2) - overallmean)^2, na.rm=na.rm)
    SSeffect <- sum(length(g1)*(mean(g1,na.rm=na.rm)-overallmean)^2, length(g2)*(mean(g2,na.rm=na.rm)-overallmean)^2)
    return(SSeffect / SStotal)
    
  }
  
}

