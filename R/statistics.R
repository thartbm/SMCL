

#' @title Get an eta-squared (for a t-test).
#' @param g1 A numeric vector (scores for group 1).
#' @param g2 A numeric vector (scores for group 2).
#' @param na.rm Boolean: remove NA's from g1 and g2.
#' @return A number: the eta squared (effect size).
#' @description Calculate or bootstrap a confidence interval.
#' @details Eta-squared is a measure of effect size that expresses the 
#' propportion of variance explained by an effect. For a t-test, that effect
#' is the separation of the data into two groups. (This test does not work for 
#' t-testsof one group against a single value and does not differentiate 
#' between two-sample and paired-sample t-tests.)
#' @examples
#' show example?
#' @export
etaSquaredTtest <- function(g1,g2,na.rm=TRUE) {
  
  SStotal <- sum((c(g1,g2) - mean(c(g1,g2),na.rm=na.rm))^2, na.rm=na.rm)
  SSeffect <- sum(c((g1-mean(g1,na.rm=na.rm))^2, (g2-mean(g2,na.rm=na.rm))^2), na.rm=na.rm)
  
  return(SSeffect / SStotal)
  
}
