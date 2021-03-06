

#' @title MASS ginv: generalized Moore-Penrose pseudo-inverse
#' @param X A matrix.
#' @param tol Tolerance.
#' @return Generalized Moore-Penrose pseudo inverse of matrix X.
#' @description This function is copied from the MASS package (2019-09-15).
#' The only reason to do so is to reduce dependencies.
#' @details Not yet.
#' @examples
#' library('SMCL')
#' M <- matrix(c(1:9),3,3)
#' Mi <- ginv(M)
#' zapsmall( M %*% Mi %*% M ) == zapsmall( M )
#' zapsmall( Mi %*% M %*% Mi ) == zapsmall( Mi )
#' zapsmall( t(M %*% Mi) ) == zapsmall( Mi %*% M )
#' zapsmall( t(Mi %*% M) ) == zapsmall( M %*% Mi )
#' @export
ginv <- function (X, tol = .Machine$double.eps*max(dim(X)) ) 
{
  # tol = sqrt(.Machine$double.eps)
  if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
    stop("'X' must be a numeric or complex matrix")
  if (!is.matrix(X)) 
    X <- as.matrix(X)
  Xsvd <- svd(X)
  if (is.complex(X)) 
    Xsvd$u <- Conj(Xsvd$u)
  Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
  if (all(Positive)) 
    Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
  else if (!any(Positive)) 
    array(0, dim(X)[2L:1L])
  else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
                                               t(Xsvd$u[, Positive, drop = FALSE]))
}