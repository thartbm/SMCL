#' Rotation adaptation data for use with the two-rate model
#'
#' A dataset with a perturbation schedule and the reach direction errors of a group
#' of participants doing a typical adaptation schedule that evokes a rebound.
#' The data is stored in a somewhat wide format, atypical for R.
#'
#' @format A data frame with 164 rows and 20 columns:
#' \describe{
#'   \item{block}{blocks 1 through 4: aligned, rotated, reversed, error-clamped}
#'   \item{trial}{trial within the block}
#'   \item{schedule}{the rotation on the trial, with NA for error-clamp trials}
#'   \item{p003 ... p035}{each of the other 17 columns desrcibes the reach 
#'   deviations of one participant at one-third the distance to the target}
#' }
"tworatedata"
