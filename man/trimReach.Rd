% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reachProcessing.R
\name{trimReach}
\alias{trimReach}
\title{Trim a data frame describing a reach to the interesting part.}
\usage{
trimReach(
  trialdf,
  homeStart = NA,
  targetReached = NA,
  velocity = 0.05,
  firstMove = FALSE,
  holdHome = NA,
  untilHold = NA,
  device = "hand",
  posunit = "pix",
  timeunit = "ms",
  homepos = c(0, 0)
)
}
\arguments{
\item{trialdf}{Data frame representing the reach with variables in columns
(named consistent with settings below) and samples in rows.}

\item{homeStart}{If the participant has to get to the home position before
starting the out-and-back reach, this part could be trimmed. Set `homeStart`
to a numeric value expressing how close the `device` has to be to the home
position, specified in the same unit as `posunit`. The start of the trial
will be trimmed (not returned) up to when the device is that close to the 
start/home position. By default this is NA, so that this part is not 
trimmed. If this is a character variable, starting with "pr" and ending with
numbers, those numbers should indicate a proportion of the home-to-target
distance to use as cutoff value for trimming the first part of the reach.}

\item{targetReached}{If the return movement is represented in the data, this
may have to be trimmed as well. This parameter sets the *distance* at which 
the target is considered reached, and data after this point is trimmed.
The target position should be in columns named "targetx_[posunit]" and 
"targety_[posunit]". This argument is a numeric variable given in the 
position unit specified later on.}

\item{velocity}{Very slow movement is usually not diagnostic. _After_ the
other parts of the data are trimmed, the instantaneous velocity is used as
a cut-off criterion: the first part of the reach that is under the velocity
criterion as a fraction of the maximum velocity in the whole reach, is 
trimmed. And either the final part that is below the velocity criterion is
trimmed, or everything after the first dip below the velocity criterion, 
depending on the `firstMove` parameter. Set to `NA` for no velocity 
criterion. By default this is set conservatively to 0.05.

May give unexpected results if used with `untilHold`.}

\item{firstMove}{Only used if the `velocity` parameter is not `NA`. If set 
to TRUE, the first part of the trajectory up to where it dips below the 
velocity criterion is kept (the rest is trimmed). If FALSE, only the final
part of the trajectory that goes below the velocity criterion is trimmed.

May give unexpected results if used in combination with `untilHold`.}

\item{untilHold}{Not used if set to `NA` (default). Otherwise, this should
be a list with four named entries:

"kind": character setting one of (currently) two ways to determine a hold

"mindist": numeric: minimum distance from home that the hold has to occur 
at, given in position units as set below

"threshold": numeric setting maximum velocity or distance in position and
time units as set below
  
"epoch": numeric duration of the hold in time units specified below

`kind` can be one of "sample-velocity" or "epoch-distance". When it is
"sample-velocity", a sequence of samples spanning the hold epoch all should
have velocity below the threshold value. When it is "epoch-distance" the 
total distance moved during the epoch should be beloc the threshold value.

All data _after_ the hold is trimmed, but the hold itself is not.

May give unexpected results if used in combination with `firstMove` or any
`velocity` criterion.}

\item{device}{The position columns to use are given by "[device]x_[posunit]"
in the `trialdf`, and similar for y. Can be something like 'hand', 'cursor',
'mouse', 'stylus' or 'robot'.}

\item{posunit}{The unit used for the x and y position data. Could be "pix"
or "cm", or whatever is used in the data. Default: "pix".}

\item{timeunit}{The unit used for the time stamps of each sample. The column
names is "time_[timeunit]". Default: "ms"}

\item{homepos}{The coordinates of the home position. Default is (0,0).}
}
\value{
Data frame describing the reach, minus the trimmed parts.
}
\description{
?
}
\details{
?
}
\examples{
?
}
