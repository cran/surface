\name{startingModel}
\alias{startingModel}
\title{
Create an initial model to use in SURFACE analysis
}
\description{
Allows the user to force some number of regime shifts to be in the model (e.g. if there is strong a priori reason to include them, or to evaluate how their inclusion changest the result of SURFACE analysis). 
}
\usage{
startingModel(otree, odata, shifts = NULL, brownian = FALSE)
}
\arguments{
  \item{otree}{
Phylogenetic tree in \code{ouchtree} format
}
  \item{odata}{
Data frame with rownames corresponding to \code{otree@labels}
}
  \item{shifts}{
A named character vector of regime shifts. Names should correspond to \code{otree@nodes}, and regime assignments can be any character other than \code{"a"} (see details). Defaults to \code{NULL}, in which case a single-regime OU model is returned. 
}
  \item{brownian}{
A logical indicating whether to return the fitted Brownian motion model for the data set by calling the \code{ouch} function \code{brown} and obtaining AICs by adding log-likelihoods across traits. If TRUE, overrides any specified \code{shifts}
}
}
\details{
For most analysis, this function is not accessed by the user, but is called from within \code{surfaceForward} to initialize the run with a single-regime OU model. If \code{shifts} are supplied, they are always modified so that the first element codes a basal regime 'shift' \code{c("1"="a")}. Thus, if any other element in \code{shifts} is specified as regime \code{"a"}, or has name \code{"1"}, an error will be returned. 
\code{startingModel} can also be used to obtain a fit (with AICc calculated after adding log-likelihoods across traits) for any hypothesized Hansen model or for Brownian motion (if \code{brownian=TRUE}) for straightforward comparison with the model returned by SURFACE
}
\value{
A list of length 1 containing an object with the same structure as the lists returned by each iteration of \code{surfaceForward} and \code{surfaceBackward} (containing elements \code{fit}, \code{all_aic}, \code{aic}, \code{savedshifts}, and \code{n_regimes}). This allows it to be supplied as argument \code{starting_list} in a call to \code{surfaceForward}. 
}
\references{
Ingram, T. & Mahler, D.L. in revision, Methods in Ecology and Evolution. 
}
\author{
Travis Ingram
}

\seealso{
\code{\link{surfaceForward}}
}

\keyword{manip}
