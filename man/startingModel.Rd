\name{startingModel}
\alias{startingModel}
\title{
Create an Initial Model for a SURFACE Analysis
}
\description{
Generate a model to start a SURFACE analysis, or fit specific Hansen or Brownian motion models that can be compared to the models returned by SURFACE
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
For most analysis, this function is not accessed by the user, but is called from within \code{surfaceForward} to initialize the run with a single-regime OU model. However, the user can optionally supply a starting model that imposes some regime shifts  (e.g. if there is strong a priori reason to include them, or to evaluate how their inclusion changes the result of SURFACE analysis). If \code{shifts} are supplied, they are always modified so that the first element codes a basal regime 'shift' \code{c("1"="a")}. Thus, if any other element in \code{shifts} is specified as regime \code{"a"}, or has name \code{"1"}, an error will be returned. \code{startingModel} can also be used to obtain a fit (with AICc calculated after adding log-likelihoods across traits) for any hypothesized Hansen model or for Brownian motion (if \code{brownian=TRUE}) for comparison with models returned by SURFACE
}
\value{
A list of length 1 containing an object with the same structure as the lists returned by each iteration of \code{surfaceForward} and \code{surfaceBackward} (containing elements \code{fit}, \code{all_aic}, \code{aic}, \code{savedshifts}, and \code{n_regimes}). This allows it to be supplied as argument \code{starting_list} in a call to \code{surfaceForward}. 
}
\references{
Ingram, T. & Mahler, D.L. (2013) SURFACE: detecting convergent evolution from comparative data by fitting Ornstein-Uhlenbeck models with stepwise AIC. Methods in Ecology and Evolution 4: 416-425. 
}
\author{
Travis Ingram
}

\seealso{
\code{\link{surfaceForward}}
}

\examples{
data(surfaceDemo)
tree<-surfaceDemo$tree
dat<-surfaceDemo$sim$dat
olist<-convertTreeData(tree,dat)
otree<-olist[[1]]; odata<-olist[[2]]
startmod<-startingModel(otree, odata, shifts = c("6"="b")) 
}
\keyword{manip}
