\name{surfaceSummary}
\alias{surfaceSummary}
\title{
Summarize SURFACE output
}
\description{
Summarizes the most important features of a SURFACE analysis, extracting summary data from the list returned by \code{surfaceForward} or \code{surfaceBackward}
}
\usage{
surfaceSummary(obj)
}
\arguments{
  \item{obj}{
A list returned by \code{surfaceForward} or \code{surfaceBackward} (which may be stored at the \code{$fwd} or \code{$bwd} component of the list returned by \code{runSurface})
}
}
\value{
A list with the following components:
  \item{n_steps }{number of iterations in the stepwise analysis}
  \item{aics }{vector of AICc values at each step}
  \item{shifts }{shifts present in the final fitted Hansen model}
  \item{n_regimes }{summary of regime structure of the final fitted Hansen model (see note below)}
  \item{alpha }{estimate of alpha for each trait}
  \item{sigma_squared }{estimate of sigma_squared for each trait}
  \item{theta }{matrix of estimated optima (one per regime per trait)}
}
\references{
Ingram, T. & Mahler, D.L. in revision, Methods in Ecology and Evolution. 
}
\author{
Travis Ingram
}
\note{
The element \code{n_regimes} contains the most direct measures of the regime structure returned by a SURFACE analysis. This vector consists of \code{k} (the number of regime shifts, counting the basal regime as 1), \code{kprime}, (the number of regimes, some of which may be reached by multiple shifts), \code{deltak} (\code{k-kprime}, a measure of convergence), \code{c} (the number of shifts to convergent regimes, another measure of convergence), \code{kprime_conv} (the number of convergent regimes shifted to multiple times), and \code{kprime_nonconv} (the number of nonconvergent regimes only shifted to once)
}


\seealso{
\code{\link{surfaceForward}}, \code{\link{surfaceBackward}}
}
\keyword{manip}
