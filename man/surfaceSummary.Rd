\name{surfaceSummary}
\alias{surfaceSummary}
\title{
Summarize SURFACE Output
}
\description{
Extracts the most important results from the output of the forward, backward, or both phases of a SURFACE analysis
}
\usage{
surfaceSummary(fwd = NULL, bwd = NULL)
}
\arguments{
  \item{fwd}{
A list returned by \code{surfaceForward} (which may be stored as the \code{$fwd} component of the list returned by \code{runSurface})
}
  \item{bwd}{
A list returned by \code{surfaceBackward} (which may be stored as the \code{$bwd} component of the list returned by \code{runSurface})
}
}
\details{
If both \code{fwd} and \code{bwd} are provided, both phases of the analysis will be summarized together 
}
\value{
A list with the following components:
  \item{n_steps }{number of iterations in the stepwise analysis}
  \item{lnls }{matrix of traits-by-iterations, giving the log-likelihood for each trait at each iteration of the analysis}
  \item{n_regimes_seq }{matrix of the summaries of regime structure at each iteration of the model}
  \item{aics }{vector giving the AICc value at each step}
  \item{shifts }{shifts present in the final fitted Hansen model}
  \item{n_regimes }{summary of regime structure of the final fitted Hansen model (see note below)}
  \item{alpha }{estimate of alpha for each trait in the final model}
  \item{sigma_squared }{estimate of sigma_squared for each trait in the final model}
  \item{theta }{matrix of estimated optima (one per regime per trait) in the final model}
}
\references{
Ingram, T. & Mahler, D.L. (2013) SURFACE: detecting convergent evolution from comparative data by fitting Ornstein-Uhlenbeck models with stepwise AIC. Methods in Ecology and Evolution 4: 416-425. 
}
\author{
Travis Ingram
}
\note{
The elements \code{n_regimes_seq} and \code{n_regimes} contain measures of the regime structure in a SURFACE analysis (for each iteration, and in the final model, respectively). The measures returned are: \code{k} (the number of regime shifts, counting the basal regime as 1), \code{kprime}, (the number of regimes, some of which may be reached by multiple shifts), \code{deltak} (\code{k-kprime}, a measure of convergence), \code{c} (the number of shifts to convergent regimes, another measure of convergence), \code{kprime_conv} (the number of convergent regimes shifted to multiple times), and \code{kprime_nonconv} (the number of nonconvergent regimes only shifted to once)
}


\seealso{
\code{\link{surfaceForward}}, \code{\link{surfaceBackward}}
}
\keyword{manip}
