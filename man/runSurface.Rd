\name{runSurface}
\alias{runSurface}
\title{
Carry out a full SURFACE analysis
}
\description{
Carries out both the forward and backward phases of SURFACE's stepwise AIC routine, with sensible default behaviors. 
}
\usage{
runSurface(tree, dat, exclude = 0, aic_threshold = 0, max_steps = NULL, verbose = FALSE, plotaic = FALSE, error_skip = FALSE, only_best = FALSE, sample_shifts=FALSE, sample_threshold = 2)
}
\arguments{
  \item{tree}{
Phylogenetic tree in \code{phylo} format
}
  \item{dat}{
Data frame with taxa names as rownames matching tree, and one or more columns of trait data
}
  \item{exclude}{
Optionally, the proportion of the worst models (AICc scores for each shift point) to exclude in the current round of the forward phase (defaults to zero)
}
  \item{aic_threshold}{
Positive scalar specifying the magnitude of AICc improvement needed to accept a shift (defaults to zero, meaning any improvement in the AICc will be accepted)
}
  \item{max_steps}{
Maximum number of regimes to allow to be added in the forward phase (assuming the model improvement continues to exceed \code{aic_threshold}
}
  \item{verbose}{
A logical indicating whether to print progress (defaults to \code{FALSE})
}
  \item{plotaic}{
A logical indicating whether to plot AICc values of candidate models at each step (defaults to \code{FALSE})
}
  \item{only_best}{
A logical indicating whether to only allow one pair of regimes to be collapsed at each iteration; if \code{FALSE}, \code{igraph} functions are used to identify pairs of regimes that can be collapsed to improve the model without any inconsistencies (defaults to \code{FALSE})
}
  \item{error_skip}{
A logical indicating whether to skip over any candidate model that produces an error message (this is rare, but can cause an entire analysis to abort; defaults to \code{FALSE}) 
}
  \item{sample_shifts}{
A logical indicating whether to randomly sample from among the best models at each step (those within \code{sample_threshold} of the best AICc), rather than deterministically selecting the best candidate model (defaults to \code{FALSE}
}
  \item{sample_threshold}{
Number of AICc units within which to sample among candidate models that are close to as good as the best model at each step (defaults to 2, but only used if \code{sample_shifts=TRUE}, and only used in the backward phase if \code{only_best=TRUE})
}

}
\details{
Attempts to do all steps of SURFACE, including converting data structures and running both forward and backward phases of the analysis. Default behavior should be appropriate in most circumstances, but many functionalities require using the functions \code{surfaceForward} and \code{surfaceBackward} that are called within \code{runSurface}
}
\value{
A list with two elements, \code{fwd} and \code{bwd}. 
  \item{fwd }{The results of the forward phase, as returned by \code{surfaceForward} }
  \item{bwd }{The results of the backward phase, as returned by \code{surfaceBackward} }
}
\references{
Butler, M.A. & King, A.A. (2004) Phylogenetic comparative analysis: a modeling approach for adaptive evolution. American Naturalist 164: 683-695.

Ingram, T. & Mahler, D.L. in revision, Methods in Ecology and Evolution. 
}
\author{
Travis Ingram
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{surfaceBackward}}, \code{\link{surfaceForward}}
}

\keyword{models}
\keyword{iteration}