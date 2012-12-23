\name{surfaceBackward}
\alias{surfaceBackward}
\alias{collapseRegimes}
\title{
Collapsing Convergent Regimes in a Hansen Model
}
\description{
Carries out the backward phase of SURFACE's stepwise AIC routine. Beginning with a fitted Hansen model produced by \code{surfaceForward}, tests pairwise collapses of regimes and identifies collapses that most improve the fit. Continues this iterative process until the model stops improving beyond the given AIC threshold
}
\usage{
surfaceBackward(otree, odata, starting_model, aic_threshold = 0, max_steps = NULL, save_steps = FALSE, filename = "temp_back_list.R", verbose = FALSE, only_best = FALSE, plotaic = FALSE, error_skip = FALSE, sample_shifts = FALSE, sample_threshold = 2)
collapseRegimes(otree, odata, oldshifts, oldaic, oldfit, aic_threshold = 0, only_best = FALSE, verbose = TRUE, plotaic = TRUE, error_skip = FALSE, sample_shifts = FALSE, sample_threshold = 2)
}
\arguments{
  \item{otree}{
Phylogenetic tree in \code{ouchtree} format
}
  \item{odata}{
Data frame with rownames corresponding to \code{otree@labels}
}
  \item{starting_model}{
The Hansen model to attempt regime collapses on; typically the final element of a \code{surfaceForward} analysis
}
  \item{aic_threshold}{
Positive scalar specifying the magnitude of AICc improvement needed to accept a shift (defaults to zero, meaning any improvement in the AICc will be accepted)
}
  \item{max_steps}{
Maximum number of iterations of the stepwise routine
}
  \item{save_steps}{
A logical indicating whether to save the current iteration of the model at each step (overwriting if necessary) to a file \code{filename} (defaults to \code{FALSE})
}
  \item{filename}{
Name of the file to save progress to at each step, if \code{savesteps=TRUE}
}
  \item{verbose}{
A logical indicating whether to print progress (defaults to \code{FALSE})
}
  \item{only_best}{
A logical indicating whether to only allow one pair of regimes to be collapsed at each iteration; if \code{FALSE}, \code{igraph} functions are used to identify pairs of regimes that can be collapsed to improve the model without any inconsistencies (defaults to \code{FALSE})
}
  \item{plotaic}{
A logical indicating whether to plot AICc values of candidate models at each step (defaults to \code{FALSE})
}
  \item{error_skip}{
A logical indicating whether to skip over any candidate model that produces an error message (this is rare, but can cause an entire analysis to abort; defaults to \code{FALSE}) 
}
  \item{sample_shifts}{
A logical indicating whether to sample from among the best models at each step (those within \code{sample_threshold} of the best AICc), rather than always selecting the best candidate model (defaults to \code{FALSE}; both \code{sample_shifts} and \code{only_best} must be set to \code{TRUE} to use this option during the backward phase
}
  \item{sample_threshold}{
Number of AICc units within which to sample among candidate models that are close to as good as the best model at each step (defaults to 2, but only used if \code{sample_shifts=TRUE} and \code{only_best=TRUE})
}
  \item{oldshifts}{
Shifts present in the previous iteration of the Hansen model
}
  \item{oldaic}{
AICc value for the Hansen model from the previous iteration
}
  \item{oldfit}{
Previous fitted Hansen model
}
}
\details{
Can be time-consuming, as the number of likelihood searches at a step is \code{k(k-1)/2}, where \code{k} is the number of regimes in the model. 
}
\value{
\code{collapseRegime} returns a list corresponding to one iteration of the backward phase of the SURFACE analysis; \code{surfaceBackward} returns a list of such lists consisting of each step of the stepwise process
  \item{fit }{The fitted Hansen model selected for improving the AICc most over the previous iteration; consists of a single \code{hansentree} object if the number of traits \code{m = 1}, or a list of \code{hansentree} objects if \code{m > 1} }
  \item{all_aic }{The AICc for each model tested during the iteration (one per pair of regimes)}
  \item{aic }{The AICc of the current Hansen model}
  \item{savedshifts }{The shifts present in the current Hansen model; represented as a named character vector of regime assignments (lower-case letters), with names indicating branches containing shifts} 
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
\code{\link{surfaceForward}}, \code{\link{surfaceSimulate}}, \code{\link{surfaceTreePlot}}, \code{\link{surfaceSummary}}
}
\keyword{models}
\keyword{iteration}
