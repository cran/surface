\name{surfaceForward}
\alias{surfaceForward}
\alias{addRegime}
\title{
Adding Regimes to a Hansen Model
}
\description{
Carries out the forward phase of SURFACE's stepwise AIC routine to add regime shifts to a Hansen model. \code{addRegime} performs one step of this analysis, and is called by \code{surfaceForward}. At each step, tests the delta-AICc of each possible shift placement (i.e. branch), and returns an updated Hansen model with a shift added. Continues this iterative process until the model stops improving beyond a given threshold.
}
\usage{
surfaceForward(otree, odata, starting_list=NULL, starting_shifts=NULL, exclude=0,  aic_threshold=0, max_steps=NULL, save_steps=FALSE, filename="temp_out_list.R", verbose=FALSE, plotaic=FALSE, error_skip=FALSE, sample_shifts=FALSE, sample_threshold=2)
addRegime(otree, odata, oldshifts, oldaic, oldfit, alloldaic=NULL, exclude=NULL, aic_threshold=0, verbose=FALSE, plotaic=FALSE, error_skip=FALSE, sample_shifts=FALSE, sample_threshold=2)
}
\arguments{
  \item{otree}{
Phylogenetic tree in \code{ouchtree} format
}
  \item{odata}{
Data frame with rownames corresponding to \code{otree@labels}
}
  \item{starting_list}{
An optional list which may containing either a partially completed analysis (which can be built upon instead of starting over), or a custom starting model that includes some pre-specified shifts, which can be created using \code{startingModel}
}
  \item{starting_shifts}{
An optional named character vector of shifts that are required to be in the Hansen model, which will be passed to \code{startingModel} when the initial model is built 
}
  \item{exclude}{
Optionally, the proportion of the worst models (AICc scores for each shift point) to exclude in the current round (defaults to zero)
}
  \item{aic_threshold}{
Positive scalar specifying the magnitude of AICc improvement needed to accept a shift (defaults to zero, meaning any improvement in the AICc will be accepted)
}
  \item{max_steps}{
Maximum number of regimes to allow to be added (assuming the model improvement continues to exceed \code{aic_threshold}
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
  \item{plotaic}{
A logical indicating whether to plot AICc values of candidate models at each step (defaults to \code{FALSE})
}
  \item{error_skip}{
A logical indicating whether to skip over any candidate model that produces an error message (this is rare, but can cause an entire analysis to abort; defaults to \code{FALSE}) 
}
  \item{sample_shifts}{
A logical indicating whether to sample from among the best models at each step (those within \code{sample_threshold} of the best AICc), rather than always selecting the best candidate model (defaults to \code{FALSE})
}
  \item{sample_threshold}{
Number of AICc units within which to sample among candidate models that are close to as good as the best model at each step (defaults to 2, but only used if \code{sample_shifts=TRUE})
}
  \item{oldshifts}{
Any shifts present in the previous iteration of the Hansen model
}
  \item{oldaic}{
AICc value for the Hansen model from the previous iteration
}
  \item{oldfit}{
Previous fitted Hansen model
}
  \item{alloldaic}{
AICc values for each tested shift point in the previous iteration
}
}
\details{
Can be time-consuming, as many likelihood searches are carried out at each iteration. Depending on the number of traits and taxa and the number of regimes that are fitted, \code{surfaceForward} can take anywhere from minutes to many hours (only tree sizes up to 128 taxa have been tested). Options to manage computation time include adding regimes one at a time with \code{addRegime} or using \code{max_steps} to perform the analysis several iterations at a time
}
\value{
\code{addRegime} returns a list describing one iteration of the forward phase of the SURFACE analysis; \code{surfaceForward} returns a list of such lists consisting of each step of the stepwise process
  \item{fit }{The fitted Hansen model selected for improving the AICc most over the previous iteration; consists of a single \code{hansentree} object if the number of traits \code{m = 1}, or a list of \code{hansentree} objects if \code{m > 1} }
  \item{all_aic }{The AICc for each model tested during the iteration (numbered by branch)}
  \item{aic }{The AICc of the current Hansen model}
  \item{savedshifts }{The shifts present in the current Hansen model; represented as a named character vector of regime shifts (lower-case letters), with names indicating branches containing shifts} 
  \item{n_regimes }{A two-element vector of the number of regime shifts and the number of distinct regimes in the current model} 
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
\code{\link{surfaceBackward}}, \code{\link{surfaceSimulate}}, \code{\link{surfaceTreePlot}}, \code{\link{surfaceSummary}}, \code{\link{convertTreeData}}, \code{\link{startingModel}}
}

\keyword{models}
\keyword{iteration}