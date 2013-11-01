\name{surfaceAICPlot}
\alias{surfaceAICPlot}
\alias{surfaceAICMultiPlot}
\title{
Plot the AIC Throughout a SURFACE Analysis
}
\description{
Plots a line graph showing how the AICc changed over the forward and backward phases of a SURFACE analysis. \code{surfaceAICPlot} can optionally show the change in the deviance or 'partial AICc' for each trait separately as well as for the analysis as a whole. \code{surfaceAICMultiPlot} plots lines from multiple runs on the same plot, allowing comparison among analyses done on alternate tree topologies or with stochasticity added using \code{sample_shifts}
}
\usage{
surfaceAICPlot(fwd = NULL, bwd = NULL, out = NULL, summ = NULL, 
traitplot = "none", cols = NULL, daic = FALSE,  ...)
surfaceAICMultiPlot(fwd = NULL, bwd = NULL, out = NULL, summ = NULL, 
cols = NULL, daic = FALSE,  ...)
}
\arguments{
  \item{fwd}{
List resulting from a \code{surfaceForward} run, or a list of such lists if calling \code{surfaceAICMultiPlot}
}
  \item{bwd}{
List resulting from a \code{surfaceBackward} run, or a list of such lists if calling \code{surfaceAICMultiPlot}
}
  \item{out}{
List resulting from a \code{runSurface} run, consisting of elements \code{fwd} and \code{bwd}, or a list of such lists if calling \code{surfaceAICMultiPlot}
}
  \item{summ}{
Object returned by \code{surfaceSummary} (run on the forward and backward phases of an analysis together), or a list of such objects if calling \code{surfaceAICMultiPlot}
}
  \item{traitplot}{
String indicating what values to use to draw lines corresponding to individual traits: \code{"none"}, \code{"dev"} or \code{"aic"} (see details); defaults to \code{"none"}  
}
  \item{cols}{
An optional character vector of colors for the AICc lines, used to color the different runs in \code{surfaceAICMultiPlot}. Only used in \code{surfaceAICPlot} if \code{traitplot = "aic"} or \code{traitplot = "dev"}, in which case the colors are used for the trait lines (the overall AICc line is drawn in black)
}
  \item{daic}{
A logical indicating whether to rescale all delta-AICc (and delta-deviance) values to the value from the starting model; defaults to \code{FALSE}, but is automatically set to \code{TRUE} if \code{traitplot = "aic"} or \code{traitplot = "dev"}
}
  \item{\dots}{
Additional arguments to be passed to the \code{plot} or \code{points} functions
}
}
\details{
If values are plotted on a trait-by-trait basis, either \code{traitplot="dev"} or \code{traitplot="aic"} can be specified. If \code{traitplot="dev"}, the deviance (-2*log likelihood) at each step is shown for each trait. If \code{traitplot="aic"}, a "partial AICc" at each step is shown for each of the \code{m} traits, consisting of the deviance and 1/m of the "penalty" part of the overall AICc, where m is the number of traits. Note that this is not a proper statistical construct, but its property of adding to give the overall AICc can be useful in visualizing the patterns among traits
}
\value{
Plots AIC values from a SURFACE analysis on the current graphics device
}
\references{
Ingram, T. & Mahler, D.L. (2013) SURFACE: detecting convergent evolution from comparative data by fitting Ornstein-Uhlenbeck models with stepwise AIC. Methods in Ecology and Evolution 4: 416-425. 

Mahler, D.L., Ingram, T., Revell, L.J. & Losos, J.B. (2013) Exceptional convergence on the macroevolutionary landscape in island lizard radiations. Science 341: 292-295.
}
\author{
Travis Ingram
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{surfaceForward}}, \code{\link{surfaceBackward}}, \code{\link{surfaceSimulate}}, \code{\link{surfaceSummary}}, \code{\link{surfaceTreePlot}}, \code{\link{surfaceTraitPlot}}
}

\keyword{hplot}
