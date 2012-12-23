\name{surfaceTreePlot}
\alias{surfaceTreePlot}
\alias{surfaceTraitPlot}
\alias{surfaceAICPlot}
\title{
Plot a the results of a SURFACE analysis
}
\description{
Plotting functions to visualize the results of a SURFACE analysis: \code{surfaceTreePlot} produces a customized \code{plot.phylo} figure; \code{surfaceTraitPlot} produces a scatterplot of species' traits, each with colors representing regimes; and \code{surfaceAICPlot} produces a line plot showing the sequence of AICc values over an entire SURFACE analysis
}
\usage{
surfaceTreePlot(tree, hansenfit, cols = NULL, convcol = TRUE, labelshifts = FALSE, ...)
surfaceTraitPlot(dat, hansenfit, whattraits = c(1, 2), cols = NULL, convcol = TRUE, pchs = c(21,21), cex.opt = 2.5, ...)
surfaceAICPlot(fwd, bwd, ...)
}
\arguments{
  \item{tree}{
Phylogenetic tree in \code{phylo} format
}
  \item{dat}{
Trait data formatted as a data frame with named rows and at least two columns
}
  \item{hansenfit}{
An object containing the fitted Hansen model to use in plotting, with elements \code{fit} and \code{savedshifts}. This may be the list produced by any one iteration of \code{surfaceForward} or \code{surfaceBackward}, or the list produced by \code{surfaceSimulate}
}
  \item{fwd}{
List resulting from a \code{surfaceForward} run
}
  \item{bwd}{
List resulting from a \code{surfaceBackward} run
}
  \item{whattraits}{
A two-element vector indicating which traits to use for the (x,y) axes of a trait plot (defaults to \code{c(1,2)} )
}
  \item{cols}{
An optional character vector of colors for painting branches in \code{surfaceTreePlot} or coloring symbols in \code{surfaceTraitPlot}. One color should be provided per regime in \code{hansenfit}; if \code{cols=NULL} the function will attempt an appropriate default 
}
  \item{convcol}{
A logical indicating whether to select separate colors for convergent (colorful) and non-convergent (greyscale) regimes (defaults to \code{TRUE})
}
  \item{labelshifts}{
A logical indicating whether to add integer labels to branches in the tree to show the order in which regime shifts were added in the forward phase (defaults to \code{FALSE})
}
  \item{pchs}{
Optional two-element vector of plotting characters for trait values and optima in \code{surfaceTraitPlot}; defaults to 21 (filled circles) for both traits and optima 
}
  \item{cex.opt}{
Character expansion for symbols representing the optima in \code{surfaceTraitPlot}; defaults to 2.5 (symbols representing data points can be specified with \code{cex})
}
  \item{\dots}{
Additional arguments to be passed to the \code{plot} or \code{points} functions
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Creates one tree or trait plot on the current graphics device
}
\references{
Ingram, T. & Mahler, D.L. in revision, Methods in Ecology and Evolution. 
}
\author{
Travis Ingram
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{surfaceForward}}, \code{\link{surfaceBackward}}, \code{\link{surfaceSimulate}}, \code{\link{surfaceSummary}}
}

\keyword{hplot}
