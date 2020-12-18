\name{surfaceTreePlot}
\alias{surfaceTreePlot}
\alias{surfaceTraitPlot}
\title{
Visualize Results of a SURFACE Analysis
}
\description{
Plotting functions to visualize the results of a SURFACE analysis, with colors depicting regime structure: \code{surfaceTreePlot} produces a customized \code{plot.phylo} figure, and \code{surfaceTraitPlot} produces a scatterplot of trait values and optima
}
\usage{
surfaceTreePlot(tree, hansenfit, cols = NULL, convcol = TRUE, labelshifts = FALSE, ...)
surfaceTraitPlot(dat, hansenfit, whattraits = c(1, 2), cols = NULL, 
convcol = TRUE, pchs = c(21, 21), cex.opt = 2.5, optellipses = FALSE, 
ellipsescale = 1, flatten1D = FALSE, add = FALSE, ypos = 0,
 plotoptima = TRUE, plottraits = TRUE, y.lim = NULL, x.lim = NULL, 
y.lab = NULL, x.lab = NULL, ...)
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
  \item{whattraits}{
A two-element integer (or a single integer; see Details) indicating which traits to use for the (x,y) axes of a trait plot (defaults to \code{c(1,2)})
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
Vector with two integers representing the plotting characters to use for trait values and optima, respectively, in \code{surfaceTraitPlot}; both default to 21 (filled circles) 
}
  \item{cex.opt}{
Character expansion for symbols representing the optima in \code{surfaceTraitPlot}; defaults to 2.5 (symbols representing data points can be specified with \code{cex})
}
  \item{optellipses}{
A logical indicating whether to draw ellipses based on the fitted OU model instead of denoting optimum positions with \code{pchs} and \code{cex.opt}. The ellipses are drawn as the optima +/- the standard deviation of the stationary distribution of the inferred OU process: sigma_squared/(2*alpha), multiplied by \code{ellipsescale}
}
  \item{ellipsescale}{
A scalar or vector indicating how many standard deviations to draw ellipses above and below the optima; if a vector, concentric ellipses of various sizes will be drawn; defaults to 1
}
  \item{flatten1D}{
A logical indicating whether all regimes should be placed on a single line when \code{surfaceTraitPlot} is called for a single trait; defaults to \code{FALSE}
}
  \item{add}{
A logical indicating whether to add a new element to an existing \code{surfaceTraitPlot} graph instead of creating a new one; defaults to \code{FALSE} 
}
  \item{ypos}{
Position on the y axis to place the traits and optima on; only applies if a single trait is used and \code{flatten1D = TRUE}
}
  \item{plotoptima}{
A logical indicating whether the optima should be displayed in \code{surfaceTraitPlot}; defaults to \code{TRUE}
}
  \item{plottraits}{
A logical indicating whether the trait values should be displayed in \code{surfaceTraitPlot}; defaults to \code{TRUE}
}
  \item{y.lim}{
Lower and upper limits for the y-axis; by default will be calculated to fit all points and ellipses fit in the frame
}
  \item{x.lim}{
Lower and upper limits for the x-axis; by default will be calculated to fit all points and ellipses fit in the frame
}
  \item{y.lab}{
y-axis label; defaults to the column name in the data frame
}
  \item{x.lab}{
x-axis label; defaults to the column name in the data frame
}
  \item{\dots}{
Additional arguments to be passed to the \code{plot} or \code{points} functions
}
}
\details{
For trait plots using the option \code{optellipses=TRUE}, note that in some cases (e.g. if alpha is very small) the ellipses will not convey useful information. If trait data are unidimensional, or if \code{whattraits} is provided as a single integer, data will be plotted on the x-axis and the y-axis will separate different regimes (and ellipse width in the y-dimension will not be meaningful)
}
\value{
Creates one tree or trait plot on the current graphics device
}
\references{
Ingram, T. & Mahler, D.L. (2013) SURFACE: detecting convergent evolution from comparative data by fitting Ornstein-Uhlenbeck models with stepwise AIC. Methods in Ecology and Evolution 4: 416-425. 

Mahler, D.L., Ingram, T., Revell, L.J. & Losos, J.B. (2013) Exceptional convergence on the macroevolutionary landscape in island lizard radiations. Science 341: 292-295.
}
\author{
Travis Ingram
}

\seealso{
\code{\link{surfaceForward}}, \code{\link{surfaceBackward}}, \code{\link{surfaceSimulate}}, \code{\link{surfaceSummary}}, \code{\link{surfaceAICPlot}}
}
\examples{
data(surfaceDemo)
tree<-surfaceDemo$tree
dat<-surfaceDemo$sim$dat
olist<-convertTreeData(tree,dat)
otree<-olist[[1]]; odata<-olist[[2]]
startmod<-startingModel(otree, odata, shifts = c("6"="b")) 
surfaceTreePlot(tree,startmod[[1]],labelshifts=TRUE,cols=c("black","red"))
surfaceTraitPlot(dat,startmod[[1]],whattraits=c(1,2),cols=c("black","red"))
}
\keyword{hplot}
