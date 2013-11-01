\name{surface-package}
\alias{surface-package}
\alias{surface}
\docType{package}
\title{
Fitting Hansen Models to Investigate Convergent Evolution
}
\description{
\code{surface} provides a wrapper to the \code{ouch} package, fitting a series of Hansen multiple-peak stabilizing selection models using stepwise AIC, and identifying cases of convergence where independent lineages discovered the same adaptive peak
}
\details{
\tabular{ll}{
Package: \tab surface\cr
Type: \tab Package\cr
Version: \tab 0.4\cr
Date: \tab 2013-02-20\cr
License: \tab GPL (>=2)\cr
}
\code{surface} uses the Hansen model of stabilizing selection around multiple adaptive peaks to infer a macroevolutionary adaptive landscape using only trait data and a phylogenetic tree. The most important functions are \code{surfaceForward} and \code{surfaceBackward}, which carry out the two stepwise phases of the method, and \code{runSurface}, a wrapper function that carries out both phases. Results can be displayed using \code{surfaceSummary}, and visualized using \code{surfaceTreePlot}, \code{surfaceTraitPlot}, and \code{surfaceAICPlot}. Hypothesis tests, such as whether the extent of convergence exceeds the expectation under a model without true convergence, can be done with the assistance of \code{surfaceSimulate}. The vignette `surface_tutorial' demonstrates the use of the various functions included in the package 
}
\author{
Travis Ingram <travis.ingram@otago.ac.nz>
}
\references{
Ingram, T. & Mahler, D.L. (2013) SURFACE: detecting convergent evolution from comparative data by fitting Ornstein-Uhlenbeck models with stepwise AIC. Methods in Ecology and Evolution 4: 416-425. 
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the ~~
%~~ R documentation directory ~~
\keyword{ package }
\seealso{
\code{\link{runSurface}}, \code{\link{surfaceForward}}, \code{\link{surfaceBackward}}, \code{\link{surfaceSimulate}}, \code{\link{surfaceSummary}}, \code{\link{surfaceTreePlot}}, \code{\link{surfaceTraitPlot}}, \code{\link{surfaceAICPlot}}
}

\examples{
#executable R code and demonstrations of the key functions can be found in the tutorial
#vignette("surface_tutorial", package = "surface")
}
