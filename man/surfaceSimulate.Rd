\name{surfaceSimulate}
\alias{surfaceSimulate}
\title{
Simulate Data for SURFACE
}
\description{
Provides several ways to simulate data sets on phylogenetic trees in conjunction with SURFACE analyses. Can simulate under simple models without regime shifts, under a Hansen model with sampled shift locations, or under a fitted Hansen model (optionally with resampled optima)
}
\usage{
surfaceSimulate(phy, type = "BM", param = 0, n_traits = NULL, dat = NULL, vcv = NULL, hansenfit = NULL, shifts = NULL, n_shifts = NULL, n_conv_shifts = NULL, n_regimes = NULL, n_per_regime = NULL, no_nested = TRUE, optima = NULL, sample_optima = TRUE, optima_distrib = NULL, optima_type = "rnorm", sigma_squared = NULL, alpha = NULL, pshift_timefactor = NULL)
}
\arguments{
  \item{phy}{
A phylogenetic tree in \code{phylo} format on which to simulate data
}
  \item{type}{
Type of simulation desired - options are \code{"BM"}, \code{"hansen-fit"}, and \code{"hansen-paint"} (see Details)
}
  \item{param}{
If \code{type="BM"}, an optional parameter to rescale the tree (see Details)
}
  \item{n_traits}{
Number of traits (if not provided will be determined from other inputs or default to 1)
}
  \item{dat}{
Optional data frame of original trait data (function will use this to extract features of the data set)
}
  \item{vcv}{
Optional evolutionary variance-covariance matrix
}
  \item{hansenfit}{
A fitted Hansen model (or a list of such if multiple traits) (if \code{type = "hansen-fit"})
}
  \item{shifts}{
A vector of regime shifts, named for the branches they are to be placed on in the Hansen model to be simulated under (if \code{type = "hansen-paint"}). If specified, \code{n_shifts}, \code{n_conv_shifts}, \code{n_regimes} and \code{n_per_regime} are all ignored
}
  \item{n_shifts}{
Number of shifts to add to the Hansen model (if \code{type = "hansen-paint"})
}
  \item{n_conv_shifts}{
Number of convergent shifts to add to the Hansen model (if \code{type = "hansen-paint"}). Either \code{n_conv_shifts} or \code{n_regimes} can be specified along with \code{n_shifts}, but not both
}
  \item{n_regimes}{
Number of regimes to add to the Hansen model (if \code{type = "hansen-paint"}). Either \code{n_conv_shifts} or \code{n_regimes} can be specified along with \code{n_shifts}, but not both
}
  \item{n_per_regime}{
Integer vector of the number of shifts to each regime in the model (if \code{type = "hansen-paint"}). If specified, the vector length determines \code{n_regimes}, and the sum of the values determines \code{n_shifts}, and the number of entries >1 determines \code{n_conv_shifts}
}
  \item{no_nested}{
A logical indicating whether to ensure that a pair of `convergent' regimes is not in fact two nested clades (if \code{type = "hansen-paint"}; defaults to \code{TRUE})
}
  \item{optima}{
Optional matrix of optima
}
  \item{sample_optima}{
A logical indicating whether to replace the optima in the fitted model with new values from a distribution based on the inferred optima (if \code{type = "hansen-fit"}; defaults to \code{TRUE})
}
  \item{optima_distrib}{
Optional matrix of optima distribution for each trait (see \code{optima_type}). Each column is a two-element vector \code{c(A, B)} for the trait. 
}
  \item{optima_type}{
How to sample optima based on \code{optima_distrib}. Can be one of \code{"rnorm"} (default; distribution is normal with mean=A, sd=B), \code{"runif"} (distribution is uniform with center=A, width=B), or \code{"even"} (optima are evenly spaced with spacing=B, then randomized)
}
  \item{sigma_squared}{
Scalar or vector of Brownian rate parameters to use in simulations
}
  \item{alpha}{
Scalar or vector of OU attraction parameter values to use in simulations
}
  \item{pshift_timefactor}{
Factor by which to bias sampling of branches to place regimes on to be earlier (if <1) or later (if >1) in the tree. The sampling probability will be \code{pshift_timefactor} times higher at the tips than at the root
}
}
\details{
Type of simulation may be \code{"BM"}, \code{"hansen-fit"}, or \code{"hansen-paint"}.

If type = \code{"BM"}, simulation uses the \code{sim.char} function in \code{geiger}, with Brownian rate \code{sigma_squared}. If \code{type = "BM"}, \code{param} values other than 0 will transform the tree based on the Early Burst (\code{param < 0}) or single-peak Ornstein-Uhlenbeck (\code{param > 0}) model before simulating, causing trait disparity to be concentrated earlier or later in the tree, respectively 

If \code{type = "hansen-fit"}, an existing \code{hansentree} object is used as the basis of simulation using \code{ouch} functions, optionally with new parameter values

If \code{type = "hansen-paint"}, a new \code{hansentree} object is produced for simulation using \code{ouch} functions, with specified parameter values and numbers of regimes and/or regime shifts
}
\value{
A list with the following components (most are NULL if \code{type = "BM"}):
  \item{data }{Simulated trait data in a data frame}
  \item{optima }{Matrix of optima for each regime for each trait in the generating model}
  \item{savedshifts }{Shift locations in the generating Hansen model}
  \item{regimes }{Regime assignments of tip taxa}
  \item{shifttimes }{Timing of each shift in the Hansen model (measured from the root of the tree}
  \item{fit }{Generating Hansen model used in the simulation}
}
\references{
Ingram, T. & Mahler, D.L. (2013) SURFACE: detecting convergent evolution from comparative data by fitting Ornstein-Uhlenbeck models with stepwise AIC. Methods in Ecology and Evolution, in press. 
}
\author{
Travis Ingram
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{surfaceForward}}, \code{\link{surfaceBackward}}, \code{\link{surfaceTreePlot}}, \code{\link{surfaceTraitPlot}}
}

\keyword{datagen}
