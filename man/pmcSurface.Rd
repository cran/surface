\name{pmcSurface}
\alias{pmcSurface}
\title{
Phylogenetic Monte Carlo for SURFACE Models
}
\description{
Modifies the \code{pmc} routine (Boettiger et al. 2012) for testing statistical significance and estimating power for a comparison of two comparative models 
}
\usage{
pmcSurface(model1, model2, otree, odata, nboot = 20, prob = 0.95)
}
\arguments{
  \item{model1}{
A Hansen model from one step of either the forward or backward phase of SURFACE
}
  \item{model2}{
Another Hansen model, generally from the next step of either the forward or backward phase of SURFACE
}
  \item{otree}{
Phylogenetic tree in \code{ouchtree} format
}
  \item{odata}{
Data frame of trait data with rownames corresponding to \code{otree@labels}
}
  \item{nboot}{
Number of bootstrap simulations to run
}
  \item{prob}{
Quantile to use to establish critical value of delta-LnL distribution (0.95 corresponds to alpha = 0.05 for a one-tailed significance test)
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with the following components:
  \item{bootlist }{long list of each bootstrap model fitting}
  \item{pmc }{a list formatted as a \code{pmc} object with components \code{$null}, \code{$test}, and \code{$lr}, but after adding likelihoods across traits}
  \item{nullL }{vector or data frame of null delta-LnL values, one per trait per simulation}
  \item{testL }{vector or data frame of test delta-LnL values, one per trait per simulation}
  \item{critval }{critical value of delta-LnL (95th percentile of null values) for significance test}
  \item{pval }{p-value from the significance test}
  \item{power }{power to detect the more complex model if true (proportion significant tests)}
}
\references{
Boettiger, C., Coop, G. & Ralph, P. (2012) Is your phylogeny informative? Measuring the power of comparative methods. Evolution 66: 2240-2251.

Ingram, T. & Mahler, D.L. (2013) SURFACE: detecting convergent evolution from comparative data by fitting Ornstein-Uhlenbeck models with stepwise AIC. Methods in Ecology and Evolution, in press. 
}
\author{
Travis Ingram
}
\note{
The only noteworthy difference from the standard \code{pmc} function is that \code{pmcSurface} will combine likelihoods across traits.
}
\seealso{
\code{\link{surfaceForward}}, \code{\link{surfaceBackward}}
}
\keyword{htest}
