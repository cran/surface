\name{getAIC}
\alias{getAIC}
\alias{npSurface}
\title{
Akaike's Information Criterion for SURFACE
}
\description{
Calculates AIC or AICc values, using the number of parameters as defined for Hansen models in SURFACE.
}
\usage{
getAIC(L, np, n, AICc = TRUE)
npSurface(fit)
}
\arguments{
  \item{fit}{
Fitted Hansen model object (the list returned by one iteration of \code{surfaceForward} or \code{surfaceBackward}
}
  \item{L}{
Log-likelihood of the model
}
  \item{np}{
Number of parameters in the model
}
  \item{n}{
Sample size (total number of trait values)
}
  \item{AICc}{
A logical indicating whether to use small-sample size corrected AIC (recommended, defaults to \code{TRUE})
}
}
\details{
Number of parameters is calculated as p = k + (k' + 2) m, where k is the number of regime shifts, k' is the number of distinct regimes, and m is the number of traits. For AICc, the sample size is taken to be the total number of trait values mn, where n is the number of taxa
}
\value{
\code{npSurface} returns an integer number of parameters
\code{getAIC} returns a numeric AIC or AICc value
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

\keyword{arith}
