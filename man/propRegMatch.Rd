\name{propRegMatch}
\alias{propRegMatch}
\title{
Similarity of Two Hansen Models
}
\description{
Calculates the pairwise matching between two alternate paintings of the same phylogenetic tree. This is done by creating a half-matrix for each \code{hansentree} object indicating whether each pairwise comparison of tip species or branches shows they are in the same regime (coded `1') or different regimes (coded `0'). The `proportion matching' value returned is the proportion of elements of the two matrices that are equal; a measure of correspondence between two Hansen models (one of which may be the `true' model if data are simulated)
}
\usage{
propRegMatch(fit1, fit2, internal = FALSE)
}
\arguments{
  \item{fit1}{
First fitted Hansen model; can be the \code{$fit} component of the list returned by either one iteration of an analyis with \code{surfaceForward} or \code{surfaceBackward}, or the list returned by \code{surfaceSimulate} 
}
  \item{fit2}{
Second fitted Hansen model; see \code{fit1}
}
  \item{internal}{
A logical indicating whether internal branches should be included in the calculation of matching in addition to tip taxa; this is only possible if the two trees have identical topology; defaults to FALSE
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A single value quantifying the proportion of pairwise regime comparisons that are the same between the two models
}
\references{
Ingram, T. & Mahler, D.L. (2013) SURFACE: detecting convergent evolution from comparative data by fitting Ornstein-Uhlenbeck models with stepwise AIC. Methods in Ecology and Evolution 4: 416-425. 
}
\author{
Travis Ingram
}


\seealso{
\code{\link{surfaceForward}}, \code{\link{surfaceBackward}}, \code{\link{surfaceSimulate}}
}
\examples{
data(surfaceDemo)
tree<-surfaceDemo$tree
dat<-surfaceDemo$sim$dat
olist<-convertTreeData(tree,dat)
otree<-olist[[1]]; odata<-olist[[2]]
startmod<-startingModel(otree, odata, shifts = c("6"="b")) 
startmod2<-startingModel(otree, odata, shifts = c("6"="b","17"="c")) 
propRegMatch(startmod[[1]]$fit, startmod2[[1]]$fit)
}
\keyword{arith}
