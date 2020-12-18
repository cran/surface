\name{getAIC}
\alias{getAIC}
\alias{npSurface}
\title{
Akaike's Information Criterion for SURFACE Models
}
\description{
Calculates AICc for a Hansen model using combined likelihoods across multiple traits
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
A logical indicating whether to use small-sample size corrected AIC; defaults to \code{TRUE}, and is currently set to \code{TRUE} during all calls within the \code{surface} functions
}
}
\details{
The number of parameters is calculated as p = k + (k' + 2) m, where k is the number of regime shifts, k' is the number of distinct regimes, and m is the number of traits. Note that this differs from many applications of Hansen models, in that SURFACE counts regime shifts as "parameters", modeling the complexity of both the adaptive landscape (number of regimes) and the evolutionary history of the clade (number of regime shifts). For AICc, the sample size is taken to be the total number of trait values mn, where n is the number of taxa
}
\value{
\code{npSurface} returns an integer number of parameters. \code{getAIC} returns a numeric AIC or AICc value
}
\references{
Ingram, T. & Mahler, D.L. (2013) SURFACE: detecting convergent evolution from comparative data by fitting Ornstein-Uhlenbeck models with stepwise AIC. Methods in Ecology and Evolution 4: 416-425. 
}
\author{
Travis Ingram
}
\examples{
data(surfaceDemo)
tree<-surfaceDemo$tree
dat<-surfaceDemo$sim$dat
olist<-convertTreeData(tree,dat)
otree<-olist[[1]]; odata<-olist[[2]]
startmod<-startingModel(otree, odata, shifts = c("6"="b")) 
np<-as.numeric(npSurface(startmod[[1]]))
LnL<-sum(sapply(startmod[[1]]$fit, function(x) summary(x)$loglik))
getAIC(LnL,np,n=ncol(dat)*nrow(dat),AICc=TRUE)
}

\keyword{arith}
