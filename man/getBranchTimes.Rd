\name{getBranchTimes}
\alias{getBranchTimes}
\title{
Extract Branching Times from an \code{ouch} Tree
}
\description{
Extracts the time from root of each node in an \code{ouchtree} or \code{hansentree} formatted phylogenetic tree; used to compute the timing of regime shifts in a Hansen model
}
\usage{
getBranchTimes(h)
}
\arguments{
  \item{h}{
Fitted \code{ouchtree} or \code{hansentree} object
}
}

\value{
A vector of branching times
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
otree<-olist[[1]]
getBranchTimes(otree)
}
\keyword{manip}
