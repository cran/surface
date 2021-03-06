\name{ouchDescendants}
\alias{ouchDescendants}
\title{
Obtain Descendants from an \code{ouch} Tree
}
\description{
Identifies the nodes and tip taxa descended from a given ancestor in an \code{ouchtree} or \code{hansentree} object. Used to test whether two `convergent' regimes are actually nested when randomly placing regime shifts in a Hansen model in the function \code{surfaceSimulate}
}
\usage{
ouchDescendants(node, otree)
}
\arguments{
  \item{node}{
Which node in the ouchtree object to identify the descendants of
}
  \item{otree}{
An \code{ouchtree} object
}
}

\value{
A vector of integers corresponding to the descendents (integers match the \code{@nodes} element of the \code{ouchtree})
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
ouchDescendants(6, otree)
}
\keyword{manip}
