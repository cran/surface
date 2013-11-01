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
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector of branching times
}
\references{
Ingram, T. & Mahler, D.L. (2013) SURFACE: detecting convergent evolution from comparative data by fitting Ornstein-Uhlenbeck models with stepwise AIC. Methods in Ecology and Evolution 4: 416-425. 
}
\author{
Travis Ingram
}
%\note{
%%  ~~further notes~~
%}

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\keyword{manip}
