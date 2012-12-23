\name{repaint}
\alias{repaint}
\title{
Paint the Branches of a Tree
}
\description{
A wrapper to the \code{paint} function in \code{ouch} to ensure that regime paintings are automatically formatted for SURFACE analysis (painting the stem branch of a clade and ensuring that the root is assigned a regime)
}
\usage{
repaint(otree, regshifts, stem = TRUE)
}
\arguments{
  \item{otree}{
Phylogenetic tree in \code{ouchtree} format
}
  \item{regshifts}{
Named character vector of regime shifts
}
  \item{stem}{
A logical indicating whether the painting of a clade should include the stem branch (recommended; defaults to \code{TRUE})
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A named character vector of regime assignments for each branch, as returned by \code{paint}
}
\references{
Butler, M.A. & King, A.A. (2004) Phylogenetic comparative analysis: a modeling approach for adaptive evolution. American Naturalist 164: 683-695.

Ingram, T. & Mahler, D.L. in revision, Methods in Ecology and Evolution. 
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