\name{convertTreeData}
\alias{convertTreeData}
\alias{convertBack}
\alias{nameNodes}
\title{
Utilities for converting between formats and preparing objects for SURFACE analysis
}
\description{
\code{convertTreeData} converts a \code{phylo} tree and data frame to inputs appropriate for analysis with \code{ouch}. \code{convertBack} converts an \code{ouchtree} to a data frame including regime information. \code{nameNodes} adds node labels to a \code{phylo} tree to ensure easy conversion. 
}
\usage{
convertTreeData(tree, dat)
convertBack(tree, otree, regshifts)
nameNodes(tree)
}
\arguments{
  \item{tree}{
Phylogenetic tree in \code{phylo} format: to ensure reliable conversion, should have node labels (e.g. using \code{nameNodes})
}
  \item{dat}{
Data frame with row names corresponding to the taxa in \code{tree} and columns consisting of one or more trait measurements
}
  \item{otree}{
Phylogenetic tree in \code{ouchtree} format
}
  \item{regshifts}{
Named character vector of regime shifts indicating branches containing shifts (numbered corresponding to \code{otree@nodes}) and regime identities (usually lower-case letters)
}
}
%\details{
%}
\value{
\code{convertTreeData} returns a list with components 
  \item{otree}{phylogenetic tree in \code{ouchtree} format}
  \item{odata}{data frame containing trait data ready for SURFACE analysis (rownames correspond to \code{otree@labels})}
\code{convertBack} returns a data frame containing original phenotypic data as well as regime assignments of tip taxa
\code{nameNodes} returns the input tree, with arbitrary node names added (\code{zzz1}, \code{zzz2}, etc) to ensure reliable conversion between formats
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

\seealso{
\code{\link{surfaceBackward}}, \code{\link{surfaceForward}}
}

\keyword{manip}
