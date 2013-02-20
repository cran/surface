\name{convertTreeData}
\alias{convertTreeData}
\alias{convertBack}
\alias{nameNodes}
\title{
Utilities for Formatting Objects for SURFACE Analysis
}
\description{
\code{convertTreeData} converts a \code{phylo}-formatted tree and a data frame into formats ready to be analyzed with the \code{ouch} functions called by \code{surface}. \code{convertBack} converts an \code{ouchtree} to a data frame including regime information. \code{nameNodes} adds node labels to a \code{phylo} tree to ensure reliable conversion between formats 
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
\code{convertTreeData} returns a list with components \code{otree} (a phylogenetic tree in \code{ouchtree} format) and \code{odata} (a data frame containing trait data, with rownames corresponding to \code{otree@labels}). \code{convertBack} returns a data frame containing original phenotypic data as well as regime assignments of tip taxa. \code{nameNodes} returns the input tree, with arbitrary node names added (\code{zzz1}, \code{zzz2}, etc) to ensure reliable conversion between formats
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
\code{\link{surfaceBackward}}, \code{\link{surfaceForward}}
}

\keyword{manip}
