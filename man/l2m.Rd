\name{l2m}
\alias{l2m}
\title{Transform a list of lists... to a matrix}
\usage{
l2m(x)
}
\arguments{
  \item{x}{A list of lists...}
}
\value{
  A matrix with a column for each distinct name in the list
}
\description{
  When using \code{lapply} or \code{sapply} to avoid
  explicit loops, one often ends up with complex structures
  represening lists of lists of lists... Function
  \code{l2m} tries to transform such a structure into a
  matrix with a column for each distinct name in the list.
}
\author{
  Ivailo Partchev
}
\keyword{list}
