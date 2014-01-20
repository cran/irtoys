\name{tet}
\alias{tet}
\title{Approximate tetrachoric correlation matrix}
\usage{
  tet(d)
}
\arguments{
  \item{d}{a matrix (or data frame, which will be converted
  to a matrix) containing only zeroes an ones. NAs are not
  allowed.}
}
\value{
  A matrix of approximate tetrachoric correlations.
}
\description{
  Matrix of tetrachoric correlations using the
  approximation by Bonett and Price (2005).
}
\examples{
tetras <- tet(Scored)
}
\author{
  Ivailo Partchev
}
\references{
  Douglas G. Bonett and Robert M. Price (2005). Inferential
  Methods for the Tetrachoric Correlation Coefficient.
  Journal of Educational and Behavioral Statistics, Vol.
  30, No. 2, pp. 213--225
}
\keyword{models}

