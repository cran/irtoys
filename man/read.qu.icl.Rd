\name{read.qu.icl}
\alias{read.qu.icl}
\title{Read in quadrature}
\usage{
  read.qu.icl(file)
}
\arguments{
  \item{file}{File name}
}
\value{
  A list of: \item{quad.points}{A vector of quadrature
  points} \item{quad.weights}{A vector of the corresponding
  quadrature weights}
}
\description{
  From ICL output, read in estimates of item parameters.
  \code{file} must be a file produced with the
  \code{write_latent_dist file} command in ICL. Quadrature
  objects are used when estimating abilities with
  \code{eap} and for some of the scaling methods in
  \code{sca}.
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{normal.qu}}, \code{\link{eap}},
  \code{\link{sca}}
}
\keyword{models}

