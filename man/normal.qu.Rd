\name{normal.qu}
\alias{normal.qu}
\title{Normal quadrature points and weights}
\usage{
  normal.qu(n = 15, lower = -4, upper = 4, mu = 0,
    sigma = 1, scaling = "points")
}
\arguments{
  \item{n}{Number of quadrature points}

  \item{lower}{Lower boundary}

  \item{upper}{Upper boundary}

  \item{mu}{Mean}

  \item{sigma}{Standard deviation}

  \item{scaling}{Determines the way in which non-default
  values of \code{mu} and \code{sigma} are applied. When
  \code{scaling="points"}, the quadrature points are
  rescaled, otherwise the quadrature weights are adapted.}
}
\value{
  A list of: \item{quad.points}{A vector of \code{n}
  quadrature points} \item{quad.weights}{A vector of the
  corresponding quadrature weights}
}
\description{
  Quadrature points and weights based on the Normal
  distribution. Quadrature objects are used when estimating
  abilities with \code{eap} and for some of the scaling
  methods in \code{sca}.
}
\examples{
quad <- normal.qu(n=20)
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{read.qu.icl}}, \code{\link{eap}},
  \code{\link{sca}}
}
\keyword{models}

