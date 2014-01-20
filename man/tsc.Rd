\name{tsc}
\alias{tsc}
\title{True scores with standard errors}
\usage{
  tsc(ip, theta)
}
\arguments{
  \item{ip}{Item parameters: a matrix with one row per
  item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}.}

  \item{theta}{An object containing ability estimates, as
  output by function \code{mlebme} or \code{eap}}
}
\value{
  A matrix with the true scores in column 1, and their
  standard errors of measurement (SEM) in column 2
}
\description{
  Computes the IRT true scores (test response function at
  the estimated ability) and an estimate of their standard
  error via the delta theorem, treating item parameters as
  known).
}
\examples{
th <- mlebme(resp=Scored, ip=Scored2pl$est)
tsc(Scored2pl$est, th)
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{mlebme}}, \code{\link{eap}},
  \code{\link{trf}}
}
\keyword{models}

