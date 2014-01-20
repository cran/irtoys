\name{scp}
\alias{scp}
\title{Plot observed and predicted scores against ability}
\usage{
  scp(resp, ip, theta = NULL)
}
\arguments{
  \item{resp}{A matrix of binary responses to a test, with
  persons as rows and items as columns.}

  \item{ip}{Item parameters: a matrix with one row per
  item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}.}

  \item{theta}{An object containing ability estimates, as
  output by function \code{mlebme} or \code{eap}.  If
  \code{NULL}, MLE will be estimated from \code{resp} and
  \code{ip}.}
}
\value{
  None
}
\description{
  Produces a plot of IRT true scores (test response
  function at the estimated ability) with a confidence band
  (plus/minus standard error). The observed sum scores are
  shown in red.
}
\examples{
scp(Scored, Scored2pl$est)
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{mlebme}}, \code{\link{eap}},
  \code{\link{tsc}}, \code{\link{trf}}
}
\keyword{models}

