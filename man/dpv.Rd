\name{dpv}
\alias{dpv}
\title{Draw plausible values}
\usage{
  dpv(resp, ip, mu = 0, sigma = 1, n = 5)
}
\arguments{
  \item{resp}{A matrix of responses: persons as rows, items
  as columns, entries are either 0 or 1, no missing data}

  \item{ip}{Item parameters: a matrix with one row per
  item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}.}

  \item{mu}{Mean of the apriori distribution. Ignored when
  \code{method="ML"}. Default is 0.}

  \item{sigma}{Standard deviation of the apriori
  distribution. Ignored when \code{method="ML"}. Default is
  1.}

  \item{n}{The number of plausible values to draw for each
  person (default is 5).}
}
\value{
  A matrix with \code{n} columns
}
\description{
  Draws (by rejection sampling) plausible values from the
  posterior distribution of ability
}
\examples{
plval <- dpv(resp=Scored, ip=Scored2pl$est)
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{mlebme}}, \code{\link{eap}}
}
\keyword{models}

