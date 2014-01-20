\name{eap}
\alias{eap}
\title{EAP estimation of ability}
\usage{
  eap(resp, ip, qu)
}
\arguments{
  \item{resp}{A matrix of responses: persons as rows, items
  as columns, entries are either 0 or 1, no missing data}

  \item{ip}{Item parameters: a matrix with one row per
  item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}.}

  \item{qu}{A quadrature object produced with
  \code{\link{normal.qu}} or read in with
  \code{\link{read.qu.icl}}}
}
\value{
  A matrix with the ability estimates in column 1, and
  their standard errors of measurement (SEM) in column 2,
  and the number of non-missing reponses in column 3
}
\description{
  Estimates the expectation of the posterior distribution
  of the latent variable ("ability") for each person.
}
\examples{
th.eap <- eap(resp=Scored, ip=Scored2pl$est, qu=normal.qu())
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{mlebme}}, \code{\link{normal.qu}},
  \code{\link{read.qu.icl}}
}
\keyword{models}

