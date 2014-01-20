\name{mlebme}
\alias{mlebme}
\title{Maximum likelihood and Bayes Modal estimation of ability}
\usage{
  mlebme(resp, ip, mu = 0, sigma = 1, method = "ML")
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

  \item{method}{\code{"ML"} for maximum likelihood or
  \code{"BM"} for Bayes Modal estimation. Default is
  \code{"ML"}, in which case any values for \code{mu} and
  \code{sigma} will be ignored.}
}
\value{
  A matrix with the ability estimates in column 1 and their
  standard errors of measurement (SEM) in column 2, and the
  number of non-missing responses in column 3
}
\description{
  Estimates the value of the latent variable ("ability")
  for each person by direct optimization
}
\examples{
th.mle <- mlebme(resp=Scored, ip=Scored2pl$est)
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{eap}}
}
\keyword{models}

