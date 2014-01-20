\name{wle}
\alias{wle}
\title{Bias-corrected (Warm's) estimates of ability}
\usage{
  wle(resp, ip)
}
\arguments{
  \item{resp}{A matrix of responses: persons as rows, items
  as columns, entries are either 0 or 1, no missing data}

  \item{ip}{Item parameters: a matrix with one row per
  item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}.}
}
\value{
  A matrix with the ability estimates in column 1, and
  their standard errors of measurement (SEM) in column 2,
  and the number of non-missing reponses in column 3
}
\description{
  Weighted likelihood estimates (WLE) of ability, designed
  to remove the first order bias term from the ML
  estimates. WLE are finite for response patterns
  consisting of either uniformly wrong or uniformly correct
  responses.
}
\examples{
th.bce <- wle(resp=Scored, ip=Scored2pl$est)
}
\author{
  Ivailo Partchev
}
\references{
  Warm T.A. (1989) Weighted Likelihood Estimation of
  Ability in Item Response Theory. Psychometrika, 54,
  427-450.
}
\seealso{
  \code{\link{mlebme}}, \code{\link{eap}}
}
\keyword{models}

