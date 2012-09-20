\name{sim}
\alias{sim}
\title{Simulate response data}
\usage{
  sim(ip, x = NULL)
}
\arguments{
  \item{ip}{Item parameters: a matrix with one row per
  item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}.}

  \item{x}{A vector of values of the latent variable
  ("abilities").}
}
\value{
  A matrix of responses: persons as rows, items as columns,
  entries are either 0 or 1, no missing data
}
\description{
  Simulate responses from the 1PL, 2PL, or 3PL model
}
\examples{
pa <- cbind(runif(20,.8,2), runif(20,-2.4,2.4), rep(0,50))
rs <- sim(ip=pa, x=rnorm(1000))
}
\author{
  Ivailo Partchev
}
\keyword{models}

