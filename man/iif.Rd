\name{iif}
\alias{iif}
\title{Item information function}
\usage{
  iif(ip, x = NULL)
}
\arguments{
  \item{ip}{Item parameters: a matrix with one row per
  item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}.}

  \item{x}{The values of the latent variable (\eqn{\theta}
  in the equation above), at which the IIF will be
  evaluated. If not given, 99 values spaced evenly between
  -4 and +4 will be used, handy for plotting.}
}
\value{
  A list of: \item{x}{A copy of the argument \code{x}}
  \item{f}{A matrix containing the IIF values: persons
  (values of (\code{x}) as rows and items as columns}
}
\description{
  The item information function (IIF) for the 3PL model can
  be computed as \deqn{I(\theta) =
  a^2\frac{Q(\theta)}{P(\theta)}\left[\frac{P(\theta)-c}{1-c}\right]^2,}
  where \eqn{\theta} is the value of the latent variable
  for a person, \eqn{a} is the discrimination parameter for
  the item, \eqn{P} is the IRF for the person and item, and
  \eqn{Q=1-P}. For the 1PL and 2PL models, the expression
  reduces to \eqn{a^2PQ}.
}
\details{
  A common use of this function would be to obtain a plot
  of the IIF.
}
\examples{
plot(iif(Scored2pl$est[1:3,]))
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{plot.iif}}, \code{\link{irf}}
}
\keyword{models}

