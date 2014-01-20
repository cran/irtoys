\name{irf}
\alias{irf}
\title{Item response function}
\usage{
  irf(ip, x = NULL)
}
\arguments{
  \item{ip}{Item parameters: a matrix with one row per
  item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}.}

  \item{x}{The values of the latent variable (\eqn{\theta}
  in the equation above), at which the IRF will be
  evaluated. If not given, 99 values spaced evenly between
  -4 and +4 will be used, handy for plotting.}
}
\value{
  A list of: \item{x}{A copy of the argument \code{x}}
  \item{f}{A matrix containing the IRF values: persons
  (values of (\code{x}) as rows and items as columns}
}
\description{
  Returns the item response function of the 3PL (1PL, 2PL)
  model, the i.e. the probabilities defined by
  \deqn{P(U_{ij}=1|\theta_i,a_j,b_j,c_j)=c_j+(1-c_j)\frac{\displaystyle\exp(a_j(\theta_i-b_j))}{1+\displaystyle\exp(a_j(\theta_i-b_j))}}
  where \eqn{U_{ij}} is a binary response given by person
  \eqn{i} to item \eqn{j}, \eqn{\theta_i} is the value of
  the latent variable ("ability") for person \eqn{i},
  \eqn{a_j} is the discrimination parameter for item
  \eqn{j}, \eqn{b_j} is the difficulty parameter for item
  \eqn{j}, \eqn{c_j} is the asymptote for item \eqn{j}.
  Some authors call the IRF "the item characteristic
  curve".
}
\details{
  In the 2PL model (\code{model="2PL"}), all asymptotes
  \eqn{c_j} are 0. In the 1PL model (\code{model="1PL"}),
  all asymptotes \eqn{c_j} are 0 and the discriminations
  \eqn{a_j} are equal for all items (and sometimes to 1).

  A common use of this function would be to obtain a plot
  of the IRF.
}
\examples{
plot(irf(Scored2pl$est[1,]))
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{plot.irf}}
}
\keyword{models}

