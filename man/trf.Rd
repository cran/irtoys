\name{trf}
\alias{trf}
\title{Test response function}
\description{
Returns the test response function (TRF) of the 3PL (1PL, 2PL) model. The
TRF is the sum of the item response functions (IRF) in a test, and 
represents the expected score as a function of the latent variable 
\eqn{\theta}. 

A common use of this function would be to obtain a plot of the TRF.
}
\usage{
trf(ip, x = NULL)
}
\arguments{
  \item{ip}{Item parameters: a matrix with one row per item, and three columns: [,1] item
  discrimination \eqn{a}, [,2] item difficulty \eqn{b}, and
  [,3] asymptote \eqn{c}.}
  \item{x}{The values of the latent variable (\eqn{\theta} in the 
  equation above), at which the IRF will be evaluated. If not given, 99 values
  spaced evenly between -4 and +4 will be used, handy for plotting.}
}
\value{
A list of:
  \item{x}{A copy of the argument \code{x}}
  \item{f}{A vector containing the TRF values}
}
%\references{}
\author{Ivailo Partchev}
\seealso{\code{\link{plot.trf}}, \code{\link{irf}}}
\examples{
data(Scored)
p.2pl  <- est(Scored, model="2PL", engine="ltm")
plot(trf(p.2pl))
}
\keyword{models}