\name{tif}
\alias{tif}
\title{Test information function}
\usage{
  tif(ip, x = NULL)
}
\arguments{
  \item{ip}{Item parameters: a matrix with one row per
  item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}.}

  \item{x}{The values of the latent variable (\eqn{\theta}
  in the equation above), at which the TIF will be
  evaluated. If not given, 99 values spaced evenly between
  -4 and +4 will be used, handy for plotting.}
}
\value{
  A list of: \item{x}{A copy of the argument \code{x}}
  \item{f}{A vector containing the TIF values}
}
\description{
  Returns the test information function (TIF) of the 3PL
  (1PL, 2PL) model. The TIF is the sum of the item
  information functions (IIF) in a test, and indicates the
  precision of measurement that can be achieved with the
  test at any value of the latent variable, bein inversely
  related to measurement variance.
}
\details{
  A common use of this function would be to obtain a plot
  of the TIF.
}
\examples{
plot(trf(Scored2pl$est))
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{plot.tif}}, \code{\link{iif}}
}
\keyword{models}

