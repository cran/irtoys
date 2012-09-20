\name{qrs}
\alias{qrs}
\title{Quantiles of the ranked sum scores}
\usage{
  qrs(resp)
}
\arguments{
  \item{resp}{A matrix of responses: persons as rows, items
  as columns, entries are either 0 or 1, no missing data}
}
\value{
  A one-column matrix of values
}
\description{
  A rough estimate of the values of the latent variable
  ("ability"). The sum scores (number of correct responses)
  are ranked, breaking ties at random. The ranks are
  divided by the sample size + 1, and the corresponding
  quantiles of the standard Normal distribution are
  returned. Used as default in the non-parametric IRF plots
  produced by \code{\link{npp}} in analogy to Jim Ramsay's
  TestGraf. Another possible use is in \code{\link{itf}}.
}
\examples{
sc <- qrs(Scored)
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{npp}}, \code{\link{itf}}
}
\keyword{models}

