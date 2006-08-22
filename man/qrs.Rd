\name{qrs}
\alias{qrs}
\title{Quantiles of the ranked sum scores}
\description{
A rough estimate of the values of the latent variable ("ability").
The sum scores (number of correct responses) are ranked, breaking
ties at random. The ranks are divided by the sample size + 1, and
the corresponding quantiles of the standard Normal distribution
are returned. Used as default in the non-parametric IRF plots
produced by \code{\link{npp}} in analogy to Jim Ramsay's TestGraf.
Another possible use is in \code{\link{itf}}.
}
\usage{
qrs(resp)
}
\arguments{
  \item{resp}{A matrix of responses: persons as rows, items as columns, entries are either 0 or 1, no missing data}
}
\value{A one-column matrix of values}
%\references{}
\author{Ivailo Partchev}
\seealso{\code{\link{npp}}, \code{\link{itf}}}
\examples{
data(Scored)
sc <- qrs(Scored)
}
\keyword{models}
