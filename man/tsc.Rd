\name{tsc}
\alias{tsc}
\title{True scores with standard errors}
\description{
Computes the IRT true scores (test response function at the estimated ability) and an estimate of their 
standard error via the delte theorem, treating item parameters as known).
}
\usage{
tsc(ip, theta)
}
\arguments{
  \item{ip}{Item parameters: a matrix with one row per item, and three columns: [,1] item
  discrimination \eqn{a}, [,2] item difficulty \eqn{b}, and
  [,3] asymptote \eqn{c}. }
  \item{theta}{An object containing ability estimates, as output by function \code{mlebme} or \code{eap}}
}
\value{A matrix with the true scores in column 1, and their standard errors of measurement (SEM) in column 2}
%\references{}
\author{Ivailo Partchev}
\seealso{\code{\link{mlebme}}, \code{\link{eap}}, \code{\link{trf}}}
\examples{
data(Scored)
p.2pl <- est(Scored, model="2PL", engine="ltm")
th <- mlebme(resp=Scored, ip=p.2pl)
tsc(p.2pl, th)
}
\keyword{models}
