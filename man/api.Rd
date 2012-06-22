\name{api}
\alias{api}
\title{The Z3 appropriateness index}
\usage{
api(resp, ip)
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
  A vector of length equal to the number of rows in
  \code{resp}, containing the appropriateness indices
}
\description{
  Computes the Z3 appropriateness index, a measure of
  person fit in IRT models
}
\examples{
p.2pl <- est(Scored, model = "2PL", engine = "ltm")
api(Scored, p.2pl)
}
\author{
  Ivailo Partchev
}
\references{
  Drasgow, F., Levine, M. V., & Williams, E. A. (1985).
  Appropriateness measurement with polychotomous item
  response models and standardized indices. British Journal
  of Mathematical and Statistical Psychology, 38, 67--80
}
\keyword{models}
