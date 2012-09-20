\name{tia}
\alias{tia}
\title{Elementary test-item analysis}
\usage{
  tia(choices, key, ...)
}
\arguments{
  \item{choices}{The original responses to the items in the
  test: persons as rows, items as columns. May contain NA.}

  \item{key}{A vector containing the key (correct answers)
  to the items in \code{choices}. If not given, the
  function will check if all data are either 0, 1, or NA:
  if yes, NA are recoded as 0, else an error message is
  returned.}

  \item{...}{Other parameters that may be passed to
  \code{sco} or \code{cov}}
}
\value{
  A list with three elements: \describe{ \item{testlevel}{A
  list of statistics at test level (currently, only
  Cronbach's alpha, may be extended in future)}
  \item{itemlevel}{A matrix showing, for each item, the
  proportion of correct responses, the correlation with the
  sum score, and the alpha that the test would have if the
  item were dropped.} \item{optionlevel}{A matrix showing,
  for each possible choice in the multiple-choice item, the
  proportion of responses given, and the correlation with
  the sum score for the test (including the item).  The
  correct response is highlighted with asterisks.} }
}
\description{
  Elementary analysis of the items in a test and the test
  sumscores based on Classical Test Theory.
}
\examples{
itemsum <- tia(Unscored, key=c(2,3,1,1,4,1,2,1,2,3,3,4,3,4,2,2,4,3))
}
\author{
  Ivailo Partchev
}
\keyword{models}

