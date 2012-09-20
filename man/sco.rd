\name{sco}
\alias{sco}
\title{Score a multiple choice test}
\usage{
  sco(choices, key, na.false = FALSE)
}
\arguments{
  \item{choices}{The original responses to the items in the
  test: persons as rows, items as columns. May contain NA.}

  \item{key}{A vector containing the key (correct answers)
  to the items in \code{choices}. If not given, the
  function will check if all data are either 0, 1, or NA:
  if yes, NA are recoded as 0, else an error message is
  returned.}

  \item{na.false}{Recode non-responses to false responses?}
}
\value{
  A matrix of responses scored 0=wrong 1=correct, and
  possibly NA
}
\description{
  Given a key, score a multiple choice test, i.e. recode
  the original choices to right (1) or wrong (0). Missing
  responses are treated as wrong.
}
\examples{
res <- sco(Unscored, key=c(2,3,1,1,4,1,2,1,2,3,3,4,3,4,2,2,4,3))
}
\author{
  Ivailo Partchev
}
\keyword{models}

