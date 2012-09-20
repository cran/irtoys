\name{read.resp}
\alias{read.resp}
\title{Read responses from a file}
\usage{
  read.resp(file, na = ".")
}
\arguments{
  \item{file}{File name}

  \item{na}{The symbol used to represent missing data}
}
\value{
  A matrix, typically of zeroes and ones, representing the
  correct or wrong responses given by persons (rows) to
  items (columns).
}
\description{
  Reads responses to a questionnaire from a text file
}
\details{
  Included for those who are too faint-hearted to write
  \code{as.matrix(read.table(file, head=F))}. Of course,
  data can be entered into R in many other ways.

  The data values in the \code{file} must be separated with
  blanks.

  Responses are the empirical data used in IRT. Note that
  \code{irtoys} deals with models for dichotomous data, and
  typically expects data consisting of zeroes and ones,
  without any missing values (non-responses are considered
  as wrong responses). In fact, there are only two commands
  in \code{irtoys} that accept other kinds of data:
  \code{sco} and \code{tgp}.

  \code{read.resp} does accept missing data and values
  other than 0 and 1. Use \code{sco} and a key to score
  multiple choice responses to 0/1. If you have dichotomous
  data that contains NAs, you can use \code{sco} without a
  key to change all NA to 0.
}
\examples{
\dontrun{
  r <- read.resp("c:/myfiles/irt.dat")
}
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{sco}}, \code{\link{tgp}},
}
\keyword{IO}

