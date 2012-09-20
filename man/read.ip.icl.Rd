\name{read.ip.icl}
\alias{read.ip.icl}
\title{Read in parameter estimates}
\usage{
  read.ip.icl(file)
}
\arguments{
  \item{file}{File name}
}
\value{
  A list with two elements, \code{est} and \code{se}, for
  the parameter estimates and their standard errors,
  correspondingly. Because ICL does not compute standard
  errors, \code{se} will be NULL. \code{est} is a matrix
  with one row per item, and three columns: [,1] item
  discrimination \eqn{a}, [,2] item difficulty \eqn{b}, and
  [,3] asymptote \eqn{c}. For the 1PL and 2PL models, all
  asymptotes are equal to 0; for the 1PL, the
  discriminations are all equal but not necessarily equal
  to 1.
}
\description{
  From ICL output, read in estimates of item parameters.
  Invoked automatically when the model is estimated via
  \code{irtoys}. If that is not the case, \code{file} must
  be a file produced with the \code{write_item_param file}
  command in ICL.
}
\author{
  Ivailo Partchev
}
\keyword{models}

