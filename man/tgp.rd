\name{tgp}
\alias{tgp}
\title{Non-parametric option curves}
\usage{
  tgp(choices, key, item,
    main = "Non-parametric response function", co = 1,
    label = FALSE)
}
\arguments{
  \item{choices}{A matrix of responses to multiple-choice
  items: persons as rows, items as columns. As a rare
  exception in \code{irtoys}, responses must not be recoded
  to 0/1, and there may be missing responses.}

  \item{key}{A vector containing the key (correct answers)
  to the items in \code{choices}.}

  \item{item}{A single number pointing to the item (column
  of \code{choices}) to plot.}

  \item{main}{The main title of the plot, given that
  \code{add=F}.}

  \item{co}{The colour of the curves. Default is 1 for
  black. Use \code{co=NA} to plot each curve in a different
  colour.}

  \item{label}{When \code{label=T}, individual curves will
  be labeled with the item number.}
}
\description{
  A plotting function producing non-parametric analogues of
  the IRF for each option in a multiple choice item not
  unlike those in Jim Ramsay's TestGraf program.
}
\examples{
key=c(2,3,1,1,4,1,2,1,2,3,3,4,3,4,2,2,4,3)
tgp(choices=Unscored, key=key, item=4, co=NA, label=TRUE)
}
\author{
  Ivailo Partchev
}
\references{
  James O. Ramsay (2000). TestGraf: A program for the
  graphical analysis of multiple choice test and
  questionnaire data. McGill University, Montreal, Canada
}
\seealso{
  \code{\link{qrs}}, \code{\link{irf}},
  \code{\link{plot.irf}}
}
\keyword{models}

