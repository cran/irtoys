\name{npp}
\alias{npp}
\title{Non-parametric characteristic curves}
\usage{
  npp(resp, x, items, from = -4, to = 4, co = 1,
    main = "Non-parametric response function", add = FALSE,
    bands = FALSE, label = FALSE)
}
\arguments{
  \item{resp}{A matrix of responses: persons as rows, items
  as columns, entries are either 0 or 1, no missing data}

  \item{x}{The values of the latent variable ("ability")
  for the same persons whose responses are given in
  \code{resp}. If not given, function \code{qrs} will be
  plugged in, which is the approach of TestGraf}

  \item{items}{An index to the items (columns of
  \code{resp}) to be shown on the plot. If not given, all
  items will be plotted.}

  \item{from}{Lower limit for ability on the plot. Default
  is -4.}

  \item{to}{Upper limit for ability on the plot. Default is
  4.}

  \item{add}{When \code{add=T}, the curve is added to a
  plot, otherwise a new plot is started. Default is F.}

  \item{main}{The main title of the plot, given that
  \code{add=F}.}

  \item{co}{The colour of the curves. Default is 1 for
  black. Use \code{co=NA} to plot each curve in a different
  colour.}

  \item{bands}{When \code{bands=T}, confidence bands are
  added.}

  \item{label}{When \code{label=T}, individual curves will
  be labeled with the item number.}
}
\description{
  A plotting routine producing non-parametric analogues of
  the IRF not unlike those in Jim Ramsay's TestGraf
  program.  The curves are produced by a kernel binomial
  regression of the actual responses to an item on some
  estimates of the latent variable, by courtesy of package
  \code{sm}.
}
\examples{
# plot items 1:5 in different colours, label
npp(Scored, items=1:5, co=NA, label=TRUE)

# For item 7, compare npp with the 2PL parametric IRF
npp(Scored, items=7, bands=TRUE)
plot(irf(ip=Scored2pl$est[7,]), co=3, add=TRUE)
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

