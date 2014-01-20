\name{plot.irf}
\alias{plot.irf}
\title{A plot method for item response functions}
\usage{
  \method{plot}{irf} (x, add = FALSE,
    main = "Item response function", co = 1, label = FALSE,
    ...)
}
\arguments{
  \item{x}{An object produced by function \code{irf}}

  \item{add}{When \code{add=T}, the IRF is added to a plot,
  otherwise a new plot is started. Default is F.}

  \item{main}{The main title of the plot, given that
  \code{add=F}.}

  \item{co}{The colour of the IRF curve. Default is 1 for
  black. Use \code{co=NA} to plot each IRF in a different
  colour.}

  \item{label}{When \code{label=T}, individual curves will
  be labeled with the item number.}

  \item{...}{Any additional plotting parameters}
}
\description{
  Useful for plotting item response functions. The \code{x}
  argument of \code{irf} should better be left out unless
  something special is required.
}
\examples{
# plot IRF for all items in red, label with item number
plot(irf(Scored2pl$est), co="red", label=TRUE)
# plot IRF for items 2, 3, and 7 in different colours
plot(irf(Scored2pl$est[c(2,3,7),]), co=NA)
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{irf}}
}
\keyword{models}

