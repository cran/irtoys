\name{plot.iif}
\alias{plot.iif}
\title{A plot method for item information functions}
\usage{
  \method{plot}{iif} (x, add = FALSE,
    main = "Item information function", co = 1,
    label = FALSE, ...)
}
\arguments{
  \item{x}{An object produced by function \code{iif}}

  \item{add}{When \code{add=T}, the IIF is added to a plot,
  otherwise a new plot is started. Default is F.}

  \item{main}{The main title of the plot, given that
  \code{add=F}.}

  \item{co}{The colour of the IIF curve. Default is 1 for
  black. Use \code{co=NA} to plot each IIF in a different
  colour.}

  \item{label}{When \code{label=T}, individual curves will
  be labeled with the item number.}

  \item{...}{Any additional plotting parameters}
}
\description{
  Useful for plotting item information functions. The
  \code{x} argument of \code{iif} should better be left out
  unless something special is required.
}
\examples{
# plot IIF for all items in red, label with item number
plot(iif(Scored2pl$est), co="red", label=TRUE)
# plot IIF for items 2, 3, and 7 in different colours
plot(iif(Scored2pl$est[c(2,3,7),]), co=NA)
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{iif}}
}
\keyword{models}

