\name{plot.tif}
\alias{plot.tif}
\title{A plot method for test information functions}
\usage{
  \method{plot}{tif} (x, add = FALSE,
    main = "Test information function", co = 1, ...)
}
\arguments{
  \item{x}{An object produced by function \code{tif}}

  \item{add}{When \code{add=T}, the TIF is added to a plot,
  otherwise a new plot is started. Default is F.}

  \item{main}{The main title of the plot, given that
  \code{add=F}.}

  \item{co}{The colour of the TIF curve. Default is 1 for
  black. Use \code{co=NA} to plot each TIF in a different
  colour.}

  \item{...}{Any additional plotting parameters}
}
\description{
  Useful for plotting test information functions. The
  \code{x} argument of \code{tif} should better be left out
  unless something special is required.
}
\examples{
plot(tif(Scored2pl$est))
}
\author{
  Ivailo Partchev
}
\seealso{
  \code{\link{tif}}
}
\keyword{models}

