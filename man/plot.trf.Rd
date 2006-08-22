\name{plot.trf}
\alias{plot.trf}
\title{A plot method for test response functions}
\description{
Useful for plotting test response functions. The \code{x} argument of 
\code{trf} should better be left out unless something special is required.
}
\usage{
\method{plot}{trf}(x, add = FALSE, main = "Test response function", co = 1, ...)
}
\arguments{
  \item{x}{An object produced by function \code{trf}}
  \item{add}{When \code{add=T}, the IRF is added to a plot, otherwise a new plot is started. Default is F.}
  \item{main}{The main title of the plot, given that \code{add=F}.}
  \item{co}{The colour of the TRF curve. Default is 1 for black. Use \code{co=NA} to plot each TRF in a different colour.}
  \item{...}{Any additional plotting parameters}
}
\author{Ivailo Partchev}
\seealso{\code{\link{trf}}}
\examples{
data(Scored)
p.2pl  <- est(Scored, model="2PL", engine="ltm")
plot(trf(p.2pl))
}
\keyword{models}% at least one, from doc/KEYWORDS
