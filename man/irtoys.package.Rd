\name{irtoys-package}
\alias{irtoys-package}
\alias{irtoys}
\docType{package}
\title{Estimate and plot IRT models bor binary responses}
\description{
Provides a common interface to the estimation of item parameters
in IRT models for binary responses with three different programs 
(ICL, BILOG, and \code{ltm}, and a variety of functions useful
with IRT models. 
}
\details{
The \code{irtoys} package contains a bunch of functions 
potentially useful to those teaching or learning Item Response
Theory (IRT). Although there is no shortage of good IRT programs,
those tend to have wildly different and often unwieldy user
interfaces. Besides, no single program does everything one needs.
Item parameters can be estimated with a program like ICL or BILOG,
non-parametric approaches are implemented in TestGraf,
transformation to a common scale needs ST, and so on. Some
programs, such as ICL, have no graphical capabilities at all,
while others offer stunning interactive graphics but refuse to
output a Postscript file. Package \code{irtoys} provides a common
interface to some of the most basic functions in ICL, BILOG, and 
\R's own \code{ltm}, some of the functionality of TestGraf and ST,
and a variety of other functions. Those who want to take advantage
of the full functionality of ICL, BILOG & Co. must still master
their syntax.

To take full advantage of \code{irtoys}, one needs some IRT
software. Package \code{ltm} is automatically loaded.  ICL 
by Brad Hanson can be downloaded from 
his site, \url{www.b-a-h.com}. BILOG is commercial software sold
by SSI --- see \url{www.ssicentral.com} for further detail. 
On Windows, make sure that the executable files (\code{icl.exe} 
for ICL, \code{blm1.exe}, \code{blm2.exe}, and \code{blm3.exe}, 
for BILOG) are located in directories that are included in the 
PATH variable.  
}
\author{Ivailo Partchev <Ivailo.Partchev@uni-jena.de>}
\references{
S. E. Embretson and S. P. Reise (2000),
Item Response Theory for Psychologists,
Lawrence Erlbaum Associates,
Mahwah, NJ
}
\keyword{models}
%\examples{}
