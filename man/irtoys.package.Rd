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
his site, \url{www.b-a-h.com}. Executables are provided for Windows, 
Linux, and Macintosh. On Windows, place \code{icl.exe} in a directory that is
included in the PATH variable (\code{c:\windows} and the \code{R/bin} 
directory are expected to always work). On Linux, place \code{icl} in the
\code{/usr/bin/} directory, and make it executable. For Macintosh, please ask
somebody who really knows.

BILOG is commercial software sold
by SSI --- see \url{www.ssicentral.com} for further detail. 
On Windows, make sure that the three files \code{BLM1.EXE}, 
\code{BLM2.EXE}, and \code{BLM3.EXE}, 
for BILOG) are located in a directory that is
included in the PATH variable (\code{c:\windows} and the \code{R/bin} 
directory are expected to always work). On Linux, place the three files in the
\code{/usr/bin/} directory, and make sure that \code{wine} is installed. 
For Macintosh, please ask somebody who really knows.
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
