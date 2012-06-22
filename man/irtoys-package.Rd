\docType{package}
\name{irtoys-package}
\alias{irtoys}
\alias{irtoys-package}
\title{Estimate and plot IRT models bor binary responses}
\description{
  \tabular{ll}{ Package: \tab irtoys\cr Type: \tab
  Package\cr Version: \tab 0.1.4\cr Date: \tab
  2011-06-22\cr License: \tab GPL (>= 2)\cr LazyLoad: \tab
  yes\cr LazyData: \tab yes\cr }
}
\details{
  Provides a common interface to the estimation of item
  parameters in IRT models for binary responses with three
  different programs (ICL, BILOG, and \code{ltm}, and a
  variety of functions useful with IRT models.

  The \code{irtoys} package contains a bunch of functions
  potentially useful to those teaching or learning Item
  Response Theory (IRT). Although there is no shortage of
  good IRT programs, those tend to have wildly different
  and often unwieldy user interfaces. Besides, no single
  program does everything one needs. Item parameters can be
  estimated with a program like ICL or BILOG,
  non-parametric approaches are implemented in TestGraf,
  transformation to a common scale needs ST, and so on.
  Some programs, such as ICL, have no graphical
  capabilities at all, while others offer stunning
  interactive graphics but refuse to output a Postscript
  file.

  Package \code{irtoys} provides a common interface to some
  of the most basic functions in ICL, BILOG, and 's own
  \code{ltm}, some of the functionality of TestGraf and ST,
  and a variety of other functions. Those who want to take
  advantage of the full functionality of ICL, BILOG & Co.
  must still master their syntax.

  To take full advantage of \code{irtoys}, some IRT
  software is needed. Package \code{ltm} is automatically
  loaded.  ICL by Brad Hanson can be downloaded from his
  site, \url{www.b-a-h.com}: executables are provided for
  Windows, Linux, and Macintosh. BILOG is commercial
  software sold by SSI --- see \url{www.ssicentral.com} for
  further detail.

  On Windows, make sure that the executable files
  (\code{icl.exe} for ICL, \code{BLM1.EXE},
  \code{BLM2.EXE}, and \code{BLM3.EXE} for BILOG) are
  located in a directory that is included in the PATH
  variable.  On Linux, BILOG, being a Windows program, is
  run with \code{wine}, and should also be on a path where
  wine can find it.  On my machine, I have simply put the
  three files in \code{~/.wine/drive_c/windows/}. It seems
  that new versions of wine expect them to be explicitly
  tagged as executable. On Macintosh, at least \code{ltm}
  should work in all cases.
}
\author{
  Ivailo Partchev <partchev@gmail.com>
}
\references{
  S. E. Embretson and S. P. Reise (2000), Item Response
  Theory for Psychologists, Lawrence Erlbaum Associates,
  Mahwah, NJ
}
\keyword{models}
