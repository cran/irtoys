#' Estimate and plot IRT models for binary responses
#' 
#' \tabular{ll}{
#' Package: \tab irtoys\cr
#' Type: \tab Package\cr
#' Version: \tab 0.2.0\cr
#' Date: \tab 2016-01-30\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' LazyData: \tab yes\cr
#' }
#' 
#' 
#' The \code{irtoys} package contains a bunch of functions potentially useful
#' to those teaching or learning Item Response Theory (IRT). R being 
#' particularly good at graphics, there is a rich array of plotting 
#' functions to visualize IRT models or assess their fit. Ability estimates 
#' can be estimated by MLE, BME, EAP, WLE. Various IRT scaling methods are supported: 
#' MM, MS, Stocking-Lord, and Hebaera. Last not least, \code{irtoys} may be
#' the only package to estimate Haberman's interaction model, although a new and
#' more powerful package is in the making.
#' 
#' Currently, there are several R packages that can estimate the item parameters
#' in various uni- and multidimensional IRT models, but only several years ago one
#' had to use stand-alone programs that had wildly different and
#' often unwieldy user interfaces. Besides, no single program does everything
#' one needs. One of the original purposes of \code{irtoys} was to provide a 
#' simple and unified interface to some of the most basic
#' functions in ICL, BILOG, and R's own \code{ltm}, such that beginners could
#' concentrate on learning IRT rather than syntaxes. Once that these steps 
#' have been made, those wishing to take
#' advantage of the full functionality of ICL, BILOG & Co. must still master
#' the syntax of their program of choice.
#' 
#' To take full advantage of \code{irtoys}, some IRT software is needed.
#' Package \code{ltm} is automatically loaded.  ICL by Brad Hanson can be
#' downloaded from his site, \url{www.b-a-h.com}: executables are provided for
#' Windows, Linux, and Macintosh. Because of the technical problems 
#' occasionally encountered with this site, I have set up an alternative
#' source:
#' 
#' For Linux: \url{https://dl.dropboxusercontent.com/u/31225257/icl_linux.tar.gz}
#' 
#' For Windows: \url{https://dl.dropboxusercontent.com/u/31225257/icl_win.zip}
#' 
#' For Macintosh: \url{https://dl.dropboxusercontent.com/u/31225257/icl_mac.sea.bin}
#' 
#' BILOG is commercial software sold by SSI ---
#' see \url{www.ssicentral.com} for further detail.
#' 
#' On Windows, make sure that the executable files (\code{icl.exe} for ICL,
#' \code{BLM1.EXE}, \code{BLM2.EXE}, and \code{BLM3.EXE} for BILOG) are located
#' in a directory that is included in the PATH variable.  On Linux, BILOG,
#' being a Windows program, is run with \code{wine}, and should also be on a
#' path where wine can find it.  On my machine, I have simply put the three
#' files in \code{~/.wine/drive_c/windows/}. It seems that new versions of wine
#' expect them to be explicitly tagged as executable. On Macintosh, at least
#' \code{ltm} should work in all cases.
#'
#' NOTE: Starting with version 0.2.0, function \code{est} returns a list of three
#' elements: \code{est} contains the parameter estimates and is thus identical
#' to the output in earlier versions, \code{se} contains the standard errors,
#' in a similar format, and \code{vcm} contains the variance-covariance matrices
#' (NULL when using ICL). When passing item parameters to another function that
#' only needs the estimates, \code{irf(ip)} and 
#' \code{irf(ip$est)} can be used interchangeably. This facilitates using simulated
#' item parameters. A function that does require the complete object is \code{}. 
#' 
#' Also, function \code{itf} now returns item fit statistics 
#' as a vector rather than a list. Finally, since most of the functions in \code{irtoys}
#' have been written with the "logistic" metric in mind (i.e., \eqn{a_j(\theta_i-b_j)}
#' rather than \eqn{1.7a^*_j(\theta_i-b_j)}, function \code{est} now estimates item 
#' parameters only in the logistic metric.
#'   
#' @name irtoys-package
#' @aliases irtoys-package irtoys
#' @docType package
#' @author Ivailo Partchev <partchev@@gmail.com>
#' @references S. E. Embretson and S. P. Reise (2000), Item Response Theory for
#' Psychologists, Lawrence Erlbaum Associates, Mahwah, NJ
#' @keywords models
NULL


#' Binary (true/false) responses to a test
#' 
#' Real-life data set containing the responses to a test, scored as true or
#' false.
#' 
#' 
#' @name Scored
#' @docType data
#' @format A data set with 472 persons and 18 items.
#' @keywords datasets
NULL


#' Original, unscored multiple-choice responses to a test
#' 
#' Real-life data set containing the responses to a test, before they have been
#' recoded as true or false. Can be used with only two functions in the
#' package: \code{scoreResponses} and \code{tgf}. All other functions expect binary data,
#' which can be produced with \code{scoreResponses}.
#' 
#' 
#' @name Unscored
#' @docType data
#' @format A data set with 472 persons and 18 items. Each item has 4 possible
#' answers, of which only one is true. There are many NA, which can be treated
#' as wrong responses.
#' @keywords datasets
NULL


#' Example item parameters
#' 
#' Item parameter estimates for the 2PL model, estimated with \code{ltm} from
#' the example data set \code{Scored}. These are provided as a check, and to
#' speed up the examples for the various functions in the package.
#' 
#' 
#' @name Scored2pl
#' @docType data
#' @format The object returned by \code{est}
#' @keywords datasets
NULL

#' Example item parameters 1PL (Bilog)
#' 
#' Item parameter estimates for the 1PL model, estimated with \code{bilog} from
#' the example data set \code{Scored}. These are provided because not users will
#' have BILOG-MG available.
#' 
#' 
#' @name b1
#' @docType data
#' @format The object returned by \code{est}
#' @keywords datasets
NULL

#' Example item parameters 2PL (Bilog)
#' 
#' Item parameter estimates for the 2PL model, estimated with \code{bilog} from
#' the example data set \code{Scored}. These are provided because not users will
#' have BILOG-MG available.
#' 
#' 
#' @name b2
#' @docType data
#' @format The object returned by \code{est}
#' @keywords datasets
NULL

#' Example item parameters 3PL (Bilog)
#' 
#' Item parameter estimates for the 3PL model, estimated with \code{bilog} from
#' the example data set \code{Scored}. These are provided because not users will
#' have BILOG-MG available.
#' 
#' 
#' @name b3
#' @docType data
#' @format The object returned by \code{est}
#' @keywords datasets
NULL



