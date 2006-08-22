\name{grp}
\alias{grp}
\title{Bin the latent variable estimates}
\description{
Groups a vector of estimates of the latent variable into a
histogram-like object. Typically invoked by function 
\code{itf}, unless the user wishes to access some of the
non-default settings. 
}
\usage{
grp(theta, bin.type = "quant", bin.numb = 10, ref.type = "meds")
}
\arguments{
  \item{theta}{The latent variable estimates to be binned}
  \item{bin.type}{One of \code{"autom"}, \code{"gauss"}, or \code{"quant"}. \code{"autom"} uses the algorithm of \code{hist()}, producing bins with highly variable frequencies. \code{"gauss"} and \code{"quant"} try to produce groups of roughly equal size. \code{"gauss"} assumes a Normal distribution while \code{"quant"} is based on the empirical quantiles. Default is \code{"quant"}}
  \item{bin.numb}{The number of bins (only a suggestion when \code{bin.type="autom"})}
  \item{ref.type}{The points at which \code{itf} will evaluate the IRF. One of \code{"mids"} (the mid-point of each bin), \code{"meds"} (the median of the values in the bin), or \code{"means"} (the mean of the values in the bin). Default is \code{"meds"}. When \code{"bin.type=autom"}, \code{"meds"} defaults to \code{"mids"}}
}
\value{
A list of:
  \item{breaks}{The breaks between adjacent bins}
  \item{counts}{The number of values in each bin}
  \item{ref}{Depending on \code{ref.type}, the mids of the bin, or the mean or median of the values in the bin}
}
\author{Ivailo Partchev}
\seealso{\code{\link{itf}}}
\examples{
data(Scored)
p.2pl  <- est(Scored, model="2PL", engine="ltm")
th.mle <- mlebme(resp=Scored, ip=p.2pl)
gr     <- grp(th.mle, bin.numb=7)
}
\keyword{models}
