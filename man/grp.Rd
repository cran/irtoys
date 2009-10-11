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
grp(theta, bins=9, breaks=NULL, equal="count", type="meds")
}
\arguments{
  \item{theta}{The latent variable estimates to be binned}
  \item{bins}{Desired number of bins}
  \item{breaks}{A vector of cutpoints. Overrides \code{bins} if present.}
  \item{equal}{Either \code{"width"} for bins of equal width, or \code{"count"} for bins with roughly counts of observations. Default is \code{"quant"}}
  \item{type}{The points at which \code{itf} will evaluate the IRF. One of \code{"mids"} (the mid-point of each bin), \code{"meds"} (the median of the values in the bin), or \code{"means"} (the mean of the values in the bin). Default is \code{"meds"}.}
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
gr     <- grp(th.mle, bins=7)
}
\keyword{models}
