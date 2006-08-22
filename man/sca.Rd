\name{sca}
\alias{sca}
\title{Linear transformation of the IRT scale}
\description{
Linearly transform a set of IRT parameters to bring them to the 
scale of another set of parameters. Four methods are implemented:
Mean/Mean, Mean/Sigma, Lord-Stocking, and Haebara.  
}
\usage{
sca(old.ip, new.ip, old.items, new.items, 
  old.qu = NULL, new.qu = NULL, method = "MS")
}
\arguments{
  \item{old.ip}{A set of parameters that are already on the desired scale}
  \item{new.ip}{A set of parameters that must be placed on the same scale as \code{old.ip}}
  \item{old.items}{A vector of indexes pointing to those items in \code{old.ip} that are common to both sets of parameters}
  \item{new.items}{The indexes of the same items in \code{new.ip}}
  \item{old.qu}{A quadrature object for \code{old.ip}, typically produced by the same program that estimated \code{old.ip}. Only needed if \code{method="LS"} or \code{method="HB"}}
  \item{new.qu}{A quadrature object for \code{new.ip}, typically produced by the same program that estimated \code{new.ip}. Only needed if \code{method="HB"}}
  \item{method}{One of "MM" (Mean/Mean), "MS" (Mean/Sigma), "SL" (Stocking-Lord), or "HB" (Haebara). Default is "MS"}
}
%\details{Add if not too lazy}
\value{
A list of:
  \item{slope}{The slope of the linear transformation}
  \item{intercept}{The intercept of the linear transformation}
  \item{scaled.ip}{The parameters in \code{new.ip} tranformed to a scale that is compatible with \code{old.ip}}
}
\references{Kolen, M.J. & R.L. Brennan (1995) Test Equating: Methods and
Practices. Springer.}
\author{Ivailo Partchev}
\note{
Although the Stocking-Lord and the Haebara methods were programmed
after Kolen & Brennan (1995), they do not produce the same results 
as the program ST. Results for Stocking-Lord come closer to ST than
those for the Haebara method.
}
\examples{
# a small simulation to demonstrate transformation to a common scale
# fake 50 2PL items
pa <- cbind(runif(50,.8,2), runif(50,-2.4,2.4), rep(0,50))
# simulate responses with  two samples of different ability levels
r.1 <- sim(ip=pa[1:30,],  x=rnorm(1000,-.5))
r.2 <- sim(ip=pa[21:50,], x=rnorm(1000,.5))
# estimate item parameters
p.1 <- est(r.1, engine="ltm")
p.2 <- est(r.2, engine="ltm")
# plot difficulties to show difference in scale
plot(c(-3,3), c(-3,3), ty="n", xlab="True",ylab="Estimated",
   main="Achieving common scale")
text(pa[1:30,2],  p.1[,2], 1:30)
text(pa[21:50,2], p.2[,2], 21:50, co=2)
# scale with the default Mean/Sigma method
pa.sc = sca(old.ip=p.1, new.ip=p.2, old.items=21:30, new.items=1:10)
# add difficulties of scaled items to plot
text(pa[21:50,2], pa.sc$scaled.ip[,2], 21:50, co=3)
}
\keyword{models}
