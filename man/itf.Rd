\name{itf}
\alias{itf}
\title{Test item fit}
\usage{
  itf(resp, ip, item, stat = "lr", theta,
    standardize = TRUE, mu = 0, sigma = 1, bins = 9,
    breaks = NULL, equal = "count", type = "means",
    do.plot = TRUE, main = "Item fit")
}
\arguments{
  \item{resp}{A matrix of responses: persons as rows, items
  as columns, entries are either 0 or 1, no missing data}

  \item{ip}{Item parameters: a matrix with one row per
  item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}.}

  \item{item}{A single number pointing to the item (column
  of \code{resp}, row of \code{ip}), for which fit is to be
  tested}

  \item{stat}{The statistic to be computed, either
  \code{"chi"} or \code{"lr"}. Default is \code{"lr"}. See
  details below.}

  \item{theta}{A vector containing some viable estimate of
  the latent variable for the same persons whose responses
  are given in \code{resp}. If not given (and \code{group}
  is also missing), EAP estimates will be computed from
  \code{resp} and \code{ip}.}

  \item{standardize}{Standardize the distribution of
  ability estimates?}

  \item{mu}{Mean of the standardized distribution of
  ability estimates}

  \item{sigma}{Standard deviation of the standardized
  distribution of ability estimates}

  \item{bins}{Desired number of bins (default is 9)}

  \item{breaks}{A vector of cutpoints. Overrides
  \code{bins} if present.}

  \item{equal}{Either \code{"width"} for bins of equal
  width, or \code{"count"} for bins with roughly counts of
  observations. Default is \code{"quant"}}

  \item{type}{The points at which \code{itf} will evaluate
  the IRF. One of \code{"mids"} (the mid-point of each
  bin), \code{"meds"} (the median of the values in the
  bin), or \code{"means"} (the mean of the values in the
  bin). Default is \code{"means"}.}

  \item{do.plot}{Whether to do a plot}

  \item{main}{The title of the plot if one is desired}
}
\value{
  A vector of three numbers: \item{Statistic}{The value of
  the statistic of item fit} \item{DF}{The degrees of
  freedom} \item{P-value}{The p-value}
}
\description{
  Returns a statistic of item fit together with its degrees
  of freedom and p-value. Optionally produces a plot.
}
\details{
  Given a long test, say 20 items or more, a large-test
  statistic of item fit could be constructed by dividing
  examinees into groups of similar ability, and comparing
  the observed proportion of correct answers in each group
  with the expected proportion under the proposed model.
  Different statistics have been proposed for this purpose.

  The chi-squared statistic
  \deqn{X^2=\sum_g(N_g\frac{(p_g-\pi_g)^2}{\pi_g(1-\pi_g)},}
  where \eqn{N_g} is the number of examinees in group
  \eqn{g}, \eqn{p_g=r_g/N_g}, \eqn{r_g} is the number of
  correct responses to the item in group \eqn{g}, and
  \eqn{\pi_g} is the IRF of the proposed model for the
  median ability in group \eqn{g}, is attributed by
  Embretson & Reise to R. D. Bock, although the article
  they cite does not actually mention it. The statistic is
  the sum of the squares of quantities that are often
  called "Pearson residuals" in the literature on
  categorical data analysis.

  BILOG uses the likelihood-ratio statistic
  \deqn{X^2=2\sum_g\left[r_g\log\frac{p_g}{\pi_g} +
  (N_g-r_g)\log\frac{(1-p_g)}{(1-\pi_g)}\right],} where
  \eqn{\pi_g} is now the IRF for the mean ability in group
  \eqn{g}, and all other symbols are as above.

  Both statistics are assumed to follow the chi-squared
  distribution with degrees of freedom equal to the number
  of groups minus the number of parameters of the model (eg
  2 in the case of the 2PL model). The first statistic is
  obtained in \code{itf} with \code{stat="chi"}, and the
  second with \code{stat="lr"} (or not specifying
  \code{stat} at all).

  In the real world we can only work with estimates of
  ability, not with ability itself. \code{irtoys} allows
  use of any suitable ability measure via the argument
  \code{theta}. If \code{theta} is not specified,
  \code{itf} will compute EAP estimates of ability, group
  them in 9 groups having approximately the same number of
  cases, and use the means of the ability eatimates in each
  group. This is the approximate behaviour of BILOG.

  If the test has less than 20 items, \code{itf} will issue
  a warning. For tests of 10 items or less, BILOG has a
  special statistic of fit, which can be found in the BILOG
  output. Also of interest is the fit in 2- and 3-way
  marginal tables in package \code{ltm}.
}
\examples{
fit   <- itf(resp=Scored, ip=Scored2pl$est, item=7)
}
\author{
  Ivailo Partchev
}
\references{
  S. E. Embretson and S. P. Reise (2000), Item Response
  Theory for Psychologists, Lawrence Erlbaum Associates,
  Mahwah, NJ

  M. F. Zimowski, E. Muraki, R. J. Mislevy and R. D. Bock
  (1996), BILOG--MG. Multiple-Group IRT Analysis and Test
  Maintenance for Binary Items, SSI Scientific Software
  International, Chicago, IL
}
\seealso{
  \code{\link{eap}}, \code{\link{qrs}}
}
\keyword{models}

