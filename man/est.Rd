\name{est}
\alias{est}
\title{Estimate item parameters}
\usage{
  est(resp, model = "2PL", engine = "icl", nqp = 20,
    est.distr = FALSE, nch = 5, a.prior = TRUE,
    b.prior = FALSE, c.prior = TRUE, bilog.defaults = TRUE,
    rasch = FALSE, run.name = "mymodel")
}
\arguments{
  \item{resp}{A matrix of responses: persons as rows, items
  as columns, entries are either 0 or 1, no missing data}

  \item{model}{The IRT model: "1PL", "2PL", or "3PL".
  Default is "2PL".}

  \item{engine}{One of "icl", "bilog", or "ltm". Default is
  "icl".}

  \item{nqp}{Number of quadrature points. Default is 20.}

  \item{est.distr}{T if the probabilities of the latent
  distribution are to be estimated, F if a normal
  distribution is assumed. Default is F. Ignored when
  \code{engine="ltm"}.}

  \item{nch}{Number of choices in the original item
  formulation. Used to determine the prior for the
  asymptote when \code{engine="bilog"}, \code{model="3PL"},
  and \code{c.prior=T}. Default is 5.}

  \item{a.prior}{Whether a prior for the item
  discriminations is used. Ignored when \code{model="1PL"}
  or \code{engine="ltm"}. Default is T.}

  \item{b.prior}{Whether a prior for the item difficulties
  is used. Ignored when \code{engine="ltm"}. Default is F.}

  \item{c.prior}{Whether a prior for the asymptotes is
  used. Ignored when \code{model="1PL"} or
  \code{model="2PL"} or \code{engine="ltm"}. Default is T.}

  \item{bilog.defaults}{When \code{engine="icl"} and a
  prior is used, use the default priors in BILOG rather
  than the default priors in ICL. Ignored when
  \code{engine="ltm"}. Default is T.}

  \item{rasch}{When \code{engine="bilog"} and
  \code{model="1PL"} and \code{"rasch"=T}, the common value
  for discriminations is forced to 1, and the sum of the
  difficulties is 0. When \code{engine="ltm"} and
  \code{model="1PL"} and \code{"rasch"=T}, the common value
  for discriminations is forced to 1. Ignored in all other
  cases. Default is F.}

  \item{run.name}{A (short) string used in the names of all
  files read or written by ICL or BILOG. Default is
  \code{"mymodel"}. Change to something else to keep the
  outputs of ICL of BILOG for further use. Ignored when
  \code{engine="ltm"}}
}
\value{
  A list with two elements, \code{est} and \code{se}, for
  the parameter estimates and their standard errors,
  correspondingly. Each element is a matrix with one row
  per item, and three columns: [,1] item discrimination
  \eqn{a}, [,2] item difficulty \eqn{b}, and [,3] asymptote
  \eqn{c}. For the 1PL and 2PL models, all asymptotes are
  equal to 0; for the 1PL, the discriminations are all
  equal but not necessarily equal to 1. When ICL is used as
  estimation engine, \code{se} is NULL as ICL does not
  compute standard errors for the item parameter estimates.
}
\description{
  Estimate IRT item parameters using either ICL, BILOG, or
  \code{ltm}. Provides access to the most widely used
  options in these programs.
}
\details{
  Estimate the parameters of an IRT model defined in the
  most general case ("3PL") as
  \deqn{P(U_{ij}=1|\theta_i,a_j,b_j,c_j)=c_j+(1-c_j)\frac{\displaystyle\exp(a_j(\theta_i-b_j))}{1+\displaystyle\exp(a_j(\theta_i-b_j))}}
  where \eqn{U_{ij}} is a binary response given by person
  \eqn{i} to item \eqn{j}, \eqn{\theta_i} is the value of
  the latent variable ("ability") for person \eqn{i},
  \eqn{a_j} is the discrimination parameter for item
  \eqn{j}, \eqn{b_j} is the difficulty parameter for item
  \eqn{j}, \eqn{c_j} is the asymptote for item \eqn{j}.

  Some authors prefer to represent the model with a logit
  \eqn{1.7a^*_j(\theta_i-b_j)} rather than
  \eqn{a_j(\theta_i-b_j)}. This option has been removed
  from \code{irtoys} as it is not supported by the
  remaining functions of the package.

  In the 2PL model (\code{model="2PL"}), all asymptotes
  \eqn{c_j} are 0. In the 1PL model (\code{model="1PL"}),
  all asymptotes \eqn{c_j} are 0 and the discriminations
  \eqn{a_j} are equal for all items (and sometimes to 1).

  Package \code{irtoys} provides a simple common interface
  to the estimation of item parameters with three different
  programs. It only accesses the most basic and widely used
  options in these programs. Each of the three programs has
  a much wider choice of options and cababilities, and
  serious users must still learn the corresponding syntax
  in order to access the advanced features.  Even when
  models are fit "by hand", \code{irtoys} may be useful in
  plotting results, doing comparisons across programs etc.

  Estimation of the more complex IRT models (2PL and 3PL)
  for some "difficult" data sets often has to use prior
  distributions for the item parameters. \code{irtoys}
  adopts the default behaviour of BILOG: no priors for
  \eqn{b} in any model, priors for \eqn{a} in the 2PL and
  3PL models, priors for \eqn{c} in the 3PL model. This can
  be overriden by changing the values of \code{a.prior},
  \code{b.prior}, and \code{c.prior}.

  If priors are used at all, they will be the same for all
  items. Note that both ICL and BILOG can, at some
  additional effort, set different priors for any
  individual item. At default, the common priors are the
  BILOG defaults: \code{normal(0,2)} for \eqn{b},
  \code{lognormal (0, 0.5)} for \eqn{a}, and
  \code{beta(20*p+1, 20(1-p)+1)} for \eqn{c}; \eqn{p} is 1
  over the number of choices in the original item
  formulations, which can be set with the parameter
  \code{nch}, and is again assumed the same for all items.

  When \code{engine="icl"} and \code{bilog.defaults=F}, any
  priors used will be the ICL default ones, and based on
  the 4-parameter beta distribution: \code{beta(1.01, 1.01,
  -6, 6)} for \eqn{b}, \code{beta(1.75, 3, 0, 3)} for
  \eqn{a}, and \code{beta(3.5, 4, 0, 0.5)} for \eqn{c}.
  When \code{engine="ltm"}, all commands involving priors
  are ignored.

  \code{est} only works when some IRT software is
  installed.  Package \code{ltm} is automatically loaded.
  ICL can be downloaded from \url{www.b-a-h.com}. BILOG is
  commercial software sold by SSI --- see
  \url{www.ssicentral.com} for further detail.  On Windows,
  make sure that the executable files (\code{icl.exe} for
  ICL, \code{blm1.exe}, \code{blm2.exe}, and
  \code{blm3.exe}, for BILOG) are located in directories
  that are included in the PATH variable.
}
\examples{
p.1pl <- est(Scored, model="1PL", engine="ltm")
p.2pl <- est(Scored, model="2PL", engine="ltm")
}
\author{
  Ivailo Partchev
}
\references{
  Bradley A. Hanson (2002), ICL: IRT Command Language.
  \url{www.b-a-h.com}

  Dimitris Rizopoulos (2006). ltm: Latent Trait Models
  under IRT. \url{cran.r-project.org}

  M. F. Zimowski, E. Muraki, R. J. Mislevy and R. D. Bock
  (1996), BILOG--MG. Multiple-Group IRT Analysis and Test
  Maintenance for Binary Items, SSI Scientific Software
  International, Chicago, IL.  \url{www.ssicentral.com}
}
\keyword{models}

