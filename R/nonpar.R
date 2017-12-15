#' Item fit plot 
#' 
#' Produces a plot that compares the estimated trace line (item response function)
#' for an item with the data. The data is represented with a non-parametric
#' trace line computed as in \code{tgf} (not \code{npp}). Approximate confidence
#' intervals for the parametric line are also shown.
#' 
#' Comparing the outputs of \code{npp} and \code{irf} has at least two drawbacks:
#' (i) package \code{sm} is used as a black box, and (ii) the confidence intervals
#' are drawn around the data (even though represented by the nonparametric regression)
#' rather than the parametric curve. In this function, the parametric curve is shown in
#' red, with a 68% confidence interval in pink and a 95% confidence interval in paler pink.
#' The non-parametric curve representing the data is shown in black. It is computed 
#' by applying Bayes theorem to the density of ability.
#' The standard error of the parametric curve is computed by
#' applying the delta theorem on the standard error of the item parameters, as computed 
#' by the underlying software (currently only available with ICL).  
#' @param resp A matrix of responses: persons as rows, items as columns,
#' entries are either 0 or 1, no missing data
#' @param ip Item parameters: the object returned by function \code{est} -- 
#' note that the complete object is required since the standard errors play an important part.
#' @param x A vector of abilities, as long as there are persons, against which to plot
#' probabilities of a correct response. Normally these would be the output of \code{qrs},
#' and these are provided by default when x is NULL. However, the user may want to use 
#' something else, or pass the same \code{qrs} output to several plots (various calls to
#' \code{qrs} provide slightly different output because of the random breaking of ties.) 
#' @param item The item for which a plot is requested (column of \code{resp}).
#' @param main The main title of the plot.
#' @param use.sm When TRUE, empirical data will be shown with a regression estimated
#' by \code{sm}, otherwise the Bayes theorem will be used. Default is FALSE.
#' @author Ivailo Partchev
#' @seealso \code{\link{irf}}, \code{\link{npp}}, \code{\link{tgf}}
#' 
#' @examples
#' 
#' # a plot for item 5 
#' irfPlot(Scored, ip=b3, item=4)
#' 
irfPlot = function(resp, ip, x=NULL, item, main="Parametric vs non-parametric trace line", use.sm=FALSE) {
  # the parametric trace line
  abc = ip$est[item,,drop=FALSE]
  vcm = ip$vcm[[item]]
  ptl = irf(ip=abc)
  P = ptl$f
  plot(c(-4,4),c(0,1),type="n",xlab="Ability",ylab="Probability of a correct response",
       main=main,sub=paste("Item",item))
  if(!is.null(ip$vcm)) {
    no_asymp = sd(ip$est[,3])==0
    no_slope = sd(ip$est[,1])==0
    pc = (1-P) / (1-abc[3])
    if (no_asymp) {
      if (no_slope) SE=P*(1-P)*sqrt(ip$vcm[[item]]) else {
        grad = cbind(P*(1-P), P*(1-P)*ptl$x)
        SE = sqrt(diag(grad %*% vcm %*% t(grad)))
      }
    } else {
      grad = cbind(pc*(ptl$x-abc[2])*(P-abc[3]), -abc[1]*pc*(P-abc[3]), pc)
      SE = sqrt(diag(grad %*% vcm %*% t(grad)))
      SE[!is.finite(SE)]=0
    }
    polygon(c(ptl$x,rev(ptl$x)),c(P+2*SE,rev(P-2*SE)),col="mistyrose",border=NA) 
    polygon(c(ptl$x,rev(ptl$x)),c(P+SE,rev(P-SE)),col="pink",border=NA) 
  }
  lines(ptl$x,P,type="l",col=2,lwd=2)
  # the non-parametric
  if(is.null(x)) x = qrs(resp) 
  y = resp[,item]
  if (use.sm) {
    h = h.select(x=x, y=y)
    br = sm.binomial(x=x, y=y, h=h, display="none")
    lines(br$eval.points, br$estimate, lw=2)
  } else {
    n = 512
    dx = density(x, bw = "nrd0", n = n)
    x1 = dx$x
    yprop = mean(y)
    dxi = density(x[y == 1], bw = dx$bw, n = n, from = min(dx$x), to = max(dx$x))
    y1 = dxi$y/dx$y * yprop
    y1 = y1[which(x1 >= min(x) & x1 <= max(x))]
    x1 = x1[x1 >= min(x) & x1 <= max(x)]
    lines(x1,y1,lwd=2,col=1)
  }
}
  

#' Non-parametric characteristic curves
#' 
#' A plotting routine producing non-parametric analogues of the IRF not unlike
#' those in Jim Ramsay's TestGraf program.  The curves are produced by a kernel
#' binomial regression of the actual responses to an item on some estimates of
#' the latent variable, by courtesy of package \code{sm}.
#' 
#' 
#' @param resp A matrix of responses: persons as rows, items as columns,
#' entries are either 0 or 1, no missing data
#' @param x The values of the latent variable ("ability") for the same persons
#' whose responses are given in \code{resp}. If not given, function \code{qrs}
#' will be plugged in, which is the approach of TestGraf
#' @param items An index to the items (columns of \code{resp}) to be shown on
#' the plot. If not given, all items will be plotted.
#' @param from Lower limit for ability on the plot. Default is -4.
#' @param to Upper limit for ability on the plot. Default is 4.
#' @param add When \code{add=T}, the curve is added to a plot, otherwise a new
#' plot is started. Default is F.
#' @param main The main title of the plot, given that \code{add=F}.
#' @param co The colour of the curves. Default is 1 for black. Use \code{co=NA}
#' to plot each curve in a different colour.
#' @param bands When \code{bands=T}, confidence bands are added.
#' @param label When \code{label=T}, individual curves will be labeled with the
#' item number.
#' @author Ivailo Partchev
#' @seealso \code{\link{qrs}}, \code{\link{irf}}, \code{\link{plot.irf}}
#' @references James O. Ramsay (2000). TestGraf: A program for the graphical
#' analysis of multiple choice test and questionnaire data. McGill University,
#' Montreal, Canada
#' @keywords models
#' 
#' @examples
#' 
#' # plot items 1:5 in different colours, label
#' npp(Scored, items=1:5, co=NA, label=TRUE)
#' 
#' # For item 7, compare npp with the 2PL parametric IRF 
#' npp(Scored, items=7, bands=TRUE)
#' plot(irf(ip=Scored2pl, items=7), co=3, add=TRUE)
#' 
npp = function(resp, x, items, from=-4, to=4, co=1, 
  main="Non-parametric response function", add=FALSE, bands=FALSE, label=FALSE) {
  if (missing(x)) x = qrs(resp)  
  if (missing(items)) items = 1:ncol(resp) 
  if (!add) plot(c(from,to),c(0,1),xlab="Ability",
    ylab="Probability of a correct response", main=main, ty="n")
  invisible(lapply(items, function(i) {
    if (is.na(co)) co = i
    h = h.select(x=x,y=resp[,i])
    br=sm.binomial(x=x,y=resp[,i],h=h,display="none")
    if (bands) segments(br$eval.points,br$lower,br$eval.points,br$upper,col=co)
    lines(br$eval.points,br$estimate,lw=2,co=co)
    if (label) {
      lx = sample(1:length(br$eval.points),1)
      points(br$eval.points[lx],br$estimate[lx],co="white",cex=1.6,pch=19)
      text(br$eval.points[lx],br$estimate[lx],i,co=1,cex=.6)
    }}))  
}

#' Quantiles of the ranked sum scores
#' 
#' A rough estimate of the values of the latent variable ("ability"). The sum
#' scores (number of correct responses) are ranked, breaking ties at random.
#' The ranks are divided by the sample size + 1, and the corresponding
#' quantiles of the standard Normal distribution are returned. Used as default
#' in the non-parametric IRF plots produced by \code{\link{npp}} in analogy to
#' Jim Ramsay's TestGraf. Another possible use is in \code{\link{itf}}.
#' 
#' 
#' @param resp A matrix of responses: persons as rows, items as columns,
#' entries are either 0 or 1, no missing data
#' @return A one-column matrix of values
#' @author Ivailo Partchev
#' @seealso \code{\link{npp}}, \code{\link{itf}}
#' @keywords models
#' 
#' @examples
#' 
#' sc <- qrs(Scored)
#' 
qrs = function(resp) {
  raw.scores = apply(resp, 1, sum, na.rm=TRUE)
  ranks = rank(raw.scores, ties.method = "random")
  return(as.matrix(qnorm(ranks/(length(ranks)+1))))
}


#' Non-parametric option curves
#' 
#' A plotting function producing non-parametric analogues of the IRF for each
#' option in a multiple choice item not unlike those in Jim Ramsay's TestGraf
#' program.
#' 
#' 
#' @param choices A matrix of responses to multiple-choice items: persons as
#' rows, items as columns. As a rare exception in \code{irtoys}, responses must
#' not be recoded to 0/1, and there may be missing responses.
#' @param key A vector containing the key (correct answers) to the items in
#' \code{choices}.
#' @param item A single number pointing to the item (column of \code{choices})
#' to plot.
#' @param main The main title of the plot, given that \code{add=F}.
#' @param co The colour of the curves. Default is 1 for black. Use \code{co=NA}
#' to plot each curve in a different colour.
#' @param label When \code{label=T}, individual curves will be labeled with the
#' item number.
#' @author Ivailo Partchev
#' @seealso \code{\link{qrs}}, \code{\link{irf}}, \code{\link{plot.irf}}
#' @references James O. Ramsay (2000). TestGraf: A program for the graphical
#' analysis of multiple choice test and questionnaire data. McGill University,
#' Montreal, Canada
#' @keywords models
#' 
#' @examples
#' 
#' key=c(2,3,1,1,4,1,2,1,2,3,3,4,3,4,2,2,4,3)
#' tgf(choices=Unscored, key=key, item=4, co=NA, label=TRUE)
#' 
tgf = function(choices, key, item, 
    main="Non-parametric response function", co=1, label=FALSE) {
  if (!(item %in% 1:ncol(choices))) stop("bad item number")
  x = qrs(sco(choices, key))
  y = choices[, item]
  cc = complete.cases(x,y)
  x  = x[cc]
  y  = y[cc]
  n = 512
  plot(c(-3, 3), c(0, 1), xlab="Ability", 
    ylab="Probability of a correct response", main=main, ty="n")
  ft = table(y) 
  ny = length(ft)
  dx = density(x, bw="nrd0", n=n)
  x1 = dx$x
  yprop = prop.table(ft)
  y1 = matrix(0, ny, n)  
  for(i in 1:ny) {
    dxi = density(x[y==i], bw=dx$bw, n=n, from=min(dx$x), to=max(dx$x))
    y1[i,] = dxi$y/dx$y * yprop[i]
  }
  y1 = y1[,which(x1 >= min(x) & x1 <= max(x))]
  x1 = x1[x1 >= min(x) & x1 <= max(x)]
  invisible(sapply(1:nrow(y1), function(i) {
    if (is.na(co)) co = i
    lines(x1, y1[i,], lw=2, co=co)
    if (label) {
      lx = sample(1:length(x1), 1)
      points(x1[lx], y1[i,lx], co="white", cex=1.6, pch=19)
      text(x1[lx], y1[i,lx], i, co=1, cex=0.6)
    }
  }))
}
