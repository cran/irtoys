#' Interaction model
#' 
#' Estimates the parameters of the interaction model. The plots can be produced with the
#' appropriate plotting method.
#' 
#' 
#' @param x A matrix of responses: persons as rows, items as columns,
#' entries are either 0 or 1, no missing data
#' @param eps Convergence criterion, default is 1e-1
#' @param rasch if TRUE, the Rasch model will be estimated instead.
#' @return A matrix
#' @author Ivailo Partchev, using C code and theory by Gunter Maris
#' @references Shelby Haberman. The interaction model. Chapter in: 
#' Mathias von Davier & Claus H. Carstensen (Eds.) (2007). 
#' Multivariate and Mixture Distribution Rasch Models. New York:Springer 
#' @keywords models
#' 
interactionModel = function(x, eps=1e-1, rasch=FALSE) {
  # not sure if I can handle NAs -- remove until I find out
  x[is.na(x)] = 0
  # do the sufficient statistics
  sumsc = rowSums(x)
  bsuf = colSums(x)
  csuf = crossprod(sumsc-1, x)
  b = runif(length(bsuf))
  c = rep(0,length(bsuf))
  score=sapply(0:length(bsuf),function(p)sum(sumsc==p))
  r = IM_update(b,c,bsuf,csuf,score,rasch)
  itr = 1
  while (r$conv > eps)
  {
  # cat("Iteration ", itr, "    Criterion ", r[[3]], "\n")
   r = IM_update(r[[1]],r[[2]],bsuf,csuf,score,rasch)
   itr = itr + 1
  }
  result = r
  result$scores=score
  class(result) = "imp"
  return(result)
}


#' A plot method for the interaction model
#' 
#' Plot the item-total regressions fit by the interaction (or Rasch) model
#' 
#' 
#' @param x An object produced by function \code{interactionModel}
#' @param items The items to plot (column numbers). If NULL, all items will be plotted
#' @param shade The part of the probability mass for the sum scores to shade out,
#' shown as percentage. Default is 10: shade the most extreme 10% of the distribution.
#' Ignored when \code{add=TRUE}.
#' @param highlight Cutpoint for the interaction parameter below which a regression
#' line will be highlighted in red. Default is -Inf (do not highlight).
#' @param add When \code{add=TRUE}, the graph is added to a plot, otherwise a new
#' plot is started. Default is FALSE.
#' @param main The main title of the plot, given that \code{add=FALSE}.
#' @param label When \code{label=TRUE}, individual curves will be labeled with the
#' item number.
#' @param ... Any additional plotting parameters
#' @author Ivailo Partchev, using theory and C code by Gunter Maris
#' @seealso \code{\link{interactionModel}}
#' @keywords models
#' @method plot imp
#' 
#' @examples
#' 
#' plot(interactionModel(Scored), highlight=-.3)
#' 
plot.imp = function(x, items=NULL, shade=10, highlight=-Inf, 
    add=FALSE, main="Item-total regression", label=FALSE, ...) {
  p = curv(x)
  n = ncol(p)
  if (!is.null(items)) items=sort(items) else items=1:n  
  p = p[, items, drop=FALSE]
  k = ncol(p)
  shade = shade/200
  scores = rep(0:n, x$scores)
  q = quantile(scores, c(shade, 1-shade))
  if (!add) {
    plot(c(0,n), c(0,1), ty="n", xlab="Sum score", ylab="Probability", main=main, ...)
    tmp = par('usr')
    # par(new=TRUE)
    rect(tmp[1], tmp[3], q[1], tmp[2], col="#DDDDDD", border=NA)
    rect(q[2], tmp[3], tmp[2], tmp[4], col="#DDDDDD", border=NA)
  }
  couleur = 1 + (x$c[items] < highlight)
  for (i in 1:k) lines(0:n, p[,i], col=couleur[i], ...)
  if (label) {
    lx = sample(0:n, k, replace = FALSE)
    for (i in 1:k) {
      points(lx[i], p[lx[i]+1,i], co="white", cex=1.6, pch=19)
      text(lx[i], p[lx[i]+1,i], items[i], co=1, cex=.6)
    }
  }
  box()
}  

#void Update(double *b, double *c, int *n, int *m, int *bsuf, int *csuf, double *converged);
# make a wrapper around .C
IM_update=function(b,c,bsuf,csuf,score,rasch=FALSE)
{
  n=length(b)
  converged=-1
  tmp<-.C("Update",
          b=as.double(b),
          c=as.double(c),
          n=as.integer(n),
          score=as.integer(score),
          bsuf=as.integer(bsuf),
          csuf=as.integer(csuf),
          converged=as.double(converged),
          rasch=as.integer(rasch))
  return(list(b=tmp$b,c=tmp$c,conv=tmp$converged))
}

curv = function(x) {
  #void ItTotal(double *b, double *c, int *n, double* prob, int *i)
  n = length(x$b) # nItems
  b = x$b
  c = x$c
  o = matrix(0, n+1, n)
  for (i in 1:n) {
    prob = double(n+1)
    tmp <- .C("ItTotal",
              as.double(b),
              as.double(c),
              as.integer(n),
              as.double(prob),        
              as.integer(i-1))
    prob = tmp[[4]]
    o[,i] = prob
  }
  o
}

clik<-function(x,b,c)
{
  n=ncol(x)
  score=rowSums(x)
  l=rep(0,nrow(x))
  for (s in 1:(n-1))
  {
    who=which(score==s)
    g=elsym(exp(b+(s-1)*c))
    l[who]=x[who,]%*%(b+(s-1)*c)-log(g[s+1])
  }
  return(l)
}


elsym = function(b) {
  n = length(b)
  g = rep(0, n)
  j = -1L
  tmp = .C("ElSym",
          b=as.double(b),
          n = as.integer(n),
          g = as.double(g),
          j = as.integer(j))
  return(tmp$g)
}

#' Item-total regressions for the Rasch vs. the interaction model
#' 
#' Compare the item-total regressions fit by the Rasch model and 
#' the interaction model, for one, several, or all items in the test.
#' 
#' 
#' @param x A matrix of scored responses (persons in rows, items in columns)
#' @param items The items to plot (column numbers). If NULL, all items will be plotted
#' @param showData If TRUE, the observed proportion correct at each sum score will
#' be shown on the plot. Default is FALSE (show only the regressions)
#' @param shade The part of the probability mass for the sum scores to shade out,
#' shown as percentage. Default is 10: shade the most extreme 10% of the distribution.
#' Ignored when \code{add=TRUE}.
#' @param ncol When plotting  multiple items, the user can suggest the number of
#' columns in the matrix of plots (up to 3, will be adjusted if necessary)
#' @param ... Any additional plotting parameters
#' @author Ivailo Partchev, using theory and C code by Gunter Maris
#' @seealso \code{\link{interactionModel}}
#' @keywords models
#' 
#' @examples
#' 
#' rim(Scored, items=2)
#' 
rim = function(x, items=NULL, showData=FALSE, shade=10, ncol=3, ...) {
  ncol = min(ncol,3)
  ss = rowSums(x)
  xx = sort(unique(ss))
  rm = interactionModel(x, rasch=TRUE)
  im = interactionModel(x)
  rmp = curv(rm)
  imp = curv(im)
  n = ncol(rmp)
  if (!is.null(items)) items=sort(items) else items=1:n  
  npic = length(items)
  ncol = min(ncol, npic)
  nrow = npic %/% ncol + npic %% ncol
  nrow = min(3, nrow)
  mxs = nrow*ncol
  layout(matrix(1:mxs, byrow=TRUE, ncol=ncol))
  shade = shade/200
  sumScores = rep(0:n, rm$scores)
  q = quantile(sumScores, c(shade, 1-shade))
  for (i in items) {
    plot(c(0,n), c(0,1), ty="n", xlab="Sum score", ylab="Probability", 
         main=paste("Item",i), ...)
    tmp = par('usr')
    # par(new=TRUE)
    rect(tmp[1], tmp[3], q[1], tmp[2], col="#DDDDDD", border=NA)
    rect(q[2], tmp[3], tmp[2], tmp[4], col="#DDDDDD", border=NA)
    if (showData) {
      yy = tapply(x[,i], ss, mean)
      points(xx, yy)
    }
    lines(0:n, rmp[,i], col=1)
    lines(0:n, imp[,i], col=2)
    box()
  }
  layout(1)
  return(NULL)
}