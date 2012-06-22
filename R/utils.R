#' Simulate response data
#' 
#' Simulate responses from the 1PL, 2PL, or 3PL model
#' 
#' 
#' @param ip Item parameters: a matrix with one row per item, and three
#' columns: [,1] item discrimination \eqn{a}, [,2] item difficulty \eqn{b}, and
#' [,3] asymptote \eqn{c}.
#' @param x A vector of values of the latent variable ("abilities").
#' @return A matrix of responses: persons as rows, items as columns, entries
#' are either 0 or 1, no missing data
#' @author Ivailo Partchev
#' @keywords models
#' @export
#' @examples
#' 
#' pa <- cbind(runif(20,.8,2), runif(20,-2.4,2.4), rep(0,50))
#' rs <- sim(ip=pa, x=rnorm(1000))
#' 
sim = function(ip, x=NULL) {
  i = irf(ip,x)
  d = dim(i$f)
  u = runif(d[1]*d[2])
  dim(u) = d
  return(ifelse(i$f  > u, 1, 0))
}


#' Score a multiple choice test
#' 
#' Given a key, score a multiple choice test, i.e. recode the original choices
#' to right (1) or wrong (0). Missing responses are treated as wrong.
#' 
#' 
#' @param choices The original responses to the items in the test: persons as
#' rows, items as columns. May contain NA.
#' @param key A vector containing the key (correct answers) to the items in
#' \code{choices}. If not given, the function will check if all data are either
#' 0, 1, or NA: if yes, NA are recoded as 0, else an error message is returned.
#' @param na.false Recode non-responses to false responses?
#' @return A matrix of responses scored 0=wrong 1=correct, and possibly NA
#' @author Ivailo Partchev
#' @keywords models
#' @export
#' @examples
#' 
#' res <- sco(Unscored, key=c(2,3,1,1,4,1,2,1,2,3,3,4,3,4,2,2,4,3))
#' 
sco = function(choices, key, na.false=FALSE) {
  if(missing(key)) {
    if (all(choices %in% c(0,1,NA))) key = rep(1,ncol(choices)) else
    stop("trying to score multiple choices without a key")
  }
  if(length(key) != ncol(choices)) stop ("wrong length of key")
  correct = sapply(1:ncol(choices), function(i) as.numeric(choices[,i]==key[i]))
  if (na.false) correct[is.na(correct)] = 0
  return(correct)
}





#' Elementary test-item analysis
#' 
#' Elementary analysis of the items in a test and the test sumscores
#' based on Classical Test Theory.
#' 
#' @param choices The original responses to the items in the test: persons as
#' rows, items as columns. May contain NA.
#' @param key A vector containing the key (correct answers) to the items in
#' \code{choices}. If not given, the function will check if all data are either
#' 0, 1, or NA: if yes, NA are recoded as 0, else an error message is returned.
#' @param ... Other parameters that may be passed to \code{sco} or \code{cov}
#' @return A list with three elements: 
#' \describe{
#'   \item{testlevel}{A list of statistics at test level (currently, only
#'   Cronbach's alpha, may be extended in future)}
#'   \item{itemlevel}{A matrix showing, for each item, the proportion of
#'   correct responses, the correlation with the sum score, and the 
#'   alpha that the test would have if the item were dropped.} 
#'   \item{optionlevel}{A matrix showing, for each possible choice in the
#'   multiple-choice item, the proportion of responses given, and 
#'   the correlation with the sum score for the test (including the item).
#'   The correct response is highlighted with asterisks.}
#' }
#' @author Ivailo Partchev
#' @keywords models
#' @export
#' @examples
#' 
#' itemsum <- tia(Unscored, key=c(2,3,1,1,4,1,2,1,2,3,3,4,3,4,2,2,4,3))
#' 
tia = function(choices, key, ...) {
  stopifnot(is.matrix(choices), (nc <- ncol(choices)) == length(key))
  scres = sco(choices, key)
  sumsc = rowMeans(scres, na.rm=T)
  itr = lapply(1:nc, function(i) {
    mmx = model.matrix(~0+as.factor(choices[,i]))
	  pvl = colMeans(mmx)
    itc = cor(cbind(sumsc[!is.na(choices[,i])], mmx))[1,-1]
    px = rep("", ncol(mmx)); px[key[i]]="*"
    res = rbind(pvl,itc)
    attr(res,"dimnames") = list(c("Rel. frequency","Cor. with sum"),
        paste(px,1:ncol(mmx),px,sep=""))
    res
  })
  sx = cov(scres, use="complete")
  k = nrow(sx)
  alpha = k/(k-1)*(1-sum(diag(sx))/sum(sx))
  pva = colMeans(scres, na.rm=T)
  rit = cor(cbind(sumsc,scres),use="complete")[-1,1]
  diagx = sum(diag(sx)) - diag(sx)
  csumx = sum(sx) - 2*colSums(sx) - diag(sx)
  alpha.drop = (k-1)/(k-2)*(1-diagx/csumx)
  tsk=cbind(pva,rit,alpha.drop)
  attr(tsk,"dimnames") = list(paste("Item",1:nc,sep=""), c("Prop. correct","Item-sum cor.","Alpha without"))
  list(testlevel=list(alpha=alpha),itemlevel=tsk,optionlevel=itr)
}


#' Transform a list of lists... to a matrix
#' 
#' When using \code{lapply} or \code{sapply} to avoid explicit loops, one often
#' ends up with complex structures represening lists of lists of lists...
#' Function \code{l2m} tries to transform such a structure into a matrix with a
#' column for each distinct name in the list.
#' 
#' 
#' @param x A list of lists...
#' @return A matrix with a column for each distinct name in the list
#' @author Ivailo Partchev
#' @keywords list
#' @export
l2m = function(x) {
  unx = unlist(x)
  m = NULL
  nam = unique(names(unx))
  for (i in nam) m = cbind(m,unx[names(unx)==i])
  rownames(m) = NULL
  colnames(m) = nam
  return(m)
}
