# item response functions
irf = function(ip,x=NULL) {
  if (is.null(x)) x = seq(-4,4,length=101) 
  if (is.null(dim(ip))) dim(ip) = c(1,3)
  ni = dim(ip)[1]
  f = sapply(1:ni, function(i) ip[i,3] + (1.0 - ip[i,3]) / (1.0 + exp(ip[i,1]*(ip[i,2] - x))))
  r = list(x=x, f=f) 
  class(r) = "irf"
  return(r)
}

plot.irf = function(x,  
    add=FALSE, main="Item response function", co=1, label=FALSE, ...) {
  if (!add) plot(c(min(x$x), max(x$x)), c(0,1), ty="n", xlab="Ability",
  ylab="Probability of a correct response", main=main)
  invisible(lapply(1:ncol(x$f), function(i) {
    if (is.na(co)) co = i
    lines(x$x, x$f[,i], lw=2, co=co)
  }))
  if (label) invisible(lapply(1:ncol(x$f), function(i) {
    lx = sample(1:length(x$x), 1)
    points(x$x[lx], x$f[lx,i], co="white", cex=1.6, pch=19)
    text(x$x[lx], x$f[lx,i],i, co=1, cex=.6)
  })) 
}

# test response function aka expected scores
trf = function(ip,x=NULL) {
  i = irf(ip,x)
  if (is.null(dim(i$f))) dim(i$f) = c(length(i$x),length(i$f))
  f = apply(i$f,1,sum)
  r = list(x=i$x, f=f, ni=ncol(i$f))
  class(r) = "trf"
  return(r)
}

plot.trf = function(x, 
    add=FALSE, main="Test response function", co=1, ...) {
  if (is.na(co)) co = 1
  if (!add) plot(c(min(x$x), max(x$x)), c(0,x$ni), ty="n", xlab="Ability",
  ylab="Expected score", main=main)
  lines(x$x, x$f, lw=2, co=co)
}

# simulate responses for the 3PL model
sim = function(ip, x=NULL) {
  i = irf(ip,x)
  d = dim(i$f)
  u = runif(d[1]*d[2])
  dim(u) = d
  return(ifelse(i$f  > u, 1, 0))
}

# item information function 
iif = function(ip, x=NULL) {
  if (is.null(dim(ip))) dim(ip) = c(1,3)
  ip2 = ip
  ip2[,3] = 0
  i2 = irf(ip2, x)
  p2 = i2$f  
  q2 = 1 - p2
  if (any(ip[,3] != 0)) {
    p3 = irf(ip, x)$f
    q3 = 1 - p3
    f = p2*p2*q3/p3
  } else f  = p2*q2
  asq = rep(ip[,1]*ip[,1], each=length(i2$x))
  r = list(x=i2$x,f=asq*f)
  class(r) = "iif"
  return(r)  
}

plot.iif = function(x,  
  add=FALSE, main="Item information function", co=1, label=FALSE, ...) {
  if (!add) plot(c(min(x$x), max(x$x)), c(0,max(x$f)), ty="n", xlab="Ability",
  ylab="Item information",main=main)
  invisible(lapply(1:ncol(x$f), function(i) {
    if (is.na(co)) co = i
    lines(x$x, x$f[,i], lw=2, co=co)
    }))
    if (label) invisible(lapply(1:ncol(x$f), function(i) {
      lx = sample(1:length(x$x),1)
      points(x$x[lx], x$f[lx,i], co="white", cex=1.6, pch=19)
      text(x$x[lx], x$f[lx,i], i, co=1, cex=.6)
    })) 
}

# test information function 
tif = function(ip, x=NULL) {
  i = iif(ip, x)
  if (is.null(dim(i$f))) dim(i$f) = c(length(i$x),length(i$f))
  f = apply(i$f, 1, sum)
  r = list(x=i$x, f=f, ni=ncol(i$f))
  class(r) = "tif"
  return(r)
}

plot.tif = function(x, add=FALSE, main="Test information function", co=1, ...) {
  if (is.na(co)) co = 1
  if (!add) plot(c(min(x$x), max(x$x)), c(0,max(x$f)), ty="n", xlab="Ability",
  ylab="Information", main=main)
  lines(x$x, x$f, lw=2, co=co)
}

# 3PL log posterior for one person / response vector
# r=responses, p=parm list, x=ability, mu, sigma
llf = function(x,r,p,mu,sigma,method) {
	pr = p[,3] + (1.0 - p[,3])/(1.0 + exp(p[,1]*(p[,2] - x)))
	pr = pmax(pr, .00001); pr = pmin(pr, .99999)
	ll = r*log(pr) + (1-r)*log(1.0-pr)
	lf = sum(ll)
	if (method != "ML") lf = lf + log(dnorm(x,mu,sigma)) 
  return(lf)
} 

mle.one = function(resp, ip, mu=mu, sigma=sigma, method=method) {                                                            
    cc = !is.na(resp)                                        
    resp = resp[cc]                                          
    ip = ip[cc, , drop=FALSE]                                             
    n = length(resp)                                         
    if (n < 1) return(c(NA, NA, 0))                                 
    est = optimize(llf, lower = -4, upper = 4, maximum = TRUE, 
        r = resp, p = ip, mu = mu, sigma = sigma, method = method)$maximum
    ti = tif(ip, est)$f
    if (method != "ML") ti = ti + 1/(sigma * sigma)
    sem = sqrt(1/ti)
    return(c(est, sem, n))
}

# 3PL MLE/BME ability estimates, direct optimization
mlebme = function(resp, ip, mu=0, sigma=1, method="ML") {
 if (is.null(dim(resp))) dim(resp) = c(1,length(resp))
 if (is.null(dim(ip))) stop("item parameters not a matrix")
 if (nrow(ip) != ncol(resp)) stop("responses - item parameters mismatch")
 np = nrow(resp)
 o = sapply(1:np, function(i) mle.one(resp=resp[i,], 
    ip=ip, mu=mu, sigma=sigma, method=method))
 rownames(o) = c("est","sem","n")
 return(t(o)) 
}

# 3PL EAP ability estimate for one person:
# r=responses, p=parm list, u=quad list)
eap.one = function(r, p, qp, qw) {
  cc = !is.na(r)
  r  = r[cc]
  p  = p[cc,,drop=FALSE]
  n  = length(r)
  if (n < 1) return(c(NA, NA, 0))
  ll = sapply(qp, llf, r=r, p=p, mu=NULL, sigma=NULL, method="ML")
  wl = exp(ll)*qw
  swl = sum(wl)
  x  = sum(wl*qp)/swl
  dev = qp - x
  sem = sqrt(sum(wl*dev*dev)/swl)
  return(c(x,sem,n))
}

# 3PL EAP ability estimates
eap = function(resp, ip, qu) {
  if (is.null(dim(resp))) dim(resp) = c(1,length(resp))
  if (is.null(dim(ip))) stop("item parameters not a matrix")
  if (nrow(ip) != ncol(resp)) stop("responses - item parameters mismatch")
  np = nrow(resp)
  qp = qu$quad.points
  qw = qu$quad.weights
  o  = sapply(1:np, function(i) eap.one(r=resp[i,], p=ip, qp, qw),USE.NAMES=FALSE)
  rownames(o) = c("est","sem","n")
  return(t(o))
}

# bias-corrected (Warm's) estimate for one person
bce.one = function(resp, ip) {                                                            
    cc = !is.na(resp)                                        
    resp = resp[cc]                                          
    ip = ip[cc, , drop=FALSE]                                             
    n = length(resp)                                         
    if (n < 1) return(c(NA, NA, 0))                                 
    est = uniroot(scf, lower=-10, upper=10, r=resp, p=ip)$root		
    ev = bcv(est, resp, ip)
	return(c(est, sqrt(ev), n))
}

# variance of the Warm estimator
bcv = function(x,r,p) {
	three = any(p[,3] > 0)
	pr = 1 / (1 + exp(p[,1]*(p[,2] - x)))
	if (three) {
		p3 = p[,3] + (1 - p[,3])*p
		ii = p[,1]^2 / p3 * (1 - p3) * pr^2
	} else {
		ii = p[,1]^2 * pr * (1 - pr)
	}
	isum = sum(ii)
	jsum = sum(ii * p[,1] * (1 - 2*pr))
	return(1.0/isum + jsum^2 / (4 * isum^4))
}
	
# score function (Warm's estimates for the 3PL 
# are unwieldy for direct optimization so use 1st deriv)
# r=responses, p=parm list, x=ability
scf = function(x,r,p) {
	three = any(p[,3] > 0)
	lgt = exp(p[,1] * (x - p[,2]))
	pr = lgt / (1 + lgt)
	z = r - pr
	if (three) z = z - p[,3]*r / (p[,3] + lgt)
	sm = sum(p[,1]*z)
	if (three) {
		pr3 = p[,3] + (1 - p[,3])*pr
		ii = p[,1]^2 / pr3 * (1 - pr3) * pr^2
	} else {
		ii = p[,1]^2 * pr * (1 - pr)
	}
	isum = sum(ii)
	jsum = sum(ii * p[,1] * (1 - 2*pr))
	return(sm + jsum / (isum*2))
}

# Bias-corrected (aka Warm's) ability estimates
wle = function(resp, ip) {
 if (is.null(dim(resp))) dim(resp) = c(1,length(resp))
 if (is.null(dim(ip))) stop("item parameters not a matrix")
 if (nrow(ip) != ncol(resp)) stop("responses - item parameters mismatch")
 np = nrow(resp)
 o = sapply(1:np, function(i) bce.one(resp=resp[i,], ip=ip))
 rownames(o) = c("est","sem","n")
 return(t(o)) 
}

like = function(x, r, p, mu=0, s=1, log=FALSE, post=TRUE) {
  pr = irf(p,x)$f
	pr = pmax(pr, .00001); pr = pmin(pr, .99999)
  ll = log(pr) %*% r + log(1 - pr) %*% (1-r)
  if (post) 
    if (log) ll=ll+dnorm(x,mu,s,log=TRUE) else ll=exp(ll)*dnorm(x,mu,s)
  else if (!log) ll=exp(ll)
  return(ll)
}

ddf = function(x,r,p,d,mu,s) 
  log(like(x,r,p,mu=mu,s=s,post=TRUE)/d) - dt(x,df=3,log=TRUE) 

# rejection sampling for one person
dpv.one = function(resp, ip, n=5, mu, s) {
  cc = !is.na(resp)
  resp = resp[cc]
  ip   = ip[cc,]
  if (length(resp) < 1) return(rep(NA,n))
  d   = integrate(like, lower=-6, upper=6, p=ip, r=resp, mu=mu, s=s, post=TRUE)$value 
  dd  = optimize(f=ddf, c(-6,6), r=resp, p=ip, d=d, mu=mu, s=s, maximum=TRUE)$objective
  pv = rep(0,n)
  k  = 0
  repeat {
    th = rt(1, df=3)
    lf = log(like(th, r=resp, p=ip, mu=mu, s=s, post=TRUE) / d)  
    lg = dt(th, df=3, log=TRUE)
    prob = exp(lf - lg - dd)
    if (runif(1) < prob) {k = k+1; pv[k] = th}
    if (k==n) break
  }
  return(pv)
}

# plausible values by rejection sampling
dpv = function(resp, ip, mu=0, sigma=1, n=5) {
 if (is.null(dim(resp))) dim(resp) = c(1,length(resp))
 if (is.null(dim(ip))) stop("item parameters not a matrix")
 if (nrow(ip) != ncol(resp)) stop("responses - item parameters mismatch")
 np = nrow(resp)
 o = sapply(1:np, function(i) dpv.one(resp=resp[i,], ip=ip, mu=mu, s=sigma, n=n))
 return(t(o)) 
}

# TestGraf style "abilities"  r is the response matrix
qrs = function(resp) {
  raw.scores = apply(resp, 1, sum, na.rm=TRUE)
  ranks = rank(raw.scores, ties.method = "random")
  return(as.matrix(qnorm(ranks/(length(ranks)+1))))
}

# non-parametric plot IRF  
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
    if (bands) segments(br$eval.points,br$lower,br$eval.points,br$upper,co=co)
    lines(br$eval.points,br$estimate,lw=2,co=co)
    if (label) {
      lx = sample(1:length(br$eval.points),1)
      points(br$eval.points[lx],br$estimate[lx],co="white",cex=1.6,pch=19)
      text(br$eval.points[lx],br$estimate[lx],i,co=1,cex=.6)
    }}))  
}

# make normal quadrature points and weights
normal.qu = function(n=15,lower=-4,upper=4,mu=0,sigma=1,scaling="points"){
  if (upper<=lower || sigma<=0 || n<3) stop("bad argument")
  qp=seq(lower,upper,length.out=n)
  if(scaling=="points") {
  	qw=dnorm(qp,0,1)
  	qw=qw/sum(qw)
  	qp=qp*sigma+mu
  } else {
  	qw=dnorm(qp,mu,sigma)
  	qw=qw/sum(qw)
  }
  return(list(quad.points=qp, quad.weights=qw))
}

# from an ICL file, read quadrature points and weights
read.qu.icl = function(file) {
  f=read.table(file,head=FALSE)
  return(list(quad.points=f$V1, quad.weights=f$V2))
}

# from an ICL file, read parameter estimates
read.ip.icl = function(file) {
  return(matrix(scan(file),ncol=4,byrow=TRUE)[,-1])
}

# from a BILOG file, read parameter estimates
read.ip.bilog = function(file) {
  p = read.fwf(file=file, wid=c(8,8,rep(10,13),4,1,1), skip=4, header=FALSE)
  return(cbind(p[,5],p[,7],p[,11]))
}

# read responses from an ascii file 
read.resp = function(file, na=".") {
  return(as.matrix(read.table(file=file, head=FALSE, na=na)))
}

# prepare and run an ICL setup, return parameter estimates
est.icl = function(resp, model, nqp, est.distr, logistic,
  nch, a.prior, b.prior, c.prior, bilog.defaults, run.name) {
  nit = ncol(resp)
  f = paste(run.name,".tcl", sep="")
  d = paste(run.name,".dat", sep="")
  p = paste(run.name,".iclp",sep="")
  write.table(resp,file=d, append=FALSE, sep="", row.names=FALSE, col.names=FALSE, na=".")
  cat("output -log_file",paste(run.name,".iclo",sep=""),"\n",file=f)
  m = switch(model, "1PL"=1, "2PL"=2, "3PL"=3, 4)
  if (m>3) {
    warning(paste("unknown model",model,"using 2PL instead"))
    m = 2
    model = "2PL"
  } 
  cat("set_default_model_dichtomous",model,"\n",file=f,append=TRUE)
  if (!b.prior) 
    cat("options -default_prior_b none\n",file=f,append=TRUE) else
      if (bilog.defaults)
          cat("options -default_prior_b {normal 0.0 2.0}\n",file=f,append=TRUE)
  if (m > 1) {
    if (!a.prior) 
      cat("options -default_prior_a none\n",file=f,append=TRUE) else
        if (bilog.defaults)
            cat("options -default_prior_a {lognormal 0.0 0.5}\n",file=f,append=TRUE)
  }
  if (m > 2) {
    if (!c.prior) 
      cat("options -default_prior_c none\n",file=f,append=TRUE) else
        if (bilog.defaults) {
            prb = 1 /nch  
            cat("options -default_prior_c {beta",20*prb+1,20*(1-prb)+1,"0.0 1.0}\n",file=f,append=TRUE)
        }
  }
  if (logistic) cat("options -D 1.0\n",file=f,append=TRUE)
  cat("allocate_items_dist",nit,"-num_latent_dist_points",nqp,"\n",file=f,append=TRUE)  
  cat("read_examinees",d,paste(nit,"i1",sep=""),"\n",file=f,append=TRUE)
  cat("starting_values_dichotomous\n",file=f,append=TRUE)
  cat("EM_steps -max_iter 2000",file=f,append=TRUE) 
  if (est.distr) cat(" -estim_distr\n",file=f,append=TRUE) else cat("\n",file=f,append=TRUE) 
  cat("write_item_param",p,"\n",file=f,append=TRUE)
  cat("write_latent_dist",paste(run.name,".icld",sep=""),"\n",file=f,append=TRUE)
  cat("release_items_dist\n",file=f,append=TRUE)
  system(paste("icl",f))
  parms = read.ip.icl(p)
  return(parms) 
}

# prepare and run a BILOG setup, return parameter estimates
est.blm = function(resp, model, nqp, est.distr, logistic,
  nch, a.prior, b.prior, c.prior, bilog.defaults, run.name, rasch) {
  nit = ncol(resp)
  if (nit>9999) stop("cannot have more than 9999 items")
  f = paste(run.name,".blm", sep="")
  d = paste(run.name,".dat", sep="")
  p = paste(toupper(run.name),".BLMP",sep="")
  np = nrow(resp)
  if (np>999999) stop("cannot have more than 999999 observations")
  resp = cbind(sprintf("%06d",1:np),resp)
  write.table(resp, file=d, append=FALSE, sep="", row.names=FALSE, col.names=FALSE, na=".", quote=FALSE)
  cat("Running Bilog from R\n\n",file=f)
  m = switch(model, "1PL"=1, "2PL"=2, "3PL"=3, 4)
  if (m>3) {
    warning(paste("unknown model",model,"using 2PL instead"))
    m = 2
    model = "2PL"
  }
  cat(">GLOBAL DFName = '",d,"',\n",sep="",file=f,append=TRUE)
  cat("   NPArm = ",m,",\n",sep="",file=f,append=TRUE) 
  if (logistic) cat("   LOGistic\n",file=f,append=TRUE)
  cat("   SAVE;\n",file=f,append=TRUE) 
  cat(">SAVE PARm = '",p,"';\n",sep="",file=f,append=TRUE)
  cat(">LENGTH NITems = (",nit,");\n",sep="",file=f,append=TRUE)   
  cat(">INPUT NTOtal = ",nit,",\n",sep="",file=f,append=TRUE)
  if (m>2) cat("   NALT = ",nch,",\n",sep="",file=f,append=TRUE)
  if (any(is.na(resp))) {
    cat("   NFName = 'myNF.ile'\n",file=f,append=TRUE)
    cat(paste(rep(".",ncol(resp)),collapse=""),"\n",file="myNF.ile")
  }
  cat("   SAMple = ",np,",\n",sep="",file=f,append=TRUE) 
  cat("   NIDchar = 6;\n",file=f,append=TRUE) 
  ifoo = paste("(ITEM",sprintf("%04d",1),"(1)ITEM",sprintf("%04d",nit),")",sep="")
  cat(">ITEMS INAmes = ",ifoo,";\n",sep="",file=f,append=TRUE)
  cat(">TEST1 TNAme = 'TEST',\n",file=f,append=TRUE)  
  ifoo = paste("(",1,"(1)",nit,")",sep="")
  cat("   INUmber = ",ifoo,";\n",sep="",file=f,append=TRUE) 
  cat("(6A1,",nit,"A1)\n",sep="",file=f,append=TRUE) 
  cat(">CALIB NQPt = ",nqp,",\n",sep="",file=f,append=TRUE)
  if (est.distr) cat("   EMPirical,\n",file=f,append=TRUE) 
  if (m==1 && rasch) cat("   RASCH,\n",file=f,append=TRUE) 
  if (b.prior) cat("   TPRior,\n",file=f,append=TRUE) 
  if (m>1 && !a.prior) cat("   NOSprior,\n",file=f,append=TRUE) 
  if (m>2 && !c.prior) cat("   NOGprior,\n",file=f,append=TRUE) 
  cat("   CYCles = 3000,\n",sep="",file=f,append=TRUE)
  cat("   NEWton = 0;\n",file=f,append=TRUE)
  if (Sys.info()["sysname"]=="Linux") {
  	system(paste("wine","BLM1.EXE",run.name))
  	system(paste("wine","BLM2.EXE",run.name))
  	system(paste("wine","BLM3.EXE",run.name))
  } else {
  	system(paste("blm1",run.name))
  	system(paste("blm2",run.name))
  	system(paste("blm3",run.name))
  }
  parms = read.ip.bilog(p)
  return(parms)  
}

# prepare and run an LTM setup, return parameter estimates
est.ltm = function(resp, model, nqp, logistic, rasch) {
  library(ltm)
  nit = ncol(resp)
  switch(model,
    "1PL" = {
      constr = if (rasch) rbind(c(nit+1, 1)) else NULL
      m = rasch(resp, constraint=constr, control = list(GHk = nqp))},
    "2PL" = {m = ltm(resp ~ z1, control = list(GHk = nqp))},
    "3PL" = {m = tpm(resp, control = list(GHk = nqp), max.guessing=1)}, 
    stop(paste("model",model,"not supported in ltm"))
  )
  p = coef(m)
  if (!logistic) p[,2] = p[,2]/1.7
  p = if(model=="3PL") cbind(p[,3], p[,2], p[,1]) else
      cbind(p[,2],p[,1],rep(0,nit))      
  return(p) 
}

# estimate item parameters via ICL, BILOG, or ltm
est = function(resp, model="2PL", engine="icl", nqp=20, est.distr=FALSE,
  logistic=TRUE, nch=5, a.prior=TRUE, b.prior=FALSE, c.prior=TRUE, 
  bilog.defaults=TRUE, rasch=FALSE, run.name="mymodel") {
  res = switch(engine,
    "icl"=  est.icl(resp, model, nqp, est.distr, logistic, nch, a.prior, b.prior, c.prior, bilog.defaults, run.name),
    "bilog"=est.blm(resp, model, nqp, est.distr, logistic, nch, a.prior, b.prior, c.prior, bilog.defaults, run.name, rasch),
    "ltm"=  est.ltm(resp, model, nqp, logistic, rasch),
    {
      warning(paste("unknown engine",engine,"using icl instead"))
      est.icl(resp,model, nqp, est.distr, logistic, nch, a.prior, b.prior, c.prior, bilog.defaults, run.name)      
    }
  )
  return(res)
}   

# MS or MM linear scaling, return transformation
simple.scale = function(sp, np, mm=FALSE) {
  A = if (mm) mean(np[,1])/mean(sp[,1]) else sd(sp[,2])/sd(np[,2])
  B = mean(sp[,2]) - A*mean(np[,2])
  return(list(A=A,B=B))
}

# function optimised in Lord-Stocking scaling
sl = function (x, sp, np, qp, qw) {
  A = x[1]
  B = x[2]
  np[,1] = np[,1]/A
  np[,2] = np[,2]*A + B
  dif = trf(ip=sp,x=qp)$f - trf(ip=np,x=qp)$f
  return(sum(dif*dif*qw))
}

# function optimised in Haebara scaling
hb = function (x, sp, np, qp, qw) {
  A = x[1]
  B = x[2]
  np[,1] = np[,1]/A
  np[,2] = np[,2]*A + B
  dif = irf(ip=sp,x=qp)$f - irf(ip=np,x=qp)$f
  return(sum(dif*dif*qw))
}

# versions with correction for back-equating
sl2 <- function (x, sp, np, qp, qw){
    A21 = x[1]
    K21 = x[2]
    A12 = 1/A21
    K12 = -K21/A21
    s = length(qp)/2
    np21 = np
    sp12 = sp
    np21[, 1] = np21[, 1]/A21
    np21[, 2] = np21[, 2]*A21 + K21
    Q1 <- trf(ip = sp, x = qp[1:s])$f -
          trf(ip = np21, x = qp[1:s])$f
    sp12[, 1] = sp12[, 1]/A12
    sp12[, 2] = sp12[, 2]*A12 + K12
    Q2 <- trf(ip = sp12, x = qp[(s+1):(2*s)])$f -
          trf(ip = np, x = qp[(s+1):(2*s)])$f
    dif <- c(Q1, Q2)
    return(sum(dif * dif * qw))
}

hb2 <- function (x, sp, np, qp, qw){
    A21 = x[1]
    K21 = x[2]
    A12 = 1/A21
    K12 = -K21/A21
    s = length(qp)/2
    np21 = np
    sp12 = sp
    np21[, 1] = np21[, 1]/A21
    np21[, 2] = np21[, 2]*A21 + K21
    Q1 <- irf(ip = sp, x = qp[1:s])$f -
          irf(ip = np21, x = qp[1:s])$f
    sp12[, 1] = sp12[, 1]/A12
    sp12[, 2] = sp12[, 2]*A12 + K12
    Q2 <- irf(ip = sp12, x = qp[(s+1):(2*s)])$f -
          irf(ip = np, x = qp[(s+1):(2*s)])$f
    dif <- rbind(Q1, Q2)
    return(sum(dif * dif * qw))
}

# do Lord-Stocking or Haebara scaling, return transformation
adv.scale = function(sp,np,sq=NULL,nq=NULL,haeb=FALSE,bec=FALSE) {
  if (is.null(sq)) stop("no quadrature for characteristic curve method")
  if (is.null(nq) && haeb) stop("Haebara method needs both old and new quadrature")
  qp = if (haeb) c(sq$quad.points,  nq$quad.points)  else sq$quad.points
  qw = if (haeb) c(sq$quad.weights, nq$quad.weights) else sq$quad.weights
  if (bec) {
    r  = if (haeb) optim(c(1,0),hb2,method="BFGS",sp=sp,np=np,qp=qp,qw=qw) else
      optim(c(1,0),sl2,method="BFGS",sp=sp,np=np,qp=qp,qw=qw)    
  } else {
    r  = if (haeb) optim(c(1,0),hb,method="BFGS",sp=sp,np=np,qp=qp,qw=qw) else
      optim(c(1,0),sl,method="BFGS",sp=sp,np=np,qp=qp,qw=qw)    
  }	  
  return(list(A=r$par[1],B=r$par[2])) 
}

# do one of four scaling methods, return transformation and scaled new parms
sca = function(old.ip, new.ip, old.items, new.items,
  old.qu=NULL, new.qu=NULL, method="MS", bec=FALSE) {
  if (length(old.items)  != length(new.items)) stop("no of common items does not match")
  if (!all(old.items %in% 1:nrow(old.ip))) stop("bad index for some scaled item")
  if (!all(new.items %in% 1:nrow(new.ip))) stop("bad index for some new item")
  sp = old.ip[old.items, ]
  np = new.ip[new.items, ]
  r = switch(method,
    "MS"= simple.scale(sp,np,mm=FALSE),
    "MM"= simple.scale(sp,np,mm=TRUE),
    "HB"= adv.scale(sp,np,old.qu,new.qu,haeb=TRUE,bec=bec),
    "SL"= adv.scale(sp,np,old.qu,haeb=FALSE,bec=bec),
    stop(paste("unknown scaling method",method))
  )
  new.ip[,1] = new.ip[,1] / r$A
  new.ip[,2] = new.ip[,2] * r$A + r$B   
  return(list(slope=r$A, intercept=r$B, scaled.ip=new.ip))
}

# bin ability estimates for item fit stats
grp = function(theta, bins=9, breaks=NULL, equal="count", type="meds") {
  if (!is.null(dim(theta))) theta = theta[,1]
  if (is.null(breaks)) {
    if (equal=="count") {
      qu = seq(0, 1, 1/bins)
      breaks=quantile(theta, probs=qu, names=FALSE, na.rm=TRUE)
    } else {
      r = range(theta)
      st = (r[2]-r[1])/bins
      breaks = seq(r[1], r[2], by=st)
    }
  }
  grmemb = findInterval(theta, breaks, right=TRUE)
  counts = table(grmemb)
  ref = switch(type,
    mids = {
      dd = diff(breaks,1)
      md = mean(dd)/2
      c(breaks[1]-md, breaks[-length(breaks)]+dd/2, breaks[length(breaks)]+md)
    },
    meds = tapply(theta, grmemb, median),
    means= tapply(theta, grmemb, mean)
  )
  return(list(breaks=breaks,counts=counts,ref=ref))
}

# compute an item fit statistic with df and pval.
# Optionally plot the IRF with residuals shown
itf = function(resp, ip, item, stat = "lr", theta, groups,
  standardize=TRUE, mu=0, sigma=1, do.plot=TRUE, main="Item fit") {
# expected prop of correct answers at the mids of the histogram
  if (missing(theta)) theta = eap(resp, ip, normal.qu())
  if (missing(groups)) {
    groups = switch(stat,
      "chi" = grp(theta),
      "lr"  = grp(theta, type="means"),
      stop("unknown statistic")
    )
  }
  if (nrow(ip)<20) warning("item fit statistic computed for a test of less than 20 items")
  if (!(item %in% 1:nrow(ip))) stop("bad item number")
  pa = ip[item,]
  iresp = resp[,item]
  oo = !is.na(iresp)
  iresp = iresp[oo]
  theta = theta[oo,]
  if (standardize) theta = scale(theta)*sigma + mu
  ep = as.vector(irf(ip=pa, x=groups$ref)$f)
  nn = groups$counts
# observed prop of correct answers in each bin
  gr =  findInterval(theta[,1], groups$breaks, right=TRUE) * iresp
  rg = tabulate(gr[gr>0], nbins=length(nn))
  op = rg / nn
# calc number of parameters
  npar = 3 - (pa[3]==0) - (pa[1]==1)
  if (stat=="chi") tst = sum((op-ep)^2/(ep*(1-ep))*nn)
  else {
    cmp = rg*log(op/ep)+(nn-rg)*log((1-op)/(1-ep))
    tst = 2*sum(cmp[is.finite(cmp)])
  }
  dfr = sum(nn>0) - npar
  pval=pchisq(tst,dfr,lower.tail=FALSE)
  if (do.plot) {
    ifit = paste("Q = ",round(tst,2),"    d.f. = ",dfr,"    P = ",round(pval,4),sep="")
    plot(c(-4,4),c(0,1),main="",xlab="Ability",ylab="Proportion right",type="n")
    title(main=main,mtext(ifit,line=0.5,cex=0.7))
    points(groups$ref, op)
    plot(irf(pa),add=TRUE)
  }
  return(list(statistic=tst,dfr=dfr,pval=pval))
}

# feeble attempt at coercing a list of list of lists... to a matrix
l2m = function(x) {
  unx = unlist(x)
  m = NULL
  nam = unique(names(unx))
  for (i in nam) m = cbind(m,unx[names(unx)==i])
  rownames(m) = NULL
  colnames(m) = nam
  return(m)
}

# score a test, replace NA with 0
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

tgp = function(choices, key, item, 
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

api = function(resp, ip){
  th = mlebme(resp, ip)[,1]
  p = irf(ip, th)$f
	p = pmax(p, .00001); pr = pmin(p, .99999)
  if(is.null(dim(p))) p=matrix(p,ncol=length(p))
  q = 1 - p
  lp= log(p)
  lq= log(q)
  l = apply(lp*resp + lq*(1-resp), 1, sum)
  m = apply(p*lp+q*lq, 1, sum)
  v = apply(p*q*(log(p/q))^2, 1, sum)
  (l-m)/sqrt(v)
}

tsc = function(ip, theta){
   p = irf(ip, theta[,1])$f
   if (is.null(dim(ip))) dim(ip) = c(1,3)
   if (is.null(dim(p)))  p = matrix(p, ncol=1)
   sc = apply(p, 1, sum)
   aq = sweep(1-p, 2, ip[,1], "*")
   if(any(ip[,3]!=0)) {
      p = sweep(p, 2, ip[,3],  "-")
      p = sweep(p, 2, 1-ip[,3],"/")
   }
   jb = apply(aq*p, 1, sum)
   se = jb*theta[,2]
   cbind(sc,se)
}

scp = function(resp, ip, theta=NULL) {
  if (is.null(theta)) theta=mlebme(resp,ip)
	or = order(theta[,1])
	theta = theta[or,]
  ts = tsc(ip, theta)
	os = apply(resp, 1, sum, na.rm=TRUE)
	os = os[or]
	plot(theta[,1], ts[,1], type="l", lwd = 2, xlab="Estimated ability", ylab="Score",
		main="Observed and predicted scores")
	points(theta[,1], ts[,1]-ts[,2], type="l")
	points(theta[,1], ts[,1]+ts[,2], type="l")
	points(theta[,1], os, co="red")
}  
