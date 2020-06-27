# CREATING THE LOG-POSTERIOR OBJECTIVE FUNCTION
krige.posterior<-function(tau2,phi,sigma2,beta,y,X,east,north,semivar.exp=2,
  p.spatial.share=0.5,p.range.share=0.5,p.range.tol=0.05,p.beta.var=10,
  tot.var=var(y),local.Sigma=NULL,distance=NULL,max.distance=NULL,checks = TRUE){

  if(checks==TRUE){
  if(any(is.na(y))) stop("missing values in 'y' ")
	if(TRUE %in% apply(X, 2, function(x) any(is.na(x)))) stop("missing values in 'X' ")
	if(any(is.na(east))) stop("missing values in 'east' ")
	if(any(is.na(north))) stop("missing values in 'north' ")
  if(semivar.exp<=0 | semivar.exp>2) stop("semivar.exp must be greater than 0 and less than or equal to 2.")
  if(p.spatial.share<=0 | p.spatial.share>=1) stop("p.spatial.share must be between 0 and 1.")
  if(p.range.share<=0) stop("p.range.share must be greater than 0.")
  if(p.range.tol<=0 | p.range.tol>=1) stop("p.range.tol must be between 0 and 1.")
  if(p.beta.var<=0) stop("p.beta.var must be greater than 0.")
}

  if(is.null(distance)) distance<-k_distmat(cbind(east,north))
  if(is.null(max.distance)) max.distance<-max(distance)
  if(is.null(local.Sigma)){
    Sigma<-ifelse(distance>0, sigma2*exp(-abs(phi*distance)^semivar.exp), tau2+sigma2)
  } else Sigma<-local.Sigma

  mu<-X%*%beta
  log.det.Sigma<-sum(log(k_eigenvalue(Sigma)))
  logpost<- -(1/2)*log.det.Sigma-0.5*t(y-mu)%*%k_inv(Sigma)%*%(y-mu)-
     t(beta)%*%beta/(2*p.beta.var)-log(sigma2)*(1+2*p.spatial.share)/p.spatial.share-tot.var/sigma2+
     log(tau2)*(2*p.spatial.share-3)/(1-p.spatial.share)-tot.var/tau2-
     log(1/phi)*(1+2*p.range.share)/p.range.share-max.distance*phi/((-log(p.range.tol))^(1/semivar.exp))
  return(logpost)
}
