#Kriging prediction function from Banerjee et al. (2015, p. 169, Eq. 7.5):
krige.pred<-function(pred.x,pred.east,pred.north,train.y,train.x,train.east,train.north,mcmc.iter,powered.exp=2,credible=NULL,inv.Sigma=NULL){
	if (is.matrix(pred.x) && TRUE %in% apply(pred.x, 2, function(x) any(is.na(x)))) stop("missing values in 'pred.x'")
  if (any(is.na(pred.east))) stop("missing values in 'pred.east'")
  if (any(is.na(pred.north))) stop("missing values in 'pred.north'")
  if (any(is.na(train.y))) stop("missing values in 'train.y'")
  if (TRUE %in% apply(train.x, 2, function(x) any(is.na(x)))) stop("missing values in 'train.x'")
  if (any(is.na(train.east))) stop("missing values in 'train.east'")
  if (any(is.na(train.north))) stop("missing values in 'train.north'")
	if(nrow(train.x)!=length(train.y)) stop("Number of observation in train.x and train.y must be equal.")
	if((ncol(train.x)+3)!=ncol(mcmc.iter)) stop("The number of columns in mcmc.iter should equal 3+ncol(train.x).")
	if(!is.null(credible) & (credible<0 || credible>1)) stop("credible must be between 0 and 1, or NULL if no credible interval desired.")
	if(!is.null(credible) & !is.null(inv.Sigma)) message("User-provided value of inv.Sigma is ignored when a credible interval is predicted.")
	if(!is.null(inv.Sigma)) if(nrow(inv.Sigma)!=length(train.y)){
		message("User-provided value of inv.Sigma is non-conformable with training data. Function-generated value will be used instead.")
		inv.Sigma<-NULL
		}
	train.coord<-cbind(train.east,train.north)
	test.coord<-cbind(pred.east,pred.north)
	all.dist<-k_distmat(rbind(test.coord,train.coord))
	train.dist<-all.dist[-c(1:nrow(test.coord)),-c(1:nrow(test.coord))]
	pred.dist<-all.dist[1:nrow(test.coord),-c(1:nrow(test.coord))]

 if(is.null(credible)){
 	if(dim(mcmc.iter)[1]==1){
			beta<-mcmc.iter[,-c(1:3)]
			tau2<-mcmc.iter[,1]
			phi<-mcmc.iter[,2]
			sigma2<-mcmc.iter[,3]
 		}else{
			beta<-apply(mcmc.iter[,-c(1:3)],2,median)
			tau2<-median(mcmc.iter[,1])
			phi<-median(mcmc.iter[,2])
			sigma2<-median(mcmc.iter[,3])
		}
	if(is.null(inv.Sigma)) {
		Sigma<-ifelse(train.dist>0, sigma2*exp(-abs(phi*train.dist)^powered.exp), tau2+sigma2)
		inv.Sigma<-k_inv(Sigma)
		}
	smoother<-sigma2*exp(-abs(phi*pred.dist)^powered.exp)
	resid<-train.y-train.x%*%beta
	forecast<-pred.x%*%beta+smoother%*%inv.Sigma%*%resid
	} else{
		no.pred<-ifelse(is.matrix(pred.x),nrow(pred.x),1)
		forecast.iter<-matrix(NA,nrow=no.pred,ncol=nrow(mcmc.iter))
		for(i in 1:nrow(mcmc.iter)){
			beta<-mcmc.iter[i,-c(1:3)]
			tau2<-mcmc.iter[i,1]
			phi<-mcmc.iter[i,2]
			sigma2<-mcmc.iter[i,3]
			Sigma<-ifelse(train.dist>0, sigma2*exp(-abs(phi*train.dist)^powered.exp), tau2+sigma2)
			smoother<-sigma2*exp(-abs(phi*pred.dist)^powered.exp)
			resid<-train.y-train.x%*%beta
			forecast.iter[,i]<-pred.x%*%beta+smoother%*%k_inv(Sigma)%*%resid
		}
		tail<-(1-credible)/2
		forecast<-t(apply(forecast.iter,1,quantile,c(.5,tail,1-tail)))
		colnames(forecast)<-c("Estimate","Lower C.I.","Upper C.I.")
		rownames(forecast)<-rownames(pred.x)
		}
	return(forecast)
}
