# GEWEKE DIAGNOSTIC FUNCTION
geweke<-function(X,early.prop=.1,late.prop=.5,precision=4){ 
  if(!is.matrix(X)) stop("Input X must be of the matrix class.")
  if(early.prop<0 || early.prop>1) stop("early.prop must be between 0 and 1.")
  if(late.prop<0 || late.prop>1) stop("late.prop must be between 0 and 1.")
  if(early.prop+late.prop>1) stop("The sum of early.prop+late.prop must be less than 1.")
  if(precision<=2) stop("precision must be 2 or larger.")
  iter<-dim(X)[1]
  K<-dim(X)[2]
  early<-c(1:ceiling(1+early.prop*(iter-1)))
  late<-c(floor(iter-late.prop*(iter-1)):iter)
  n.early<-length(early)
  n.late<-length(late)
  geweke.vec<-rep(NA,K)
  for(k in 1:K){
    early.ar<-ar(X[early,k],aic=T)
    early.var<-early.ar$var.pred/(1-sum(early.ar$ar))^2
    late.ar<-ar(X[late,k],aic=T)
    late.var<-late.ar$var.pred/(1-sum(late.ar$ar))^2
    geweke.vec[k]<-(mean(X[early,k])-mean(X[late,k]))/sqrt((early.var/n.early)+(late.var/n.late))
    }
  geweke.mat<-rbind(round(geweke.vec,precision),round(2*(1-pnorm(abs(geweke.vec))),precision))
  colnames(geweke.mat)<-colnames(X)
  row.names(geweke.mat)<-c("z-ratio","p-value")
  return(geweke.mat)
}
