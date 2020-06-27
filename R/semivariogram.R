#Semivariogram function from Banerjee, Carlin, and Gelfand (2015, p. 24, Eq. 2.1)
semivariogram<-function(x,east,north,bins=13,draw.plot=TRUE){
	distance<-as.numeric(dist(cbind(east,north)))
	differences<-as.numeric(dist(x))
	my.breaks<-seq(0,max(distance),length=bins+1)
	categories<-cut(distance,breaks=my.breaks)
	semivar.calc<-function(x){0.5*mean(x^2)}
	semivariances<-as.numeric(by(differences,INDICES=categories,FUN=semivar.calc))
	names(semivariances)<-my.breaks[-1]
	my.digits<-ifelse(max(my.breaks)<10,2,0)
	my.labels<-round(my.breaks[-1],my.digits)
	if(draw.plot==TRUE){
		plot(y=semivariances,x=1:bins,xlab="Distance",ylab="Semivariance",xlim=c(0,bins),ylim=c(0,max(semivariances)),axes=F)
		axis(1,at=1:bins,labels=my.labels,cex.axis=.75);axis(2);box()
		}
	return(semivariances)
}
