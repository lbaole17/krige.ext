#FUNCTION TO RETURN VALUES OF A PARAMETRIC POWERED EXPONENTIAL SEMIVARIOGRAM#
exponential.semivariogram<-function(nugget,decay,partial.sill,distance,power=2){
	semivariance<-nugget+partial.sill*(1-exp(-abs(decay*distance)^power))
	return(semivariance)
	}