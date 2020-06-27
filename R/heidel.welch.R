#code adapted from the heidel.diag() function in the coda package
heidel.welch<-function(X,pvalue=0.05){
    if(!is.matrix(X)) stop("Input X must be of the matrix class.")
    HW.mat <- matrix(0,ncol=ncol(X),nrow=2)
    dimnames(HW.mat) <- list(c("CramerVonMises", "p-value"),colnames(X))
    pcramer<-function(q, eps = 1e-05) {
    		log.eps <- log(eps)
    		y <- matrix(0, nrow = 4, ncol = length(q))
    			for (k in 0:3) {
        			z <- gamma(k + 0.5) * sqrt(4 * k + 1)/(gamma(k + 1)*pi^(3/2) * sqrt(q))
        			u <- (4 * k + 1)^2/(16 * q)
        			y[k + 1, ] <- ifelse(u > -log.eps, 0, z * exp(-u) * besselK(x = u,nu = 1/4))
    				}
    		return(apply(y, 2, sum))
		}
    
    for (j in 1:ncol(X)) {
        start.vec <- seq(from = start(X)[1], to = end(X)[1]/2, by = nrow(X)/10)
        Y <- X[, j, drop = TRUE]
        n1 <- length(Y)
        first.ar<-ar(window(Y, start = end(Y)/2),aic=T)
        S0<-first.ar$var.pred/(1-sum(first.ar$ar))^2
        converged <- FALSE
        for (i in seq(along = start.vec)) {
            Y <- window(Y, start = start.vec[i])
            n <- length(Y)
            ybar <- mean(Y)
            B <- cumsum(Y)-ybar*(1:n)
            Bsq <- (B*B)/(n*S0)
            I <- sum(Bsq)/n
            if (converged <- !is.na(I) && pcramer(I) < 1 - pvalue) 
                break
        }
        HW.mat[,j] <- c(I, 1 - pcramer(I))
    }
    return(HW.mat)
}
