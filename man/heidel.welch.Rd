% File src/library/base/man/load.Rd
     \name{heidel.welch}
     \alias{heidel.welch}
     \title{Heidelberger and Welch Diagnostic for MCMC}
     \description{This function takes a matrix of MCMC iterations and conducts a Heidelberger and Welch convergence diagnostic on each column.}
     \usage{
heidel.welch(X,pvalue=0.05)
     }
     \arguments{
        \item{X}{An object of the \code{matrix} class with results from an MCMC algorithm. Each column should designate a parameter, and each row should designate an iteration.}
          \item{pvalue}{Alpha level for significance tests. Defaults to 0.05.}
     }
     \details{
     This is an adaptation of a function in Plummer et al.'s \code{coda} package. Heidelberger and Welch's (1993) test for nonconvergence. This version of the diagnostic only reports a Cramer-von Mises test and its corresponding p-value to determine if the chain is weakly stationary with comparisons of early portions of the chain to the end of the chain.
     }
     \value{
Returns a \code{matrix} in which the first row consists of the values of the Cramer-von Mises test statistic for each parameter, and the second row consists of the corresponding p-values. Each column of the matrix represents another parameter of interest. A significant result serves as evidence of nonconvergence, so non-significant results are desired.
     }     
     \references{
	Philip Heidelberger and Peter D. Welch. 1993. "Simulation Run Length Control in the Presence of an Initial Transient." \emph{Operations Research} 31:1109-1144.
	
	Martyn Plummer, Nicky Best, Kate Cowles and Karen Vines. 2006. "CODA: Convergence Diagnosis and Output Analysis for MCMC." \emph{R News} 6:7-11.
     }

     \examples{
#Examine Data
summary(ContrivedData)

#Initial OLS Model
contrived.ols<-lm(y~x.1+x.2,data=ContrivedData);summary(contrived.ols)

#Define Covariate Matrix
covariates<-cbind(1,ContrivedData$x.1,ContrivedData$x.2)

#set seed
set.seed(1241060320)

#For simple illustration, we set iterations to 10.
#In this case, a 10,000-iteration run converges to the true parameters.
#If you have considerable time and hardware, delete the # on the next line.
#10,000 iterations took 39 min. with 8 GB RAM & a 1.5 GHz Quad-Core processor.
M<-10#000

#Run the Full Model
contrived.run<-metropolis.krige(y=ContrivedData$y,X=covariates,range.tol=0.05,
	east=ContrivedData$s.1,north=ContrivedData$s.2,mcmc.samples=M)

#Delete 20\% for Burn-In
contrived.run<-contrived.run[(ceiling(0.2*M)+1):M,]

#examine results against true coefficients	
TRUTH<-c(0.5,2.5,0.5,0,1,2)
rbind(apply(contrived.run,2,quantile,c(.5,.05,.95)),TRUTH)

#Convergence Diagnostic: Heidelberger-Welch
heidel.welch(contrived.run)
}
\keyword{convergence}
\keyword{diagnostic}
