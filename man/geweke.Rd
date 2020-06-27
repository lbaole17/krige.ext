% File src/library/base/man/load.Rd
     \name{geweke}
     \alias{geweke}
     \title{Geweke Diagnostic for MCMC}
     \description{This function takes a matrix of MCMC iterations and conducts a Geweke convergence diagnostic on each column.}
     \usage{
geweke(X,early.prop=.1,late.prop=.5,precision=4)
     }
     \arguments{
        \item{X}{An object of the \code{matrix} class with results from an MCMC algorithm. Each column should designate a parameter, and each row should designate an iteration.}
         \item{early.prop}{Proportion of iterations to use from the start of the chain.}
          \item{late.prop}{Proportion of iterations to use from the end of the chain.}
           \item{precision}{Number of digits of test statistics and p-values to print.}
     }
     \details{
     Geweke's (1992) test for nonconvergence of a MCMC chain is to conduct a difference-of-means test that compares the mean early in the chain to the mean late in the chain. If the means are significantly different from each other, then this is evidence that the chain has not converged. The difference-of-means test is a simple z-ratio, though the standard error is estimated using the spectral density at zero to account for autocorrelation in the chain.
     }
     \value{
Returns a \code{matrix} in which the first row consists of z-scores for tests of equal means for the first and last parts of the chain. The second row consists of the corresponding p-values. Each column of the matrix represents another parameter. For each column, a significant result is evidence that the chain has not converged for that parameter. Thus, a non-significant result is desired.
     }     
     \references{
	John Geweke. 1992. "Evaluating the Accuracy of Sampling-Based Approaches to the Calculation of Posterior Moments." In \emph{Bayesian Statistics 4}, ed. J.M. Bernardo, J.O. Berger, A.P. Dawid, and A.F.M. Smith. Oxford: Clarendon Press.
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

#Convergence Diagnostic: Geweke 
#Note that the second (commented) version of Geweke is more typical
#of a 10,000 iteration run.
geweke(contrived.run,early.prop=0.5)
#geweke(contrived.run)
}
\keyword{convergence}
\keyword{diagnostic}
