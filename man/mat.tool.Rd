% File src/library/base/man/load.Rd
     \name{mat.tool}
     \alias{mat.tool}
     \alias{stop.mat.tool}
     \title{Managing the Matrix Product of R}
     \description{
       This function provides a tool to manage the matrix products (BLAS/LAPACK) used by R
     }
     \usage{
       mat.tool(proceed = NULL, engine = NULL, path = NULL)
       stop.mat.tool(proceed = NULL)
     }
     \arguments{
        \item{proceed}{A character string indicating whether to proceedm. Use "Yes", "Y", "yes", "y" to proceed; otherwise, stop.}
         \item{engine}{A character string specifying the engine will be used. "MKL" for Intel MKL; "OpenBLAS" for OpenBLAS}
          \item{path}{The location of the MKL or OpenBLAS dynamic library. Only for Mac OS.}
     }
     \details{
      This function provides a quick tool to symlink R with highly optimized BLAS/LAPACK library. The current version supports Intel MKL and OpenBLAS, which should be installed before using this function. The optimized BLAS/LAPACK can utilize the multi-core modern computing architecture to improve the math computing performance of R.

      The function can automatically detect if the target library is installed and activate them as long as the library is installed at the default location. If there are more than one library is installed, it will use Intel MKL first, which is more optimized for Intel CPU. The user can also specify the \code{engine} or provide the \code{path}.
     }
     \references{
     R Development Core Team. 2020. \emph{R installation and Administration Manual}. Version 4.0.0, URL \url{https://cran.r-project.org/doc/manuals/R-admin.html}.

     Wang, Endong, et al. 2014. \emph{Intel Math Kernel Library." High-Performance Computing on the Intel(R) Xeon Phi(TM)}. Springer, Cham. 167-188.

     Xianyi, Zhang, Wang Qian, and Zhang Yunquan. 2012. "Model-driven level 3 BLAS performance optimization on Loongson 3A processor." 2012 IEEE 18th International Conference on Parallel and Distributed Systems. IEEE.
     }

     \examples{
\dontrun{
mat.tool()
# Restart the R session to activate the change.

# Use the same example as  in the metropolis.krige()
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

#Convergence Diagnostics: Geweke and Heidelberger-Welch
#Note that the second (commented) version of Geweke is more typical
#of a 10,000 iteration run.
geweke(contrived.run,early.prop=0.5)
#geweke(contrived.run)
heidel.welch(contrived.run)

#Examine the Parametric Semivariogram Graphically
raw.semivar<-semivariogram(x=ContrivedData$y,east=ContrivedData$s.1,
     north=ContrivedData$s.2)
resid.semivar<-semivariogram(x=contrived.ols$residuals,east=ContrivedData$s.1,
     north=ContrivedData$s.2,draw.plot=FALSE)
points(resid.semivar,pch=3,col='blue')
lines(exponential.semivariogram(nugget=median(contrived.run[,"tau2"]),
     decay=median(contrived.run[,"phi"]),partial.sill=median(contrived.run[,"sigma2"]),
     distance=as.numeric(names(resid.semivar))),col='red')

#Predictive Data for Three Hypothetical people
euler<-c(1,0.2,0.7)
archimedes<-c(1,0.3,0.1)
pythagoras<-c(1,0.1,0.4)
mathematicians<-rbind(euler,archimedes,pythagoras)
basel<-c(0.1,0.8)
sicily<-c(0.4,0.1)
samos<-c(0.1,0.4)
new.locations<-rbind(basel,sicily,samos)
colnames(new.locations)<-c("eastings","northings")

# Make predictions from median parameter values:
median.pred<-krige.pred(pred.x=mathematicians,
    pred.east=new.locations[,"eastings"],pred.north=new.locations[,"northings"],
    train.y=ContrivedData$y,train.x=covariates,train.east=ContrivedData$s.1,
    train.north=ContrivedData$s.2,mcmc.iter=contrived.run)
median.pred

# Make predictions with 90\% credible intervals:
cred.pred<-krige.pred(pred.x=mathematicians,
    pred.east=new.locations[,"eastings"],pred.north=new.locations[,"northings"],
    train.y=ContrivedData$y,train.x=covariates,train.east=ContrivedData$s.1,
    train.north=ContrivedData$s.2,mcmc.iter=contrived.run,credible=0.9)
cred.pred

stop.mat.tool()
# Restart the R session to activate the change.
}
}
\keyword{matrix}
