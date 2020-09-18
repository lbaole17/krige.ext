% File src/library/base/man/load.Rd
     \name{metropolis.krige}
     \alias{metropolis.krige}
     \title{Sampling Technique Using Metropolis-Hastings}
     \description{
       This function performs Metropolis-Hastings sampling for a linear model specified over point-referenced geospatial data. It returns MCMC iterations, with which results of the geospatial linear model can be summarized.
     }
     \usage{
       metropolis.krige(y,X,east,north,powered.exp=2,mcmc.samples=100,
            spatial.share=0.5,range.share=0.5,beta.var=10,
            range.tol=0.05,b.tune=1.0,nugget.tune=10.0,psill.tune=1.0,
            message=NULL, save=NULL, time=NULL, checks=TRUE)
     }
     \arguments{
            \item{y}{The dependent variable that is used in the kriging model.}
             \item{X}{The matrix of independent variables used in the kriging model.}
              \item{east}{Vector of eastings for all observations.}
               \item{north}{Vector of northings for all observations.}
               \item{powered.exp}{This exponent, which must be greater than 0 and less than or equal to 2, specifies a powered exponential correlation structure for the data. One widely used specification is setting this to 1, which yields an exponential correlation structure. Another common specification is setting this to 2 (the default), which yields a Gaussian correlation structure.}
               \item{mcmc.samples}{Number of MCMC iterations.}
               \item{spatial.share}{Prior for proportion of unexplained variance that is spatial in nature. Must be greater than 0 and less than 1. Defaults to an even split, valued at 0.5.}
               \item{range.share}{Prior for the effective range term, as a proportion of the maximum distance in the data. Users should choose the proportion of distance at which they think the spatial correlation will become negligible. Must be greater than 0. Values greater than 1 are permitted, but users should recognize that this implies that meaningful spatial correlation would persist outside of the convex hull of data. Defaults to half the maximum distance, valued at 0.5.}
               \item{beta.var}{Prior for the variance on zero-meaned normal priors on the regression coefficients. Must be greater than 0. Defaults to 10.}
               \item{range.tol}{Tolerance term for setting the effective range. At the distance where the spatial correlation drops below this term, it is judged that the effective range has been met. The default value is the commonly-used 0.05. Must be greater than 0 and less than 1.}
               \item{b.tune}{Tuning parameter for candidate generation of regression coefficients that must be greater than 0. A value of 1 means that draws will be based on the variance-covariance matrix of coefficients from OLS. Larger steps are taken for values greater than 1, and smaller steps are taken for values from 0 to 1. Defaults to 1.0.}
               \item{nugget.tune}{Tuning parameter for candidate generation of the nugget term (\code{tau2}) that must be greater than 0. A value of 1 means that draws will be based on the typical variance of an inverse gamma distribution. \emph{Smaller} steps are taken for values \emph{greater} than 1, and \emph{larger} steps are taken for \emph{decimal} values from 0 to 1. Defaults to 10.0.}
               \item{psill.tune}{Tuning parameter for candidate generation of the partial sill term (\code{sigma2}) that must be greater than 0. A value of 1 means that draws will be based on the typical variance of an inverse gamma distribution. \emph{Smaller} steps are taken for values \emph{greater} than 1, and \emph{larger} steps are taken for \emph{decimal} values from 0 to 1. Defaults to 1.0.}
               \item{message}{Experimental feature: An integer that specifies the frequency of the reporting of the acceptance rates in progress.}
               \item{save}{Experimental feature: An integer that specifies the frequency to save the iterations in progress.}
               \item{time}{Experimental feature: A vector that specifies when to report the progress and time remained.}
               \item{checks}{Experimental feature: A logical variable that defines whether to check NA and comformity of parameters.}
             }
     \details{
      Analysts should use this function if they want to estimate a linear regression model in which each observation can be located at points in geographic space. That is, each observation is observed for a set of coordinates in eastings & northings or longitude & latitude.

      Researchers must specify their model in the following manner: \code{y} should be a column vector of the dependent variable. \code{X} should be a matrix that includes all independent variables in the model, including a constant vector to estimate an intercept term. \code{east} should be a vector of all west-east coordinates for observations (ideally eastings but possibly longitude). \code{north} should be a vector of all north-south coordinates for observations (ideally northings but possibly latitude). \code{mcmc.samples} is the number of iterations to sample from the posterior distribution using the Metropolis-Hastings algorithm. This defaults to 100 iterations, but many more iterations would normally be preferred. The output of the function prints the proportion of candidate values for the coefficients and for the variance terms accepted by the Metropolis-Hastings algorithm. Particularly low or high acceptance rates respectively may indicate slow mixing (requiring more iterations) or a transient state (leading to nonconvergence), so additional messages will print for extreme acceptance rates. Users may want to adjust the tuning parameters \code{b.tune}, \code{nugget.tune}, or \code{psill.tune}, or perhaps the tolerance parameter \code{range.tol} if the acceptance rate is too high or too low.

      Since MCMC calculations are typically long, the experimental monitor functions allow users to track the progress of MCMC simulations and automatically save the temporary output data. \code{message} defines the frequency (as iterations) of the reporting of the acceptance rates; \code{save} defines the frequency to automatically save the temporary data; \code{time} defines when to report the remaining time; \code{checks} is a logical variable defining whether to run all the checks---if the users are sure the imput data is correct specified, they can turn off the checks to save additional time. 

      The returned value is a matrix of sampled values from the posterior distribution. Rows represent the iteration number, and ideally the user will discard the first several rows as burn-in. Columns represent the parameter, so summarizing the matrix by column offers summaries of the model's results.
     }
     \value{
     Returns an object of class \code{matrix} that includes all iterations of the Metropolis-Hastings sampler. Each column of the matrix represents a different parameter--starting with \code{tau2}, \code{phi}, and \code{sigma2} before listing regression coefficients. Each row represents another iteration of the MCMC sampler. Summarizing the matrix by column offers summaries of the marginal posterior distribution by parameter.
     }

     \references{
      James E. Monogan III & Jeff Gill. 2016. "Measuring State and District Ideology with Spatial Realignment." \emph{Political Science Research and Methods} 4(1):97-121.
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

#For simple illustration, we set to few iterations.
#In this case, a 10,000-iteration run converges to the true parameters.
#If you have considerable time and hardware, delete the # on the next line.
#10,000 iterations took 39 min. with 8 GB RAM & a 1.5 GHz Quad-Core processor.
M<-8
#M<-10000

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
}
\keyword{estimation}
\keyword{posterior}
