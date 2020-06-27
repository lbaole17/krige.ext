% File src/library/base/man/load.Rd
     \name{krige.pred}
     \alias{krige.pred}
     \title{Predictions by Kriging}
     \description{
       This function uses the results of a model estimated by \code{metropolis.krige} to make kriging-based predictions, the input data for that model, and data with coordinates for the new responses to be predicted.
     }
     \usage{
krige.pred(pred.x,pred.east,pred.north,train.y,train.x,train.east,train.north,
     mcmc.iter,powered.exp=2,credible=NULL,inv.Sigma=NULL)
     }
     \arguments{
             \item{pred.x}{The matrix of independent variables for observations to be predicted.}
              \item{pred.east}{Vector of eastings for observations to be predicted.}
               \item{pred.north}{Vector of northings for observations to be predicted.}
            \item{train.y}{The dependent variable that was used to estimate the original model.}
             \item{train.x}{The matrix of independent variables that was used to estimate the original model.}
              \item{train.east}{Vector of eastings for all observations that was used to estimate the original model.}
               \item{train.north}{Vector of northings for all observations that was used to estimate the original model.}
               \item{mcmc.iter}{The output matrix created by \code{metropolis.krige} containing the iterations of the MCMC sampler from the model.}
               \item{powered.exp}{This value should be set to the same value from the original model. It defaults to 2, which is also the default in the \code{metropolis.krige} function. Thus, if this option is ignored in both commands, they appropriately will be equal.}
               \item{credible}{If a credible interval on predictions is desired, a user may specify a proportion between 0 and 1 to indicate the interval probability. For example, a value of 0.9 would create a 90\% credible interval. If \code{NULL}, then no credible interval will be produced.}
               \item{inv.Sigma}{Users will almost always want to leave this parameter at its default value of \code{NULL}. If \code{inv.Sigma} is specified, the user is providing the inverse of an estimated covariance matrix of errors that will be used when making point predictions of forecasts. A matrix specified here will be ignored when credible intervals are created. This option only exists to facilitate computation of many one-at-a-time point predictions.}
     }
     \details{
      Analysts should use this function if they want to make kriged predictions for observations at new locations. To do this, researchers first must estimate a model using the \code{metropolis.krige} function.

      After estimating the model, the first three arguments present covariate and location information for the observations to be predicted. The next five arguments provide information from the model fitted with \code{metropolis.krige}, including the outcome variable, predictor variables, coordinates, and the model results themselves. Users also should ensure that \code{powered.exp} has the same value in both \code{metropolis.krige} and \code{krige.pred}. (Note that the defaults of both functions are the same, so omission in both is acceptable.)

      By default, the function uses median values of parameters to make a single point prediction for every kriged data point. However, if the uses specifies a probability with the \code{credible} option, then the function will determine the predictions for all iterations of the MCMC sample. The point estimates will then be a median of these predictions, and a credible interval will be returned based on percentiles. Note that estimating a credible interval is substantially more intensive computationally, but has the benefit of reporting uncertainty in predictions.
     }
     \value{
     Returns an object of class \code{matrix} with one prediction per row. By default the matrix has one column, as only point predictions are returned. If the \code{credible} option is specified, there are three columns respectively indicating a point estimate (median prediction from MCMC), lower bound of the credible interval, and upper bound of the credible interval.
     }

     \references{
      Sudipto Banerjee, Bradley P. Carlin, and Alan E. Gelfand. 2015. \emph{Hierarchical Modeling and Analysis for Spatial Data}. 2nd ed. Boca Raton, FL: CRC Press.
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
\keyword{predict}
\keyword{krige}
