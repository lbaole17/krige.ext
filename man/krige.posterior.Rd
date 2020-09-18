% File src/library/base/man/load.Rd
     \name{krige.posterior}
     \alias{krige.posterior}
     \title{Posterior Distribution for the Kriging Process}
     \description{
       This function finds the posterior density of a geospatial linear regression model given a point-referenced geospatial dataset and a set of parameter values. The function is useful for finding the optimum of or for sampling from the posterior distribution.
     }
     \usage{
       krige.posterior(tau2,phi,sigma2,beta,y,X,east,north,semivar.exp=2,
         p.spatial.share=0.5,p.range.share=0.5,p.range.tol=0.05,p.beta.var=10,
         tot.var=var(y),local.Sigma=NULL,distance=NULL, max.distance=NULL, checks=TRUE)
     }
     \arguments{
        \item{tau2}{Value of the nugget, or non-spatial error variance.}
         \item{phi}{Value of the decay term, driving the level of spatial correlation.}
          \item{sigma2}{Value of the partial sill, or maximum spatial error variance.}
           \item{beta}{Coefficients from linear model.}
            \item{y}{The dependent variable that is used in the kriging model.}
             \item{X}{The matrix of independent variables used in the kriging model.}
              \item{east}{Vector of eastings for all observations.}
               \item{north}{Vector of northings for all observations.}
               \item{semivar.exp}{This exponent, which must be greater than 0 and less than or equal to 2, specifies a powered exponential correlation structure for the data. One widely used specification is setting this to 1, which yields an exponential correlation structure. Another common specification is setting this to 2 (the default), which yields a Gaussian correlation structure.}
		\item{p.spatial.share}{Prior for proportion of unexplained variance that is spatial in nature. Must be greater than 0 and less than 1. Defaults to an even split.}
               \item{p.range.share}{Prior for the effective range term, as a proportion of the maximum distance in the data. Users should choose the proportion of distance at which they think the spatial correlation will become negligible. Must be greater than 0. Values greater than 1 are permitted, but users should recognize that this implies that meaningful spatial correlation would persist outside of the convex hull of data. Defaults to half the maximum distance.}
               \item{p.range.tol}{Tolerance term for setting the effective range. At the distance where the spatial correlation drops below this term, it is judged that the effective range has been met. Users are typically advised to leave this at its default value of 0.05 unless they have strong reasons to choose another level. Must be greater than 0 and less than 1.}
               \item{p.beta.var}{Prior for the variance on zero-meaned normal priors on the regression coefficients. Defaults to 10.}
               \item{tot.var}{Combined variance between the nugget and partial sill. Defaults to the variance of y. The \code{metropolis.krige} function inserts the residual variance from a standard linear model.}
               \item{local.Sigma}{The user is advised to ignore this option, or leave it the value of \code{NULL}. This option is included to reduce the number of computations required when this function is called by \code{metropolis.krige}.}
               \item{distance}{Experimental feature: The user is advised to ignore this option, or leave it the value of \code{NULL}. This option is included to reduce the number of computations required when this function is called by \code{metropolis.krige}.}
               \item{max.distance}{The user is advised to ignore this option, or leave it the value of \code{NULL}. This option is included to reduce the number of computations required when this function is called by \code{metropolis.krige}.}
               \item{checks}{Experimental feature: A logical variable that defines whether to check NA and comformity of parameters.}
     }
     \details{
      This function finds the posterior density for a kriging model. The function utilizes information provided about the parameters \code{tau2}, \code{phi}, \code{sigma2}, and \code{beta}. It also utilizes the observed data \code{y}, \code{X}, \code{east}, and \code{north}. Given a set of parameter values as well as the observed data, the function returns the posterior density for the specified model.
     }
     \value{
     Returns a single number that is the posterior density of the function, which is stored in object of class \code{matrix}.
     }

     \references{
      Jeff Gill. 2020. Measuring Constituency Ideology Using Bayesian Universal Kriging. \emph{State Politics & Policy Quarterly}.
     }

     \examples{
#Examine Data
summary(ContrivedData)

#Initial OLS Model
contrived.ols<-lm(y~x.1+x.2,data=ContrivedData);summary(contrived.ols)

#Define Covariate Matrix
covariates<-cbind(1,ContrivedData$x.1,ContrivedData$x.2)

# Find the posterior density for the Contrived Data if all parameters were 1:
s.test <- krige.posterior(tau2=1,phi=1,sigma2=1,beta=rep(1,ncol(covariates)),
     y=ContrivedData$y,X=covariates,east=ContrivedData$s.1,north=ContrivedData$s.2)

# Print posterior density
s.test
}
\keyword{posterior}
