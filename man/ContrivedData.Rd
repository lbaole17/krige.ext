% File src/library/base/man/load.Rd
     \docType{data}
     \name{ContrivedData}
     \alias{ContrivedData}
     \title{Simulated Point-Referenced Spatial Data}

     \description{
       These data are a simulated point-referenced geospatial data that serve to provide a clean example of a kriging model. There are 500 observations with coordinates located on a unit square.
     }
     \usage{
data(NY_subset)
     }

     \format{
     	       The \code{ContrivedData} dataset has 500 observations and 5 variables.

     	\describe{
      \item{\code{y}}{The outcome variable. Its true population functional form is \eqn{y_s=0+1 x_{1s}+2 x_{2s}+\omega_{s}+\epsilon_{s}}. The true variance of \eqn{\omega} is \eqn{\sigma^2=0.5} and of \eqn{\epsilon} is \eqn{\tau^2=0.5}. The decay term that shapes spatial correlation levels is \eqn{\phi=2.5}.}
       \item{\code{x.1}}{A predictor with a standard uniform distribution.}
        \item{\code{x.2}}{A predictor with a standard normal distribution.}
         \item{\code{s.1}}{Coordinate in eastings for each observation, distributed standard uniform.}
          \item{\code{s.2}}{Coordinate in northings for each observation, distributed standard uniform.}
	    }
     }
\examples{
#Examine Data
summary(ContrivedData)

#Initial OLS Model
contrived.ols<-lm(y~x.1+x.2,data=ContrivedData);summary(contrived.ols)
}
     \keyword{datasets}

