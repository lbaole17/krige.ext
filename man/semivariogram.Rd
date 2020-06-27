% File src/library/base/man/load.Rd
     \name{semivariogram}
     \alias{semivariogram}
     \title{Empirical Semivariogram}
     \description{
       This function computes the empirical semivariance for a spatially-distributed variable. Based on the user's chosen level of coarsening, the semivariance is presented for various distances.
     }
     \usage{
semivariogram(x,east,north,bins=13,draw.plot=TRUE)
     }
     \arguments{
             \item{x}{The spatial variable for which the semivariogram is desired.}
              \item{east}{Vector of eastings for the observations of x.}
               \item{north}{Vector of northings for the observations of x.}
            \item{bins}{Number of bins into which distances should be divided. The observed distances will be split at equal intervals, and the semivariance will be computed within each interval. Defaults to 13 intervals.}
             \item{draw.plot}{By default, a graph of the empirical semivariogram appears with a run of the function. Changing this setting to \code{FALSE} omits this and only returns semivariogram values.}
     }
     \details{
      Semivariance is equal to half of the variance of the difference in a variable's values at a given distance. That is, the semivariance is defined as: \eqn{\gamma(h)=0.5*E[X(s+h)-X(s)]^2}, where \eqn{X} is the variable of interest, s is a location, and h is the distance from s to another location.
      
      An empirical semivariogram is a good tool for diagnosing the kind of spatial relationship that can best describe the data. With a view of the empirical semivariogram, a user can consult images of parametric semivariograms to determine whether an exponential, Gaussian, or other powered expoential function fit the data well, or if another style of semivariogram works better. Examining this also allows the user to develop priors such as the approximate split in variance between the nugget and partial sill as well as the approximate distance of the effective range. Semivariograms are explicitly tied to a corresponding spatial correlation function, so determining the former automatically implies the latter. See Banerjee, Carlin, and Gelfand for a fuller explanation, as well as a guidebook to semivariogram diagnosis (2015, 26-30).
     }
     \value{
     Returns an object of class \code{numeric} with each bin's value of the semivariance. Each entry is labeled by the upper bound of the bin. The lower bound of each bin is 0 for the first one, and the prior upper bound for all subsequent bins.
     }
     
     \references{
      Sudipto Banerjee, Bradley P. Carlin, and Alan E. Gelfand. 2015. \emph{Hierarchical Modeling and Analysis for Spatial Data}. 2nd ed. Boca Raton, FL: CRC Press.
     }

     \examples{
#Examine Data
summary(ContrivedData)

#Initial OLS Model
contrived.ols<-lm(y~x.1+x.2,data=ContrivedData);summary(contrived.ols)

#Graphically Examine the Semivariogram of Raw Data and Residuals
raw.semivar<-semivariogram(x=ContrivedData$y,east=ContrivedData$s.1,
     north=ContrivedData$s.2)
resid.semivar<-semivariogram(x=contrived.ols$residuals,east=ContrivedData$s.1,
     north=ContrivedData$s.2,draw.plot=FALSE)
points(resid.semivar,pch=3,col='blue')
}
\keyword{semivariogram}
\keyword{krige}
