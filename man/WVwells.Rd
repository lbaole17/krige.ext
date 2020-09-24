% File src/library/base/man/load.Rd
     \docType{data}
     \name{WVwells}
     \alias{WVwells}
     \title{West Virginia Oil and Gas Production in 2012}

     \description{
       These data are a subset of the West Virginia Geological and Economic Survey of 2014. They contain information on the coordinates of wells that yielded at least some quantity of natural gas in 2012. In addition to coordinates, the data contain information on well ownership and operation, rock pressure at the well, elevation of the well, oil production, and gas production.
     }
     \usage{
data(WVwells)
     }

     \format{
     	       The \code{WVwells} dataset has 1949 observations and 18 variables.

     	\describe{
      \item{\code{APINum}}{A 10-digit number in the format assigned by the American Petroleum Institute (API), consisting of a 2-digit state code, a 3-digit county code with leading zeroes, and a 5-digit permit number with leading zeroes. Data Source: West Virginia Department of Environmental Protection, Office of Oil & Gas (WVDEP-OOG).}
       \item{\code{CntyCode}}{A 3-digit numeric code, assigned in numeric order by county name. Data Source: The county code for a well is assigned by WVDEP-OOG, based on the well location.}
        \item{\code{CntyName}}{The name of the county. Please see CntyCode (County Code) for a list of all West Virginia county names. Data Source: The county code for a well is assigned by WVDEP-OOG, based on the well location. The county name is a translation of the county code.}
         \item{\code{Operator}}{The name of the operator who owns the well at the time of reporting.  Data Source:  WVDEP-OOG plat; verified on the WR-35 completion record.}
          \item{\code{SurfaceOwn}}{The name of the owner of the surface property on which the well is located. Data Source: WVDEP-OOG plat; verified on the WR-35 completion record.}
           \item{\code{MineralOwn}}{Mineral Owner: The name of the owner of the mineral rights where the well is located. Data Source: WVDEP-OOG plat.}
             \item{\code{CompanyNum}}{The operator's serial number for the well. Data Source: WVDEP-OOG plat; verified on the WR-35 completion record.}
               \item{\code{WellNum}}{The operator's number for the well on the surface property (farm). Data Source: WVDEP-OOG plat; verified on the WR-35 completion record.}
                 \item{\code{UTMESrf}}{Surface Location--Universal Transverse Mercator, Easting: The well location at the surface measured in meters to one decimal point, east of the central meridian in UTM Zone 17; datum: NAD83. Data Source: Taken directly from the plat if given as such. Otherwise, computed from the location reported on the plat. Suspect locations may be adjusted using various additional resources (e.g. topographic maps) if deemed necessary.}
                   \item{\code{UTMNSrf}}{Surface Location--Universal Transverse Mercator, Northing: The well location at the surface measured in meters to one decimal point, north of the equator in UTM Zone 17; datum: NAD83. Data Source: Taken directly from the plat if given as such. Otherwise, computed from the location reported on the plat. Suspect locations may be adjusted using various additional resources (e.g. topographic maps) if deemed necessary.}
                    \item{\code{LonSrf}}{Surface Location--Longitude: The well location at the surface measured to a precision of 6 decimal points, in degrees west of the Prime Meridian. Data Source: Taken directly from the plat if given as such. Otherwise, computed from the location reported on the plat. Suspect locations may be adjusted using various additional resources (e.g. topographic maps) if deemed necessary.}
                     \item{\code{LatSrf}}{Surface Location--Latitude: The well location at the surface measured to a precision of 6 decimal points, in degrees north of the equator. Data Source: Taken directly from the plat if given as such. Otherwise, computed from the location reported on the plat. Suspect locations may be adjusted using various additional resources (e.g. topographic maps) if deemed necessary.}
                      \item{\code{Elevation}}{Elevation: The height of the well in feet above mean sea level. Data Source: WVDEP-OOG plat; verified on the WR-35 completion record.}
                       \item{\code{RockPres}}{Formation Rock Pressure at Surface: The pressure measured at the surface usually after stimulation, in pounds per square inch (psi). Data Source: WVDEP-OOG WR-35 completion record, submitted by the operator to WVDEP-OOG.}
                        \item{\code{GProd2012}}{2012 Gas Production Reported: The total gas production for the well for 2012 in thousands of cubic feet (MCF); includes all pay zones. Data Source: Production data reported by the operator to the State regulatory authority for Oil and Gas (WVDEP-OOG); WVGES obtained the data from WVDEP-OOG.}
                         \item{\code{OProd2012}}{2012 Oil Production Reported: The total oil production for the well for 2012 in barrels (Bbl); includes all pay zones. Production data reported by the operator to the State regulatory authority for Oil and Gas (WVDEP-OOG); WVGES obtained the data from WVDEP-OOG.}
                          \item{\code{logElevation}}{Logarithm of \code{Elevation}.}
                           \item{\code{logGProd2012}}{Logarithm of \code{GProd2012}.}
	    }
     }

\examples{
# Descriptive Statistics
summary(WVwells)

# Record means of predictors: 
# These are used BOTH to eliminate the intercept and to recover predictions later.
mean.logGas<-mean(WVwells$logGProd2012);mean.logGas
mean.logElevation<-mean(WVwells$logElevation);mean.logElevation
mean.RockPres<-mean(WVwells$RockPres);mean.RockPres

# Outcome Variable, De-Meaned
logGas <- matrix(WVwells$logGProd2012-mean.logGas,ncol=1)

# Explanatory Variable Matrix: DE-MEANED PREDICTORS AND NO CONSTANT TERM
# Because we deducted the mean from all predictors and the outcome,
# it is valid to do regression through the origin. 
wv.pred <-cbind(WVwells$logElevation-mean.logElevation,WVwells$RockPres-mean.RockPres)
dimnames(wv.pred)[[2]] <- c("LogElevation", "RockPressure")

# OLS Model
fracking.ols<-lm(logGas~wv.pred-1)
summary(fracking.ols)

# Set Number of Iterations:
# WARNING: 50 iterations is intensive on many machines.
# This example was tuned on Amazon Web Services (EC2) over many hours
# with 5,000 iterations--unsuitable in 2020 for most desktop machines.
M<-50#00 #The comment character constrains this to 50 iterations.
set.seed(1000,kind="Mersenne-Twister")#SET SEED FOR CONSISTENCY

\dontrun{
# Trial Run, Linear Model of Ideology with Geospatial Errors Using Metropolis-Hastings:
wv.mat <- metropolis.krige(y=logGas, X=wv.pred, east=WVwells$UTMESrf, north=WVwells$UTMNSrf,
     powered.exp=0.5, mcmc.samples=M, spatial.share=0.60, range.share=0.31, beta.var=1000,
     range.tol=0.1, b.tune=1, nugget.tune=1, psill.tune=30) 

# Discard first 20\% of Iterations as Burn-In (User Discretion Advised).
wv.mat <- wv.mat[(ceiling(0.2*M)+1):M,]

# Summarize Results
apply(wv.mat,2,quantile,c(0.5,0.05,0.95))

# Convergence Diagnostics
geweke(wv.mat)
heidel.welch(wv.mat)

# Draw Semivariogram
graph.terms<-apply(wv.mat[,1:3],2,quantile,0.5)
raw.semivar<-semivariogram(x=logGas,east=WVwells$UTMESrf,north=WVwells$UTMNSrf)
resid.semivar<-semivariogram(x=fracking.ols$residuals,east=WVwells$UTMESrf,
     north=WVwells$UTMNSrf,draw.plot=FALSE)
points(resid.semivar,pch=3,col='blue')
lines(exponential.semivariogram(nugget=graph.terms[1],decay=graph.terms[2],
     partial.sill=graph.terms[3],
     distance=as.numeric(names(resid.semivar)),power=0.5),col='red')

# Predictive Data for Two Wells Tapped in 2013
well.1<-c(log(1110)-mean.logElevation,1020-mean.RockPres)
well.2<-c(log(643)-mean.logElevation,630-mean.RockPres)
wells.2013<-rbind(well.1,well.2)
site.1<-c(557306.0, 4345265)
site.2<-c(434515.7, 4258449)
well.locations<-rbind(site.1,site.2)
colnames(well.locations)<-c("eastings","northings")

# Make predictions from median parameter values:
median.pred<-krige.pred(pred.x=wells.2013,
    pred.east=well.locations[,"eastings"],pred.north=well.locations[,"northings"],
    train.y=logGas,train.x=wv.pred,train.east=WVwells$UTMESrf,
    train.north=WVwells$UTMNSrf,mcmc.iter=wv.mat)
# Prediction of deviation on logged scale:
median.pred
# Prediction in thousands of cubic feet (MCF):
round(exp(median.pred+mean.logGas))

# Make predictions with 90\% credible intervals:
cred.pred<-krige.pred(pred.x=wells.2013,
    pred.east=well.locations[,"eastings"],pred.north=well.locations[,"northings"],
    train.y=logGas,train.x=wv.pred,train.east=WVwells$UTMESrf,
    train.north=WVwells$UTMNSrf,mcmc.iter=wv.mat,credible=0.90)
# Prediction of deviation on logged scale:
cred.pred
# Prediction in thousands of cubic feet (MCF) and the true yield in 2013:
Actual.Yield<-c(471171, 7211)
round(cbind(exp(cred.pred+mean.logGas),Actual.Yield))
}
}
      \source{
      	West Virginia Geological and Economic Survey. 2014. "WVMarcellusWellsCompleted102014." Morgantown, WV. \cr
\url{http://www.wvgs.wvnet.edu/www/datastat/devshales.htm}
     \cr\cr
     Accessed via: FracTracker. 2019. "West Virginia Oil & Gas Activity." \cr
     \url{https://www.fractracker.org/map/us/west-virginia/}
     }
     \references{
      Jason S. Byers & Jeff Gill. N.D. "Applied Geospatial Data Modeling in the Big Data Era: Challenges and Solutions."
     }
\keyword{datasets}

