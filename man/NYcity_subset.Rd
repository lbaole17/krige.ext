% File src/library/base/man/load.Rd
     \docType{data}
     \name{NYcity_subset}
     \alias{NYcity_subset}
     \title{New York City CCES Respondents in 2008}

     \description{
       These data are a subset of the 2008 Cooperative Congressional Election Survey (CCES) Common Content. Only 568 respondents from New York City are included, with predictors drawn from Monogan & Gill's (2016) model of self-reported ideology. The CCES data are merged with predictors on geographic location based on ZIP codes (from ArcGIS & TomTom) and county ruralism (from the USDA).
     }
     \usage{
data(NYcity_subset)
     }

     \format{
     	       The \code{NYcity_subset} dataset has 568 observations and 26 variables.

     	\describe{
      \item{\code{state}}{The state abbreviation of the respondent's residence.}
       \item{\code{zip}}{The respondent's ZIP code.}
        \item{\code{age}}{The age of the respondent in years.}
         \item{\code{female}}{An indicator of whether the respondent is female.}
          \item{\code{ideology}}{The respondent's self-reported ideology on a scale of 0 (liberal) to 100 (conservative).}
           \item{\code{educ}}{The respondent's level of education. 
           0=No Highschool,
           1=High School Graduate,
           2=Some College,
           3=2-year Degree,
           4=4-year degree,
           5=Post-Graduate.}
             \item{\code{race}}{The respondent's race. 1=White, 2=African American, 3=Nonwhite & nonblack.}
               \item{\code{empstat}}{The respondent's employment status. 1=employed, 2=unemployed, 3=not in workforce.}
                 \item{\code{ownership}}{Indicator for whether the respondent owns his or her own home.}
                   \item{\code{inc14}}{The respondent's self reported income. 1=Less than $10,000, 2=$10,000-$14,999, 3=$15,000-$19,000, 4=$20,000-$24,999, 5=$25,000-$29,999, 6=$30,000-$39,999, 7=$40,000-$49,999, 8=$50,000-$59,999, 9=$60,000-$69,999, 10=$70,000-$79,999, 11=$80,000-$89,999, 12=$100,000-$119,999, 13=$120,000-$149,999, 14=$150,000 or more.}
                    \item{\code{catholic}}{Indicator for whether the respondent is Catholic.}
                     \item{\code{mormon}}{Indicator for whether the respondent is Mormon.}
                      \item{\code{orthodox}}{Indicator for whether the respondent is Orthodox Christian.}
                       \item{\code{jewish}}{Indicator for whether the respondent is Jewish.}
                        \item{\code{islam}}{Indicator for whether the respondent is Muslim.}
                         \item{\code{mainline}}{Indicator for whether the respondent is Mainline Christian.}
                          \item{\code{evangelical}}{Indicator for whether the respondent is Evangelical Christian.}
                           \item{\code{FIPS_Code}}{FIPS code of the repondent's state.}
                            \item{\code{rural}}{Nine-point USDA scale of the ruralism of each county, with 0 meaning the most urban and 8 meaning the most rural.}
                             \item{\code{zipPop}}{Indicates the population of the repondent's ZIP code.}
                              \item{\code{zipLandKM}}{Indicates the land area in square kilometers of the repondent's ZIP code.}
                               \item{\code{weight}}{Survey weights created by the CCES.}
                                \item{\code{cd}}{The congressional district the respondent resides in.}
                                 \item{\code{fipsCD}}{Index that fuses the state FIPS code in the first two digits and the congressional district number in the last two digits.}
                                  \item{\code{northings}}{Indicates the geographical location of the respondent in kilometer-based northings.}
                                   \item{\code{eastings}}{Indicates the geographical location of the respondent in kilometer-based eastings.}
	    }
     }
\examples{
#Data
ny <- NYcity_subset

#data cleaning
ny$cathOrth<-ny$catholic+ny$orthodox
ny$consRelig<-ny$mormon+ny$evangelical
ny$jewMus<-ny$jewish+ny$islam

# Explanatory Variable Matrix
psrm.data <-cbind(1, ny$age, ny$educ, I(ny$age*ny$educ), as.numeric(ny$race==2), 
     as.numeric(ny$race==3), ny$female, I(as.numeric(ny$race==2)*ny$female), 
     I(as.numeric(ny$race==3)*ny$female), ny$cathOrth, ny$consRelig, 
     ny$jewMus, ny$mainline, ny$rural, ny$ownership, 
     as.numeric(ny$empstat==2), as.numeric(ny$empstat==3),ny$inc14)
dimnames(psrm.data)[[2]] <- c("Intercept", "Age", "Education", "Age.education", 
     "African.American", "Nonwhite.nonblack","Female", 
     "African.American.female", "Nonwhite.nonblack.female", 
     "Catholic.Orthodox", "Evang.Mormon", "Jewish.Muslim", 
     "Mainline","Ruralism", "Homeowner", "Unemployed",
     "Not.in.workforce","Income")

# Outcome Variable
ideo <- matrix(ny$ideology,ncol=1)

# Set Number of Iterations:
# WARNING: This example was tuned on Amazon Web Services (EC2) over many hours
# with 150,000 iterations--a strain in 2020 for most desktop machines.
# A test with few iterations allows illustration.
M<-5
#M<-150000 
set.seed(1,kind="Mersenne-Twister")

# Estimate the Model
out.mat <- metropolis.krige(y=ideo, X=psrm.data, east=ny$eastings, north=ny$northings,
     powered.exp=2, mcmc.samples=M, spatial.share=0.1, range.share=0.3, beta.var=1000, 
     range.tol=0.01, b.tune=0.1, nugget.tune=20, psill.tune=1) 

# Discard first 20\% of Iterations as Burn-In (User Discretion Advised).
out.mat <- out.mat[(ceiling(0.2*M)+1):M,]

# Summarize Results
apply(out.mat,2,quantile,c(0.5,0.05,0.95))

# nonconvergence diagnostics
# With few iterations there is insufficient variation for these tests.
#geweke(out.mat)
#heidel.welch(out.mat)

# Draw Semivariogram
city.ols<-lm(ideo~psrm.data-1)
graph.terms<-apply(out.mat[,1:3],2,quantile,0.5)
raw.semivar<-semivariogram(x=ideo,east=ny$eastings, north=ny$northings)
resid.semivar<-semivariogram(x=city.ols$residuals,east=ny$eastings, 
     north=ny$northings,draw.plot=FALSE)
points(resid.semivar,pch=3,col='blue')
lines(exponential.semivariogram(nugget=graph.terms[1],decay=graph.terms[2],
     partial.sill=graph.terms[3],
     distance=as.numeric(names(resid.semivar)),power=2),col='red')

# Predictive data for three prominent New Yorkers:
bill.deblasio<-c(1,58,5,58*5,0,0,0,0,0,0,0,0,0,0,1,0,0,14)
melania.trump<-c(1,49,2,49*2,0,0,1,0,0,1,0,0,0,0,1,0,0,14)
spike.lee<-c(1,63,5,63*5,1,0,0,0,0,0,0,0,1,0,1,0,0,14)
new.yorkers<-rbind(bill.deblasio,melania.trump,spike.lee)
gracie.mansion<-c(1829.802,580.4355)
trump.tower<-c(1827.654,578.3515)
hatch.house<-c(1828.273,578.7542)
new.locations<-rbind(gracie.mansion,trump.tower,hatch.house)
colnames(new.locations)<-c("eastings","northings")

# Make predictions from median parameter values:
median.pred<-krige.pred(pred.x=new.yorkers,
    pred.east=new.locations[,"eastings"],pred.north=new.locations[,"northings"],
    train.y=ideo,train.x=psrm.data,train.east=ny$eastings,
    train.north=ny$northings,mcmc.iter=out.mat,powered.exp=2,credible=NULL)
median.pred

# Make predictions with 90\% credible intervals:
cred.pred<-krige.pred(pred.x=new.yorkers,
    pred.east=new.locations[,"eastings"],pred.north=new.locations[,"northings"],
    train.y=ideo,train.x=psrm.data,train.east=ny$eastings,
    train.north=ny$northings,mcmc.iter=out.mat,powered.exp=2,credible=0.9)
cred.pred
}
      \source{
     Ansolabehere, Stephen. 2011. "CCES, Common Content, 2008." Ver. 4. \cr
     \url{http://hdl.handle.net/1902.1/14003}
     \cr\cr
     ArcGIS. 2012. "USA ZIP Code Areas." \cr
     \url{https://www.arcgis.com/home/item.html?id=8d2012a2016e484dafaac0451f9aea24}
     \cr\cr
     United States Department of Agriculture. 2013. "2013 Rural-Urban Continuum Codes." \cr
     \url{http://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx}
     }
     \references{
      James E. Monogan III & Jeff Gill. 2016. "Measuring State and District Ideology with Spatial Realignment." \emph{Political Science Research and Methods} 4(1):97-121.
     }
\keyword{datasets}

