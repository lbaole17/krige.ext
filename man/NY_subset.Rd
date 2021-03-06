% File src/library/base/man/load.Rd
     \docType{data}
     \name{NY_subset}
     \alias{NY_subset}
     \title{New York State CCES Respondents in 2008}

     \description{
       These data are a subset of the 2008 Cooperative Congressional Election Survey (CCES) Common Content. Only 1108 respondents from the state of New York are included, with predictors drawn from Gill's (2020) model of self-reported ideology. The CCES data are merged with predictors on geographic location based on ZIP codes (from ArcGIS & TomTom) and county ruralism (from the USDA).
     }
     \usage{
data(NY_subset)
     }

     \format{
     	       The \code{NY_subset} dataset has 1108 observations and 26 variables.

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
ny <- NY_subset

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
# WARNING: 20 iterations is intensive on many machines.
# This example was tuned on Amazon Web Services (EC2) over many hours
# with 20,000 iterations--unsuitable in 2020 for most desktop machines.
M<-20#000 #The comment character constrains this to 20 iterations.
set.seed(1,kind="Mersenne-Twister")

\dontrun{
# Estimate the Model
out.mat <- metropolis.krige(y=ideo,X=psrm.data,east=ny$eastings,north=ny$northings,
    powered.exp=1,mcmc.samples=M,spatial.share=0.31,range.share=0.23,beta.var=10,
    range.tol=0.01, b.tune=0.1, nugget.tune=20, psill.tune=5)	

# Discard first 20\% of Iterations as Burn-In (User Discretion Advised).
out.mat <- out.mat[(ceiling(0.2*M)+1):M,]

# Summarize Results
apply(out.mat,2,quantile,c(0.5,0.05,0.95))

#Convergence Diagnostics: Geweke and Heidelberger-Welch
#Note that the second (commented) version of Geweke is more typical
#of a 20,000 iteration run.
geweke(out.mat,early.prop=0.5)
#geweke(out.mat)
heidel.welch(out.mat)

# Draw Semivariogram
state.ols<-lm(ideo~psrm.data-1)
graph.terms<-apply(out.mat[,1:3],2,quantile,0.5)
raw.semivar<-semivariogram(x=ideo,east=ny$eastings, north=ny$northings)
resid.semivar<-semivariogram(x=state.ols$residuals,east=ny$eastings, 
     north=ny$northings,draw.plot=FALSE)
points(resid.semivar,pch=3,col='blue')
lines(exponential.semivariogram(nugget=graph.terms[1],decay=graph.terms[2],
     partial.sill=graph.terms[3],
     distance=as.numeric(names(resid.semivar)),power=1),col='red')
}
}
      \source{
     Ansolabehere, Stephen. 2011. "CCES, Common Content, 2008." Ver. 4. \cr
     \url{http://hdl.handle.net/1902.1/14003}
     \cr\cr
     ArcGIS. 2012. "USA ZIP Code Areas." \cr
     \url{https://www.arcgis.com/home/item.html?id=8d2012a2016e484dafaac0451f9aea24}
     \cr\cr
     United States Department of Agriculture. 2013. "2013 Rural-Urban Continuum Codes." \cr
     \url{https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx}
     }
     \references{
      Jeff Gill. 2020. Measuring Constituency Ideology Using Bayesian Universal Kriging. \emph{State Politics & Policy Quarterly}.
     }
\keyword{datasets}

