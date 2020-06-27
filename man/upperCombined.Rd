% File src/library/base/man/load.Rd
     \docType{data}
     \name{upperCombined}
     \alias{upperCombined}
     \title{State Legislative District (Upper Chambers) Public Opinion Ideology in 2010}

     \description{
       These data present measures of ideology in 2010 for the districts for upper chambers of state legislatures, recorded as the variable \code{krige.upper}. All 50 states' chambers are covered (including the Nebraska Unicameral). Forecasts are based on a kriging model fitted over the 2008 Cooperative Congressional Election Survey (CCES), paired with predictive data from the 2010 Census. Each district's public ideology is paired with a measure of the ideology of the State Senate member from the district (update from Shor and McCarty 2011).
     }
     \usage{
data(upperCombined)
     }

     \format{
     	       The \code{upperCombined} dataset has 1989 observations and 10 variables.

     	\describe{
     	\item{\code{st}}{Two-letter postal abbreviation for the state.}
	 \item{\code{upper}}{The state legislative district number (upper chamber).}     		
      \item{\code{STATEA}}{The FIPS code for the state.}
       \item{\code{krige.upper}}{The ideology of the average citizen in the district.}
       \item{\code{upperKluge}}{Combined index of \code{STATEA} followed by \code{upper}.}
        \item{\code{krige.upper.var}}{The variance of ideology among the district's citizens.}
                \item{\code{name}}{Last name of the state legislator, followed by first name and middle initial.}
                 \item{\code{party}}{Political party of the legislator. D=Democrat, R=Republican, X=Other.}
                         \item{\code{st_id}}{Temporary identifer variable. DO NOT USE.}
                    \item{\code{np_score}}{Ideology score for the state legislator (upper chamber). Higher values are usually interpreted as more right-wing, with lower values as more left-wing.}
	    }
     }
     
\examples{
# Descriptive Statistics
summary(upperCombined)

# Correlate Senators' DW-NOMINATE Scores with Public Opinion Ideology
cor(upperCombined$np_score,upperCombined$krige.upper,use="complete.obs")

# Plot Legislators' DW-NOMINATE Scores against Public Opinion Ideology
plot(y=upperCombined$np_score,x=upperCombined$krige.upper,
     xlab="District Ideology (Kriging)", ylab="Legislator Ideology (Shor & McCarty)", 
     main="State Legislatures: Upper Chambers", type="n")
points(y=upperCombined$np_score[upperCombined$party=="R"],
     x=upperCombined$krige.upper[upperCombined$party=="R"],pch=".",col="red")
points(y=upperCombined$np_score[upperCombined$party=="D"],
     x=upperCombined$krige.upper[upperCombined$party=="D"],pch=".",col="blue")
}
      \source{
     Ansolabehere, Stephen. 2011. "CCES, Common Content, 2008." Ver. 4. \cr
     \url{http://hdl.handle.net/1902.1/14003}
     \cr\cr
     Minnesota Population Center. 2011. \emph{National Historical Geographic Information System: Version 2.0.} Minneapolis, MN: University of Minnesota.\cr
     \samp{https://www.nhgis.org}
     \cr\cr
     Shor, Boris and Nolan M. McCarty. 2011. "The Ideological Mapping of American Legislatures." \emph{American Political Science Review} 105(3):530-551.
     }
     \references{
      James E. Monogan III & Jeff Gill. 2016. "Measuring State and District Ideology with Spatial Realignment." \emph{Political Science Research and Methods} 4(1):97-121.
     }
\keyword{datasets}

