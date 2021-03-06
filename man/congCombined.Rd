% File src/library/base/man/load.Rd
     \docType{data}
     \name{congCombined}
     \alias{congCombined}
     \title{Congressional District Public Opinion Ideology in 2010}

     \description{
       These data present measures of ideology in 2010 for 434 districts for the U.S. House of Representatives, recorded as the variable \code{krige.cong}. Forecasts are based on a kriging model fitted over the 2008 Cooperative Congressional Election Survey (CCES), paired with predictive data from the 2010 Census. Each district's public ideology is paired with the DW-NOMINATE common space score of each of its representative in 2011 (update from McCarty, Poole and Rosenthal 1997). Eight districts have repeated observations in order to include the DW-NOMINATE score when a member was replaced mid-term.
     }
     \usage{
data(congCombined)
     }

     \format{
     	       The \code{congCombined} dataset has 442 observations and 12 variables. 434 out of 435 congressional districts are covered, with eight districts duplicated when a member was replaced mid-term.

     	\describe{
\item{\code{stateCD}}{Unique identifier for each congressional district by state. The first two digits are \code{STATEA}, and the second two are \code{cd}.}
       \item{\code{krige.cong}}{The ideology of the average citizen in the congressional district.}
        \item{\code{krige.state.var}}{The variance of ideology among the district's citizens.}
         \item{\code{cong}}{The term of Congress studied--112 for this dataset.}
          \item{\code{idno}}{Identification number for the House member--ICPSR numbers continued by Poole & Rosenthal.}
           \item{\code{state}}{The ICPSR code for the state.}
             \item{\code{cd}}{The congressional district number.}
               \item{\code{statenm}}{The first seven letters of the state's name.}
                 \item{\code{party}}{Political party of the House member. 100=Democrat, 200=Republican.}
                   \item{\code{name}}{Last name of the House member, followed by first name if ambiguous.}
                    \item{\code{dwnom1}}{First dimension DW-NOMINATE common space score for the House member. Higher values are usually interpreted as more right-wing, with lower values as more left-wing.}
      					\item{\code{STATEA}}{The FIPS code for the state.}
	    }
     }
     
\examples{
# Descriptive Statistics
summary(congCombined)

# Correlate House Members' DW-NOMINATE Scores with Public Opinion Ideology
cor(congCombined$dwnom1,congCombined$krige.cong)

# Plot House Members' DW-NOMINATE Scores against Public Opinion Ideology
plot(y=congCombined$dwnom1,x=congCombined$krige.cong,
     xlab="District Ideology (Kriging)", ylab="Legislator Ideology (1st Dim., Common Space)",
     main="U.S. House of Representatives", type="n")
points(y=congCombined$dwnom1[congCombined$party==200],
     x=congCombined$krige.cong[congCombined$party==200],pch="R",col="red")
points(y=congCombined$dwnom1[congCombined$party==100],
     x=congCombined$krige.cong[congCombined$party==100],pch="D",col="blue")
}
      \source{
     Ansolabehere, Stephen. 2011. "CCES, Common Content, 2008." Ver. 4. \cr
     \url{http://hdl.handle.net/1902.1/14003}
     \cr\cr
     McCarty, Nolan M., Keith T. Poole and Howard Rosenthal. 1997. \emph{Income Redistribution and the Realignment of American Politics}. American Enterprise Institude Studies on Understanding Economic Inequality. Washington: AEI Press.
     \cr\cr
     Minnesota Population Center. 2011. \emph{National Historical Geographic Information System: Version 2.0.} Minneapolis, MN: University of Minnesota.\cr
     \samp{https://www.nhgis.org}
     }
     \references{
      Jeff Gill. 2020. Measuring Constituency Ideology Using Bayesian Universal Kriging. \emph{State Politics & Policy Quarterly}.
     }
\keyword{datasets}

