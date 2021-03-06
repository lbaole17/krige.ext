% File src/library/base/man/load.Rd
     \docType{data}
     \name{stateCombined}
     \alias{stateCombined}
     \title{State Public Opinion Ideology in 2010}

     \description{
       These data present measures of ideology in 2010 for the 50 American states, recorded as the variable \code{krige.state}. Forecasts are based on a kriging model fitted over the 2008 Cooperative Congressional Election Survey (CCES), paired with predictive data from the 2010 Census. Each state is listed twice, as each state's public ideology is paired with the DW-NOMINATE common space score of each of its two senators in 2011 (update from McCarty, Poole and Rosenthal 1997).
     }
     \usage{
data(stateCombined)
     }

     \format{
     	       The \code{stateCombined} dataset has 100 observations (2 each for 50 states) and 13 variables.

     	\describe{
      \item{\code{STATEA}}{The FIPS code for the state.}
       \item{\code{krige.state}}{The ideology of the average citizen in the state.}
        \item{\code{krige.state.var}}{The variance of ideology among the state's citizens.}
         \item{\code{cong}}{The term of Congress studied--112 for this dataset.}
          \item{\code{idno}}{Identification number for the senator--ICPSR numbers continued by Poole & Rosenthal.}
           \item{\code{state}}{The ICPSR code for the state.}
             \item{\code{cd}}{The congressional district number--0 for senators.}
               \item{\code{statenm}}{The first seven letters of the state's name.}
                 \item{\code{party}}{Political party of the senator. 100=Democrat, 200=Republican, 328=Independent.}
                   \item{\code{name}}{Last name of the senator, followed by first name if ambiguous.}
                    \item{\code{dwnom1}}{First dimension DW-NOMINATE common space score for the senator. Higher values are usually interpreted as more right-wing, with lower values as more left-wing.}
                     \item{\code{stateCD}}{Combined index of \code{STATEA} followed by \code{cd}.}
                     	\item{\code{obama}}{Barack Obama's percentage of the two-party vote in the state in 2012.}
	    }
     }
     
\examples{
# Descriptive Statistics
summary(stateCombined)

# Correlate Senators' DW-NOMINATE Scores with Public Opinion Ideology
cor(stateCombined$krige.state,stateCombined$dwnom1)

# Plot Senators' DW-NOMINATE Scores against Public Opinion Ideology
plot(y=stateCombined$dwnom1,x=stateCombined$krige.state,
     xlab="State Ideology (Kriging)", ylab="Legislator Ideology (1st Dim., Common Space)",
     main="U.S. Senate", type="n")
points(y=stateCombined$dwnom1[stateCombined$party==200],
     x=stateCombined$krige.state[stateCombined$party==200],pch="R",col="red")
points(y=stateCombined$dwnom1[stateCombined$party==100],
     x=stateCombined$krige.state[stateCombined$party==100],pch="D",col="blue")
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

