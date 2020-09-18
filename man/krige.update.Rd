% File src/library/base/man/load.Rd
\name{krige.update}
\alias{krige.update}
\title{Update Kriging Models}
\description{This function updates the MCMC iterations associated with the kriging models.}
\usage{
krige.update(krige.mat,mcmc.samples=100,accepted.beta,accepted.nugget,
        accepted.decay,accepted.psill, y, X, east, north, powered.exp=2,
        spatial.share=0.5,range.share=0.5,beta.var=10,range.tol=0.05,b.tune=1.0,
        nugget.tune=10.0,psill.tune=1.0,message=NULL, save=NULL, time=NULL,
        checks=TRUE)
}
\arguments{
\item{krige.mat}{The output MCMC matrix from \code{metropolis.krige()}}
\item{mcmc.samples}{Number of MCMC iterations to be run for updating}
\item{accepted.beta}{The accepted.beta from the previous run}
\item{accepted.nugget}{The accepted.nugget from the previous run}
\item{accepted.decay}{The accepted.decay from the previous run}
\item{accepted.psill}{The accepted.psill from the previous run}
\item{y}{The dependent variable that was used in the previous model. The same as in \code{metropolis.krige()}}
\item{X}{The matrix of independent variables used in the previous model. The same as in \code{metropolis.krige()}}
\item{east}{Vector of eastings for all observations. The same as in \code{metropolis.krige()}}
\item{north}{Vector of northings for all observations. The same as in \code{metropolis.krige()}}
\item{powered.exp}{A powered exponential correlation structure for the data. The same as in \code{metropolis.krige()}}
\item{spatial.share}{Prior for proportion of unexplained variance that is spatial in nature. The same as in \code{metropolis.krige()}}
\item{range.share}{Prior for the effective range term, as a proportion of the maximum distance in the data. The same as in \code{metropolis.krige()}}
\item{beta.var}{Prior for the variance on zero-meaned normal priors on the regression coefficients. The same as in \code{metropolis.krige()}}
\item{range.tol}{Tolerance term for setting the effective range. The same as in \code{metropolis.krige()}}
\item{b.tune}{Tuning parameter for candidate generation of regression coefficients. The same as in \code{metropolis.krige()}}
\item{nugget.tune}{Tuning parameter for candidate generation of the nugget term (\code{tau2}). The same as in \code{metropolis.krige()}}
\item{psill.tune}{Tuning parameter for candidate generation of the partial sill term (\code{sigma2}). The same as in \code{metropolis.krige()}}
\item{message}{Experimental feature: An integer that specifies the frequency of the reporting of estimates in progress.}
\item{save}{Experimental feature: An integer that specifies the frequency to save the iterations in progress.}
\item{time}{Experimental feature: A vector that specifies when to report the progress and time remained.}
\item{checks}{Experimental feature: A logical variable that defines whether to check NA and comformity of parameters.}
             }

\details{This function updates the previous run by \code{metropolis.krige} since MCMC iterations need to run typically long to gurantee convergence.

\code{krige.mat} is the output MCMC matrix of \code{metropolis.krige()} from the previous run. Analysts can specify the number of interations for update run by \code{mcmc.samples}. The acceptance rates for \emph{beta}, \emph{nugget}, \emph{decay}, and \emph{psill} from the previous model also need to be specified to make sure consistency. Other arguments should also be specified as the same as in the \code{metropolis.krige()}, including \code{y}, \code{X}, \code{east}, \code{north}, \code{powered.exp}, \code{spatial.share}, \code{range.share}, \code{beta.var}, \code{range.tol}, \code{b.tune}, \code{nugget.tune}, \code{psill.tune}, \code{message}, \code{save}, \code{time}, and
\code{checks}.

The returned object is the same as \code{metropolis.krige()}, a matrix of sampled values from the posterior distribution. Rows represent the iteration number, and ideally the user will discard the first several rows as burn-in. Columns represent the parameter, so summarizing the matrix by column offers summaries of the model's results.
}

\value{
Returns an object of class \code{matrix} that includes all iterations of the Metropolis-Hastings sampler. Each column of the matrix represents a different parameter--starting with \code{tau2}, \code{phi}, and \code{sigma2} before listing regression coefficients. Each row represents another iteration of the MCMC sampler. Summarizing the matrix by column offers summaries of the marginal posterior distribution by parameter.
}

\examples{
\dontrun{
#Set seed
set.seed(1241060320)

#For simple illustration, we set to few iterations.
#But much more are needed for the realistic estimation.
#In this case, a 10,000-iteration run converges to the true parameters.
M1<-5

contrived.run<-metropolis.krige(y=ContrivedData$y,X=covariates,range.tol=0.05,
        east=ContrivedData$s.1,north=ContrivedData$s.2,mcmc.samples=M1)

#Acceptance rates for the first 5 iterations.
#"Acceptance percentages: Coefficients=50\%. Nugget=75\%. Decay=75\%.
#Partial sill=75\%."

#Update for another 5 iterations.
M2<-5

contrived.run.update<-update.krige(krige.mat=contrived,mcmc.samples=M2,
        accepted.beta=0.5,accepted.nugget=0.75,accepted.decay=0.75,
        accepted.psill=0.75,y=ContrivedData$y,X=covariates,range.tol=0.05,
        east=ContrivedData$s.1,north=ContrivedData$s.2)
}
}
\keyword{estimation}
\keyword{update}
\keyword{posterior}
