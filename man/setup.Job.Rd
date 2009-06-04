%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{setup.Job}
\alias{setup.Job}
\alias{Job.setup}
\alias{setup.Job}
\alias{setup,Job-method}


\title{Initiates the job}

\description{
 Initiates the job by evaluating script files in the source directory and
 the job directory.

 This method is called by \code{\link[R.batch:run.Job]{*run}()}.
}

\usage{\method{setup}{Job}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
 Returns (invisibly) a \code{\link[base]{vector}} of the files sourced.
}

\details{
 All scripts files are evaluated with \code{source()} to the
 \emph{local} working environment of this job. That is, no global
 objects will be overwritten.
}

\seealso{
  \code{\link[R.batch:run.Job]{*run}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
