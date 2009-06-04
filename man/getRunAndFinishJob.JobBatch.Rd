%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getRunAndFinishJob.JobBatch}
\alias{getRunAndFinishJob.JobBatch}
\alias{JobBatch.getRunAndFinishJob}
\alias{getRunAndFinishJob.JobBatch}
\alias{getRunAndFinishJob,JobBatch-method}


\title{Gets an non-locked job, process it and moves it to a final destination}

\description{
 Gets an non-locked job, process it and moves it to a final destination.
}

\usage{\method{getRunAndFinishJob}{JobBatch}(this, sink=TRUE, reset=FALSE, clean=FALSE, verbose=FALSE, .cleanup=NULL, ...)}

\arguments{
 \item{reset}{If \code{\link[base:logical]{TRUE}}, the job is reset before processed.}
 \item{sink}{If \code{\link[base:logical]{TRUE}}, all output is sinked to a file, otherwise not.}
 \item{verbose}{If \code{\link[base:logical]{TRUE}}, extra information is displayed. The created
   Job object gets this verbose level too.}
 \item{.cleanup}{A \code{\link[base]{function}} to be called after running job. For
   internal use only.}
 \item{...}{Passed to \code{\link[R.batch:getNextJob.JobBatch]{*getNextJob}()}.}
}

\value{
 Returns the \code{\link{Job}} object that was processed. If no unlocked job
 was available, \code{\link[base]{NULL}} was returned.
}

\seealso{
  \code{\link[R.batch:run.JobBatch]{*run}()}.
  For more information see \code{\link{JobBatch}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
