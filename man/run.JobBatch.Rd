%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{run.JobBatch}
\alias{run.JobBatch}
\alias{JobBatch.run}
\alias{run.JobBatch}
\alias{run,JobBatch-method}


\title{Process some or all jobs available}

\description{
 Process some or all jobs available.

 Note that if a job is \emph{interrupted} this method will detect that
 and return quietly.  The rational for this is that it should always be
 possible to interrupt the job batch too, without having to kill the
 process.
}

\usage{\method{run}{JobBatch}(this, maxJobs=-1, sleepTime=15, clean=FALSE, ..., verbose=FALSE)}

\arguments{
 \item{maxJobs}{The maximum number of jobs to be processed.
   If \code{-1}, all jobs available will be processed.
   If \code{\link[base:is.finite]{Inf}}, it will run forever until a job is interrupted.  When
   no more jobs are detected, it will recheck for new jobs every
   \code{sleepTime} seconds.
 }
 \item{sleepTime}{A \code{\link[base]{double}} specifying the number of seconds for \R
   to sleep before checking for new jobs.}
 \item{...}{Arguments passed to \code{\link[R.batch:getRunAndFinishJob.JobBatch]{*getRunAndFinishJob}()}.}
}

\value{
 Returns (invisibly) a \code{\link[base]{list}} of descriptions of jobs processed.
}

\seealso{
  \code{\link[R.batch:getRunAndFinishJob.JobBatch]{*getRunAndFinishJob}()}.
  For more information see \code{\link{JobBatch}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
