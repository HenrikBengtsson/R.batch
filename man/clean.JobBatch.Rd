%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{clean.JobBatch}
\alias{clean.JobBatch}
\alias{JobBatch.clean}
\alias{clean.JobBatch}
\alias{clean,JobBatch-method}


\title{Cleans up among jobs in this JobBatch}

\description{
  Cleans up among jobs in this JobBatch.

  Currently, cleaning up a job means that all of its output files
  are moved to the directory of the job, i.e. \code{getPath(job)}.
}

\arguments{
 \item{jobs}{A \code{\link[base]{list}} of Jobs or a single Job to be be clean up.
     Default is to clean up all jobs in the JobBatch.}
 \item{...}{Not used.}
}

\value{
  Returns nothing.
}

\seealso{
  \code{\link[R.batch:findJobs.JobBatch]{*findJobs}()}.
  For more information see \code{\link{JobBatch}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
