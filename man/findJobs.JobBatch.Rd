%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{findJobs.JobBatch}
\alias{findJobs.JobBatch}
\alias{JobBatch.findJobs}
\alias{findJobs.JobBatch}
\alias{findJobs,JobBatch-method}


\title{Searches by name for Job:s in this JobBatch}

\description{
  Searches by name for Job:s in this JobBatch. Jobs are searched for in the subfolders a valid Job,
  that is, a job directory, can reside in.
  The name of a Job is equal to the jobs directory name, e.g.
  \code{job01}.
}

\arguments{
 \item{names}{A \code{\link[base]{vector}} of \code{\link[base]{character}} strings of job names to be found.}
 \item{regexpr}{If \code{\link[base:logical]{TRUE}}, the \code{names} strings may be regular
   expression patterns, otherwise exact matching is required.}
 \item{where}{A \code{\link[base]{vector}} of \code{\link[base]{character}} strings of directory names where to search for jobs.}
 \item{...}{Not used.}
}

\value{
  Returns a named list of Job:s with names equal to the \code{names}
  argument. If \code{regexpr} is \code{\link[base:logical]{TRUE}}, several elements may have
  the same name. Jobs not found are returned as \code{\link[base]{NULL}}.
}

\seealso{
  \code{\link{getName.Job}}().
  For more information see \code{\link{JobBatch}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
