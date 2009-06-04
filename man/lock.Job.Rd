%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{lock.Job}
\alias{lock.Job}
\alias{Job.lock}
\alias{lock.Job}
\alias{lock,Job-method}


\title{Locks the job}

\description{
 Locks the job. When a job is locked, no other \code{Job} objects can
 lock and run the same job code.
}

\usage{\method{lock}{Job}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
 Returns \code{\link[base:logical]{TRUE}} if job is locked afterwards, otherwise \code{\link[base:logical]{FALSE}}.
 If job does not exist or is already locked, an exception is thrown.
}

\seealso{
  \code{\link[R.batch:isLocked.Job]{*isLocked}()} and \code{\link[R.batch:unlock.Job]{*unlock}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
