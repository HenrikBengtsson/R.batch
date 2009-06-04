%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{unlock.Job}
\alias{unlock.Job}
\alias{Job.unlock}
\alias{unlock.Job}
\alias{unlock,Job-method}


\title{Unlocks the job}

\description{
 Unlocks the job.

 This method is called by various methods. All methods that locks a
 job will also unlock it. If a \code{Job} object is delete, it will
 also be unlocked when deallocated (by the garbage collector), if locked.
}

\usage{\method{unlock}{Job}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
 Returns \code{\link[base:logical]{TRUE}} if job is unlocked afterwards, otherwise \code{\link[base:logical]{FALSE}}.
 If job does not exist, an exception is thrown.
}

\seealso{
  \code{\link[R.batch:isLocked.Job]{*isLocked}()} and \code{\link[R.batch:lock.Job]{*lock}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
