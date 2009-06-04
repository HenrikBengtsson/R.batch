%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{isLocked.Job}
\alias{isLocked.Job}
\alias{Job.isLocked}
\alias{isLocked.Job}
\alias{isLocked,Job-method}


\title{Checks if a job is locked}

\description{
  Checks if a job is locked either by this \code{Job} object or another.
  A job is defined to be locked if an \emph{unremovable} lock file
  (\code{.lock}) exists. This function will try to delete the lock file
  before checking in order to avoid forgotten or false lock files.
}

\usage{\method{isLocked}{Job}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
 Returns \code{\link[base:logical]{TRUE}} if job is locked, otherwise \code{\link[base:logical]{FALSE}}.
}

\seealso{
  \code{\link[R.batch:lock.Job]{*lock}()} and \code{\link[R.batch:unlock.Job]{*unlock}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
