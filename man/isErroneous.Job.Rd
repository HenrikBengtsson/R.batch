%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{isErroneous.Job}
\alias{isErroneous.Job}
\alias{Job.isErroneous}
\alias{isErroneous.Job}
\alias{isErroneous,Job-method}


\title{Checks if the job is erroneous}

\description{
 Checks if the job is erroneous. A job that fails during setup is erroneous. The most
 common reason is that the source code in the src/ directory or the
 job directory contains errors.
}

\usage{\method{isErroneous}{Job}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
 Returns \code{\link[base:logical]{TRUE}} if the job is erroneous, otherwise \code{\link[base:logical]{FALSE}}.
}

\seealso{
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
