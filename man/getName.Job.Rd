%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getName.Job}
\alias{getName.Job}
\alias{Job.getName}
\alias{getName.Job}
\alias{getName,Job-method}


\title{Gets the name of the job}

\description{
 Gets the name of the job.

 The name of a job is by definition equal to the name of the basename of
 the job directory, that is, identical to \code{basename(getPath(job))}.
}

\usage{\method{getName}{Job}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{character}} string.
}

\seealso{
  \code{\link{basename}}().
  \code{\link[R.batch:getLabel.Job]{*getLabel}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
