%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getLabel.Job}
\alias{getLabel.Job}
\alias{Job.getLabel}
\alias{getLabel.Job}
\alias{getLabel,Job-method}


\title{Gets the label of the job}

\description{
 Gets the label of the job.

 If the label is not set, a default string of format
 \code{<jobname>_<user>@<host>} will be returned.
}

\usage{\method{getLabel}{Job}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
 Returns a \code{\link[base]{character}} string.
}

\seealso{
  \code{\link[R.batch:getName.Job]{*getName}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
