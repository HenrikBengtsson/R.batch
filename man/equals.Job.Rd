%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{equals.Job}
\alias{equals.Job}
\alias{Job.equals}
\alias{equals.Job}
\alias{equals,Job-method}


 \title{Checks if this job equals another}

 \description{
   Checks if this job equals another.
 }

 \usage{\method{equals}{Job}(this, other, ...)}

 \arguments{
  \item{other}{Another Job.}
  \item{...}{Not used.}
 }

 \value{
   Returns \code{\link[base:logical]{TRUE}} if the jobs are equal, otherwise \code{\link[base:logical]{FALSE}}.
 }

 \details{
  If the other object is not a Job object, this method returns \code{\link[base:logical]{FALSE}}.
  Otherwise, \code{\link[R.batch:getPath.Job]{*getPath}()} is called on both objects and these are
  compared with \code{\link[base]{identical}}().
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   For more information see \code{\link{Job}}.
 }
\keyword{internal}
\keyword{methods}
