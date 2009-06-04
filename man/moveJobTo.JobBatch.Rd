%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{moveJobTo.JobBatch}
\alias{moveJobTo.JobBatch}
\alias{JobBatch.moveJobTo}
\alias{moveJobTo.JobBatch}
\alias{moveJobTo,JobBatch-method}


 \title{Moves the job to another directory}

 \description{
  Moves the job to another directory.
 }

 \usage{\method{moveJobTo}{JobBatch}(this, job, dest=c("todo", "running", "finished", "failed", "interrupted", "erroneous"), ...)}

 \arguments{
  \item{dest}{A \code{\link[base]{character}} string specifying the destination directory
    of the job.}
  \item{...}{Not used.}
 }

 \value{
  Returns a \code{\link[base:logical]{TRUE}} if successful, otherwise \code{\link[base:logical]{FALSE}}.
 }

 \details{
  If job does not exist, is locked or destination does not exists, an
  exception is thrown.
 }

 \seealso{
   For more information see \code{\link{JobBatch}}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
