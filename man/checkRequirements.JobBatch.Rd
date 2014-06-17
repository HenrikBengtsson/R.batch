%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{JobBatch$checkRequirements}
\alias{JobBatch$checkRequirements}
\alias{checkRequirements.JobBatch}
\alias{JobBatch.checkRequirements}
\alias{checkRequirements,JobBatch-method}

 \title{Checks that requirements are fulfilled or not}

 \description{
  Checks that requirements are fulfilled or not.

  This method is called by \code{\link[R.batch:getNextJob.JobBatch]{*getNextJob}()}.
 }

 \usage{
## Static method (use this):
## JobBatch$checkRequirements(path, ...)

## Don't use the below:
\method{checkRequirements}{JobBatch}(static, path, ...)
}

 \arguments{
  \item{path}{A \code{\link[base]{character}} string of the path where to search for
    requirement files. All subdirectories are searched too.}
  \item{...}{Not used.}
 }

 \value{
  Returns NULL if all requirement files gives \code{\link[base:logical]{TRUE}}, otherwise the
  pathname (a \code{\link[base]{character}} string) of the first failed requirement file.
 }

 \seealso{
   For more information see \code{\link{JobBatch}}.
 }

 \author{Henrik Bengtsson}


\keyword{internal}
\keyword{methods}
\keyword{programming}
