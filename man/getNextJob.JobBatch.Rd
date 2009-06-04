%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getNextJob.JobBatch}
\alias{getNextJob.JobBatch}
\alias{JobBatch.getNextJob}
\alias{getNextJob.JobBatch}
\alias{getNextJob,JobBatch-method}


 \title{Gets next non-locked job}

 \description{
  Gets next non-locked job and locks it.

  This method is called by \code{\link[R.batch:getRunAndFinishJob.JobBatch]{*getRunAndFinishJob}()}.
 }

 \usage{\method{getNextJob}{JobBatch}(this, which=c("first", "last", "random"), ..., verbose=FALSE)}

 \arguments{
  \item{which}{A \code{\link[base]{character}} string. The jobs are sorted in lexicographical
    order by the directory names.
    If \code{"first"}, then the first non-locked job is retrieved.
    If \code{"last"}, then the last non-locked job is retrieved.
    If \code{"ranodm"}, a random non-locked job is retrieved.
  }
  \item{...}{Arguments, except \code{jobPath} and \code{verbose} passed
    to the constructor of \code{\link{Job}}.}
  \item{verbose}{If \code{\link[base:logical]{TRUE}}, extra information is displayed. The created
    Job object gets this verbose level too.}
 }

 \value{
  Returns a \code{\link{Job}} after first locking it.
  If no available job was found, \code{\link[base]{NULL}} is silently returned.
 }

 \seealso{
   For more information see \code{\link{JobBatch}}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
