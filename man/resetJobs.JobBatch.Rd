%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{resetJobs.JobBatch}
\alias{resetJobs.JobBatch}
\alias{JobBatch.resetJobs}
\alias{resetJobs.JobBatch}
\alias{resetJobs,JobBatch-method}


 \title{Resets Jobs in the job batch}

 \description{
  Resets Jobs in the job batch by unlocking each job and moving it back to the todo
  directory.
 }

 \usage{\method{resetJobs}{JobBatch}(this, jobs=NULL, ...)}

 \arguments{
  \item{jobs}{A \code{\link[base]{list}} of Jobs or a single Job to be reset.}
  \item{...}{Not used.}
 }

 \value{
  Returns nothing.
 }

 \examples{\dontrun{
   # Resets all jobs in job batch!
   resetJobs(batch, findJobs(batch))
 }}

 \seealso{
   For more information see \code{\link{JobBatch}}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
