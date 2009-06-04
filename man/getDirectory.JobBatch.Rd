%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getDirectory.JobBatch}
\alias{getDirectory.JobBatch}
\alias{JobBatch.getDirectory}
\alias{getDirectory.JobBatch}
\alias{getDirectory,JobBatch-method}

 \alias{JobBatch.getTodoPath}
\alias{getTodoPath.JobBatch}
\alias{getTodoPath,JobBatch-method}

 \alias{JobBatch.getRunningPath}
\alias{getRunningPath.JobBatch}
\alias{getRunningPath,JobBatch-method}

 \alias{JobBatch.getFinishedPath}
\alias{getFinishedPath.JobBatch}
\alias{getFinishedPath,JobBatch-method}

 \alias{JobBatch.getFailedPath}
\alias{getFailedPath.JobBatch}
\alias{getFailedPath,JobBatch-method}

 \alias{JobBatch.getInterruptedPath}
\alias{getInterruptedPath.JobBatch}
\alias{getInterruptedPath,JobBatch-method}

 \alias{JobBatch.getErroneousPath}
\alias{getErroneousPath.JobBatch}
\alias{getErroneousPath,JobBatch-method}

 \alias{JobBatch.getSrcPath}
\alias{getSrcPath.JobBatch}
\alias{getSrcPath,JobBatch-method}

 \alias{JobBatch.getOutputPath}
\alias{getOutputPath.JobBatch}
\alias{getOutputPath,JobBatch-method}

 \alias{JobBatch.getInputPath}
\alias{getInputPath.JobBatch}
\alias{getInputPath,JobBatch-method}


 \title{Gets a subdirectory of the job batch}

 \description{
  Gets a subdirectory of the job batch.
 }

 \usage{\method{getDirectory}{JobBatch}(this, dir=c("todo", "running", "finished", "failed", "interrupted", "erroneous", "src", "input", "output"), ...)}

 \arguments{
  \item{dir}{A name of a subdirectory.}
  \item{...}{Not used.}
 }

 \value{
  Returns a \code{\link[base]{character}} string.
 }

 \seealso{
   For more information see \code{\link{JobBatch}}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
