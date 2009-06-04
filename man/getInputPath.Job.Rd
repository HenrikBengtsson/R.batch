%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{getInputPath.Job}
\alias{getInputPath.Job}
\alias{Job.getInputPath}
\alias{getInputPath.Job}
\alias{getInputPath,Job-method}


 \title{Gets the input path of the job}

 \description{
  Gets the input path of the job.
  It is recommended to put data files here, which should be read
  by the job.

  The input path should be named \code{"input/"}. In order to read data
  from two different directories, additional input paths may be
  \code{"input2/"}, \code{"input3/"} etc. These are specified using the
  \code{index} argument. This makes it possible to "import" data from
  the output of various other batch jobs via links.
 }

 \usage{\method{getInputPath}{Job}(this, index=1, ...)}

 \arguments{
  \item{index}{An \code{\link[base]{integer}} specifying which input path to query.}
  \item{...}{Not used.}
 }

 \value{
  Returns a \code{\link[base]{character}} string.
 }

 \seealso{
   For more information see \code{\link{Job}}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
