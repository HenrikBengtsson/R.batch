%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{setupDemo.JobBatch}
\alias{setupDemo.JobBatch}
\alias{JobBatch.setupDemo}
\alias{setupDemo.JobBatch}
\alias{setupDemo,JobBatch-method}


\title{Static method to setup a demo job batch directory structure}

\description{
 Static method to setup a demo job batch directory structure.
}

\synopsis{setupDemo.JobBatch(static, demo=c("mandelbrot"), overwrite=FALSE, ...)}
\usage{JobBatch$setupDemo(demo=c("mandelbrot"), overwrite=FALSE, ...)}

\arguments{
 \item{demo}{The demo to be setup.}
 \item{overwrite}{If \code{\link[base:logical]{TRUE}}, any preexisting directory will be removed,
   otherwise an exception is thrown.}
 \item{...}{Not used.}
}

\value{
 Returns (invisibly) the path to the created directory, which has
 the base name \code{jobs-<demo>}, e.g. \code{demo-mandelbrot}.
}

\seealso{
  For more information see \code{\link{JobBatch}}.
}

\details{
  The Mandelbrot demo was adopted from code by Martin Maechler. See
  files in \code{system.file("jobs-mandelbrot/src", package="R.batch")}
  for details.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}



\keyword{internal}
\keyword{methods}
\keyword{programming}
