%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{copyFrom.JobBatch}
\alias{copyFrom.JobBatch}
\alias{JobBatch.copyFrom}
\alias{copyFrom.JobBatch}
\alias{copyFrom,JobBatch-method}


\title{Copies a job batch directory into this one}

\description{
 Copies a job batch directory into this one.
}

\usage{\method{copyFrom}{JobBatch}(this, batch, conflicts=c("skip", "overwrite", "error"), ...)}

\arguments{
 \item{batch}{Another \code{\link{JobBatch}} object.}
 \item{conflicts}{A \code{\link[base]{character}} string specifying how to handle already
   existing files. If \code{"error"}, an error is thrown.
   If \code{"skip"}, the file is quietly skipped.
   If \code{"overwrite"}, the file is quietly overwritten.}
 \item{...}{Arguments passed to \code{\link[R.batch:copyFrom.JobBatch]{*copyFrom}()}.}
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
