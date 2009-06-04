%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{unsink.Job}
\alias{unsink.Job}
\alias{Job.unsink}
\alias{unsink.Job}
\alias{unsink,Job-method}


\title{Unsinks job output}

\description{
 Unsinks job output.
}

\usage{\method{unsink}{Job}(this, output=TRUE, message=TRUE, ...)}

\arguments{
 \item{output}{If \code{\link[base:logical]{TRUE}}, standard output is unsinked, otherwise not.}
 \item{message}{If \code{\link[base:logical]{TRUE}}, standard error is unsinked, otherwise not.}
 \item{...}{Not used.}
}

\value{
 Returns nothing.
}

\seealso{
  \code{\link[R.batch:isSinked.Job]{*isSinked}()} and \code{\link[R.batch:sink.Job]{*sink}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
