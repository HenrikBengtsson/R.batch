%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{sink.Job}
\alias{sink.Job}
\alias{Job.sink}
\alias{sink.Job}
\alias{sink,Job-method}


\title{Sinks job output}

\description{
 Sinks job output.
}

\usage{\method{sink}{Job}(this, output=TRUE, message=TRUE, split=TRUE, path=getOutputPath(this), ...)}

\arguments{
 \item{output}{If \code{\link[base:logical]{TRUE}}, standard output is sinked, otherwise not.}
 \item{message}{If \code{\link[base:logical]{TRUE}}, standard error is sinked, otherwise not.
               \emph{Currently ignored!}}
 \item{split}{If \code{\link[base:logical]{TRUE}}, output will be sent to the sink and to the
             current output stream. The standard error cannot be split.}
 \item{path}{Path (directory) for the sink file. If \code{\link[base]{NULL}}, the current
             (job) directory is used.}
 \item{...}{Not used.}
}

\value{
 Returns nothing.
}

\seealso{
  \code{\link[R.batch:isSinked.Job]{*isSinked}()} and \code{\link[R.batch:unsink.Job]{*unsink}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
