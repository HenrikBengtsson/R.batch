%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{isSinked.Job}
\alias{isSinked.Job}
\alias{Job.isSinked}
\alias{isSinked.Job}
\alias{isSinked,Job-method}


\title{Checks if job output is sinked to file}

\description{
 Checks if job output is sinked to file.
}

\usage{\method{isSinked}{Job}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
 Returns \code{\link[base:logical]{TRUE}} if either output or message is sinked, otherwise \code{\link[base:logical]{FALSE}}.
}

\seealso{
  \code{\link[R.batch:sink.Job]{*sink}()} and \code{\link[R.batch:unsink.Job]{*unsink}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
