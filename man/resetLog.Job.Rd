%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{resetLog.Job}
\alias{resetLog.Job}
\alias{Job.resetLog}
\alias{resetLog.Job}
\alias{resetLog,Job-method}


\title{Reset log by removing log file}

\description{
 Reset log by removing log file.
}

\usage{\method{resetLog}{Job}(this, backup=TRUE, ...)}

\arguments{
 \item{backup}{If \code{\link[base:logical]{TRUE}}, log file is backuped before deleted.}
 \item{...}{Not used.}
}

\value{
 Returns \code{\link[base:logical]{TRUE}} if log file was reset, otherwise \code{\link[base:logical]{FALSE}}.
}

\seealso{
  \code{\link[R.batch:writeToLog.Job]{*writeToLog}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
