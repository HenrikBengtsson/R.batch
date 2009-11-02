%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{main.JobBatch}
\alias{main.JobBatch}
\alias{JobBatch.main}
\alias{main.JobBatch}
\alias{main,JobBatch-method}


\title{Static method to be called from the command line}

\description{
 Static method to be called from the command line.
 When called, this method will process one available job and return.\cr

 The following \R command line options are recognized:
 \itemize{
  \item \code{--root=<path>} or \code{--root <path>} specifies the
        root path of the batch directory.
  \item \code{--reset} specifies if each job should be reset before
        it is started.
  \item \code{--sink} specifies if each job output should be sinked
        to file.
  \item \code{--details} specifies if extra information should be
        printed to the standard output.
  \item \code{--maxJobs} specifies the \emph{maximum} number of jobs
        this batch dispatcher should process before quiting.
        If \code{-1}, it runs until no more jobs are found.
        If \code{Inf}, it runs forever until being interrupted.
  \item \code{--sleepTime} specifies the number of seconds for \R
        to sleep before checking for new jobs, when no more jobs are
        available. Only effective if \code{--maxJobs=Inf}.
  \item \code{--clean} specifies if all job specific created in the
        output path should be moved to the job path when job is done.
 }

 To avoid warning about unknown options when \R, add these options
 at end after \code{--args}. See example below.\cr

 To run this from the command line, see \code{\link{1. Useful scripts}}.
}

\synopsis{main.JobBatch(static, root="jobs", reset=FALSE, sink=TRUE, details=FALSE, maxJobs=1, sleepTime=15, clean=FALSE, ...)}
\usage{JobBatch$main(root="jobs", reset=FALSE, sink=TRUE, details=FALSE, maxJobs=1, sleepTime=15, clean=FALSE, ...)}

\arguments{
 \item{root}{A \code{\link[base]{character}} string specifying the default value for the
   command line option \code{--root}.}
 \item{reset}{A \code{\link[base]{logical}} value specifying the default value for the
   command line option \code{--reset}.}
 \item{sink}{A \code{\link[base]{logical}} value specifying the default value for the
   command line option \code{--sink}.}
 \item{details}{A \code{\link[base]{logical}} value specifying the default value for the
   command line option \code{--details}.}
 \item{maxJobs}{A \code{\link[base]{integer}} (or the double \code{\link[base:is.finite]{Inf}}) value specifying the
   default value for the command line option \code{--maxjobs}.}
 \item{sleepTime}{A \code{\link[base]{double}} value specifying the
   default value for the command line option \code{--sleepTime}.}
 \item{clean}{If \code{\link[base:logical]{TRUE}}, job specific files in the output path are
   moved to the job path after job is finished.}
 \item{...}{Not used.}
}

\value{
 Returns (invisibly) the \code{\link{Job}} object processed, otherwise \code{\link[base]{NULL}}.
}

\examples{\dontrun{
  RJobBatch --details --root=jobs-mandelbrot
}}

\seealso{
  For more information see \code{\link{JobBatch}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
