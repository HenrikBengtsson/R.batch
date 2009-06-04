%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{sourceHotCode.Job}
\alias{sourceHotCode.Job}
\alias{Job.sourceHotCode}
\alias{sourceHotCode.Job}
\alias{sourceHotCode,Job-method}


\title{Sources and removes code in the hot/ and src/hot/ directories}

\description{
 Sources and removes code in the hot/ and src/hot/ directories, that is, directories named hot/ in both the current
 job directory as well as the common src/ directory, making it possible
 either patch specific jobs or all jobs. If not put in a directory
 named 'global', the code will be sourced into the calling environment,
 which typically is "inside" the \code{onRun()} function.

 \emph{WARNING: By default, hot plugin files that were sourced, are
 removed afterward!}

 By calling this method repetably in \code{onRun(job)},
 say, in the main iteration loop, it is possible to update code while
 a job is running.
 One situation where this has been found to be useful is when it takes
 literally days to process a job and when almost done you have been
 told that there will be a power-shut before you job finishes. By
 plugging in new code this way, you can save you current session and
 shut it down nicely to be continued later. Note, there is currently
 now methods provided by \code{R.batch} that does this for you; you
 have to implement reloading etc. yourself.

 Errors that occurs while sourcing the hot code, for instance syntax
 errors but also evaluation errors, are \emph{skipped} with a warning
 and recorded in the log file. This way you will not killa process by
 misstake that have been running for days.

 Note that code in hot/ and src/hot/ will also be source by
 \code{\link[R.batch:setup.Job]{*setup}()} on startup.
}

\usage{\method{sourceHotCode}{Job}(this, remove=TRUE, envir=this$.env, ...)}

\arguments{
 \item{remove}{If \code{\link[base:logical]{TRUE}}, hot patch files will be removed after being
       read, otherwise not.}
 \item{envir}{The \code{\link[base]{environment}} where the source code should be stored.}
 \item{...}{Not used.}
}

\value{
 Returns (invisibly) a \code{\link[base]{vector}} of the files sourced.
}

\details{
 All scripts files are evaluated with \code{source()} to the
 \emph{local} working environment of this job. That is, no global
 objects will be overwritten.
}

\seealso{
  \code{\link[R.batch:run.Job]{*run}()}.
  For more information see \code{\link{Job}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
