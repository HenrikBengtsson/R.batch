%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{run.Job}
\alias{run.Job}
\alias{Job.run}
\alias{run.Job}
\alias{run,Job-method}


 \title{Runs the job}

 \description{
  Runs the job.
 }

 \usage{\method{run}{Job}(this, reset=FALSE, sink=TRUE, ...)}

 \arguments{
  \item{reset}{If \code{\link[base:logical]{TRUE}}, job is first reset, otherwise not.}
  \item{sink}{If \code{\link[base:logical]{TRUE}}, all output is sinked to a file, otherwise not.}
  \item{...}{Not used.}
 }

 \value{
  Returns a \code{\link[base:logical]{TRUE}} if job finished successfully, otherwise \code{\link[base:logical]{FALSE}}.
 }

 \details{
  First the job is locked. Then \code{\link[R.batch:setup.Job]{*setup}()} is called, and the
  current working directory is set to the job path, that is, any script
  or functions calls are evaluated within the job directory.
  Then the handle method \code{onStart()} followed by \code{onRun()} are
  evaluated with this object as the first argument.
  If an error occurs while evaluating these, it is caught and
  \code{onError()} is evaluated.
  Similar, if an interrupt, that is Ctrl-C (\code{SIGINT}), occurs it is
  caught and \code{onInterrupt()} is evaluated.
  Finally, handle method \code{onFinally()} is (always) evaluated.
  Errors or interrupts occuring while evaluating this latter method, will
  \emph{not} call \code{onInterrupt()} and \code{onError()},
  respectively.

  Note also, that if errors or additional interrupts occurs while
  evaluating \code{onInterrupt()} or \code{onError()} these will
  \emph{not} be caught. This can be an issue if for instance the user
  holds down Ctrl-C. Unfortunately, there is no solution to the problem
  at the moment [1].
 }

 \references{
   [1] H. Bengtsson, \emph{Exception handling in R}, 2004.
       \url{http://www.maths.lth.se/help/R/}
 }

 \seealso{
   \code{\link[R.batch:lock.Job]{*lock}()}.
   \code{\link[R.batch:setup.Job]{*setup}()}.
   \code{\link[R.batch:isExisting.Job]{*isExisting}()}.
   \code{\link[R.batch:isStarted.Job]{*isStarted}()}.
   For more information see \code{\link{Job}}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{internal}
\keyword{methods}
\keyword{programming}
