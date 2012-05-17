%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  JobBatch.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{JobBatch}
\docType{class}
\alias{JobBatch}


 \title{Class representing a batch job}

 \description{
  Package:  R.batch \cr
\bold{Class JobBatch}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{JobBatch}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{JobBatch}\cr
extends \link[R.oo]{Object}\cr



  Class representing a batch job.
  A \code{JobBatch} has one or several \code{\link{Job}}:s.

  To run batch jobs, most often this class the only one needed.
  The \code{\link{Job}} class is only to investigate details about a specific job.
 }

 \usage{JobBatch(root="jobs", ...)}

 \arguments{
  \item{root}{A name of a job root directory.}
  \item{...}{Not used.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.batch:as.character.JobBatch]{as.character}} \tab Gets a character string representation of the job batch.\cr
 \tab \code{\link[R.batch:checkRequirements.JobBatch]{checkRequirements}} \tab Checks that requirements are fulfilled or not.\cr
 \tab \code{\link[R.batch:clean.JobBatch]{clean}} \tab Cleans up among jobs in this JobBatch.\cr
 \tab \code{\link[R.batch:copyFrom.JobBatch]{copyFrom}} \tab Copies a job batch directory into this one.\cr
 \tab \code{\link[R.batch:createStub.JobBatch]{createStub}} \tab Creates a jobs directory structure stub.\cr
 \tab \code{\link[R.batch:findJobs.JobBatch]{findJobs}} \tab Searches by name for Job:s in this JobBatch.\cr
 \tab \code{\link[R.batch:getDirectory.JobBatch]{getDirectory}} \tab Gets a subdirectory of the job batch.\cr
 \tab \code{getErroneousPath} \tab  -\cr
 \tab \code{getFailedPath} \tab  -\cr
 \tab \code{getFinishedPath} \tab  -\cr
 \tab \code{getInputPath} \tab  -\cr
 \tab \code{getInterruptedPath} \tab  -\cr
 \tab \code{\link[R.batch:getNextJob.JobBatch]{getNextJob}} \tab Gets next non-locked job.\cr
 \tab \code{getOutputPath} \tab  -\cr
 \tab \code{\link[R.batch:getRoot.JobBatch]{getRoot}} \tab Gets the root path of the job batch.\cr
 \tab \code{\link[R.batch:getRunAndFinishJob.JobBatch]{getRunAndFinishJob}} \tab Gets an non-locked job, process it and moves it to a final destination.\cr
 \tab \code{getRunningPath} \tab  -\cr
 \tab \code{getSrcPath} \tab  -\cr
 \tab \code{\link[R.batch:getSummary.JobBatch]{getSummary}} \tab Gets a summary of the jobs directory.\cr
 \tab \code{getTodoPath} \tab  -\cr
 \tab \code{\link[R.batch:main.JobBatch]{main}} \tab Static method to be called from the command line.\cr
 \tab \code{\link[R.batch:moveJobTo.JobBatch]{moveJobTo}} \tab Moves the job to another directory.\cr
 \tab \code{\link[R.batch:print.JobBatch]{print}} \tab Prints a summary of the jobs directory.\cr
 \tab \code{\link[R.batch:resetJobs.JobBatch]{resetJobs}} \tab Resets Jobs in the job batch.\cr
 \tab \code{\link[R.batch:run.JobBatch]{run}} \tab Process some or all jobs available.\cr
 \tab \code{setRoot} \tab  -\cr
 \tab \code{\link[R.batch:setupDemo.JobBatch]{setupDemo}} \tab Static method to setup a demo job batch directory structure.\cr
 \tab \code{\link[R.batch:validate.JobBatch]{validate}} \tab Validates the job batch.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \details{
  When a job is \emph{processed} then following happens in order:
  \enumerate{
   \item A non-locked job from the "todo/" directory will be retrieved.
   \item The job will be moved to the "running/" directory.
   \item The job will be locked (a lock file is created and opened).
   \item If any of the above fails, \code{\link[base]{NULL}} is returned.
   \item The job is initiated; source code in the "src/" directory
         followed by the job directory is loaded.
         Here \code{onRun()} must be defined. All other \code{onNNN()}
         functions maybe be redefined, otherwise default ones are used.
         If there is syntax error in the source code, the job is moved to
         the "erroneous/" directory.
   \item The working directory is set to the directory of the job.
   \item If a stored image (typically from a previously interrupted
         job) is detected, it is loaded into the current job and
         onRestart() is called.
   \item The job is started and \code{onStart()} is called.
   \item \code{onRun()} is called.
   \item If sucessful, the job is moved to "finished/" and is unlocked
         (the lock file is removed).
   \item The \code{\link{Job}} object that was processed is returned.
  }

  In addition, for step 7-9:
  If an error occurs, \code{onError()} followed by \code{onFinally()}
  are called and the job is moved to the "failed/" directory.
  If an interrupt occurs, \code{onInterrupt()} followed by
  \code{onFinally()} are called and the job is moved to the "interrupted/"
  directory. By default, \code{onInterrupt()} save an image of the job,
  by calling \code{saveImage(job)}.
  In any case the job will be unlock and returned.

  Note that, if the job directory is "locked" by another process, which can
  happen if someone browser the job directory or similar, it cannot be moved.
  If this happends when a job is moved to another directory, the move
  operation will be tried 10 times every 10 seconds. If the job was not moved
  an error is generated (and the job remains in its current directory).
 }

 \examples{
# To prevent R from asking us to press ENTER before each image
options(device.ask.default=FALSE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Setup a demo job directory
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rootPath <- JobBatch$setupDemo("mandelbrot", overwrite=TRUE)
batch <- JobBatch(rootPath)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Now, imaging that this code was running on several
# different host all with access to the job directory.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
print(batch)

# Process jobs until no more exist or no more
# can be retrieved.
run(batch, verbose=-150)

print(batch)

print(list.files(getOutputPath(batch)))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ONLY FOR REDUNDANCY TESTING OF THE PACKAGE
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Assert that jobs in batch ended up where expected.
cat("Validation JobBatch results...\n");
expected <- list(erroneous="job05", failed="job04", interrupted=NULL,
                    finished=c("job01", "job02", "job03"), running=NULL);
for (name in names(expected)) {
  jobs <- getSummary(batch)[[name]]$jobs;
  if (!identical(jobs, expected[[name]])) {
    msg <- paste("Final directory '", name,
                 "' does not contain expected jobs: ",
                 paste(jobs, collapse=", "), " != ",
                 paste(expected[[name]], collapse=","));
    cat(msg, "\n");
    stop(msg);
  }
}
cat("Validation JobBatch results...done\n");
}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{classes}
\keyword{programming}
