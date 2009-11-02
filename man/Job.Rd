%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Job.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{Job}
\docType{class}
\alias{Job}


\title{Class representing a batch job}

\description{
 Package:  R.batch \cr
\bold{Class Job}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{Job}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{Job}\cr
extends \link[R.oo]{Object}\cr



 Class representing a batch job.

 A \code{Job} belongs to a \code{\link{JobBatch}}.
}

\usage{Job(jobPath=NULL, label=NULL, verbose=FALSE)}

\arguments{
  \item{jobPath}{The job path.}
  \item{label}{An optional \code{\link[base]{character}} string specifying the label (name)
    of the job.}
  \item{verbose}{If \code{\link[base:logical]{TRUE}}, detailed information will be given while
    working on the job.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{addPersistentField} \tab  -\cr
 \tab \code{\link[R.batch:as.character.Job]{as.character}} \tab Gets a character string representation of the job.\cr
 \tab \code{backupLog} \tab  -\cr
 \tab \code{\link[R.batch:equals.Job]{equals}} \tab Checks if this job equals another.\cr
 \tab \code{\link[R.batch:finalize.Job]{finalize}} \tab Finalizes job.\cr
 \tab \code{getDependencies} \tab  -\cr
 \tab \code{getDependenciesRaw} \tab  -\cr
 \tab \code{getField} \tab  -\cr
 \tab \code{getFigurePath} \tab  -\cr
 \tab \code{\link[R.batch:getInputPath.Job]{getInputPath}} \tab Gets the input path of the job.\cr
 \tab \code{\link[R.batch:getLabel.Job]{getLabel}} \tab Gets the label of the job.\cr
 \tab \code{getLog} \tab  -\cr
 \tab \code{getLogPath} \tab  -\cr
 \tab \code{\link[R.batch:getName.Job]{getName}} \tab Gets the name of the job.\cr
 \tab \code{\link[R.batch:getOutputPath.Job]{getOutputPath}} \tab Gets the output path of the job.\cr
 \tab \code{\link[R.batch:getPath.Job]{getPath}} \tab Gets the path to the job.\cr
 \tab \code{getPersistentPath} \tab  -\cr
 \tab \code{getResultPath} \tab  -\cr
 \tab \code{getRoot} \tab  -\cr
 \tab \code{\link[R.batch:getStatus.Job]{getStatus}} \tab Gets the status of the job.\cr
 \tab \code{\link[R.batch:hasFailed.Job]{hasFailed}} \tab Checks if the job failed.\cr
 \tab \code{\link[R.batch:hasStoredImage.Job]{hasStoredImage}} \tab Checks if a stored job image exists.\cr
 \tab \code{\link[R.batch:isDone.Job]{isDone}} \tab Checks if the job is done.\cr
 \tab \code{\link[R.batch:isErroneous.Job]{isErroneous}} \tab Checks if the job is erroneous.\cr
 \tab \code{\link[R.batch:isExisting.Job]{isExisting}} \tab Checks if the job exists.\cr
 \tab \code{isFinished} \tab  -\cr
 \tab \code{\link[R.batch:isLocked.Job]{isLocked}} \tab Checks if a job is locked.\cr
 \tab \code{isNewer} \tab  -\cr
 \tab \code{isPersistentField} \tab  -\cr
 \tab \code{\link[R.batch:isSinked.Job]{isSinked}} \tab Checks if job output is sinked to file.\cr
 \tab \code{\link[R.batch:isStarted.Job]{isStarted}} \tab Checks if the job is started or not.\cr
 \tab \code{lastModified} \tab  -\cr
 \tab \code{listDir} \tab  -\cr
 \tab \code{\link[R.batch:loadStoredImage.Job]{loadStoredImage}} \tab Reload a stored job image.\cr
 \tab \code{\link[R.batch:lock.Job]{lock}} \tab Locks the job.\cr
 \tab \code{moveOutputFilesTo} \tab  -\cr
 \tab \code{\link[R.batch:removeStoredImage.Job]{removeStoredImage}} \tab Removes stored job image.\cr
 \tab \code{\link[R.batch:resetLog.Job]{resetLog}} \tab Reset log by removing log file.\cr
 \tab \code{resetToRun} \tab  -\cr
 \tab \code{\link[R.batch:run.Job]{run}} \tab Runs the job.\cr
 \tab \code{\link[R.batch:saveImage.Job]{saveImage}} \tab Save an image of the job.\cr
 \tab \code{setField} \tab  -\cr
 \tab \code{setFigurePath} \tab  -\cr
 \tab \code{\link[R.batch:setLabel.Job]{setLabel}} \tab Sets the label of the job.\cr
 \tab \code{setOutputPath} \tab  -\cr
 \tab \code{setPath} \tab  -\cr
 \tab \code{\link[R.batch:setStatus.Job]{setStatus}} \tab Sets the status of the job.\cr
 \tab \code{\link[R.batch:setup.Job]{setup}} \tab Initiates the job.\cr
 \tab \code{\link[R.batch:showWarnings.Job]{showWarnings}} \tab Displays warning generated while running this job.\cr
 \tab \code{\link[R.batch:sink.Job]{sink}} \tab Sinks job output.\cr
 \tab \code{sourceDirectoryWithPreprocessor} \tab  -\cr
 \tab \code{\link[R.batch:sourceHotCode.Job]{sourceHotCode}} \tab Sources and removes code in the hot/ and src/hot/ directories.\cr
 \tab \code{touch} \tab  -\cr
 \tab \code{\link[R.batch:unlock.Job]{unlock}} \tab Unlocks the job.\cr
 \tab \code{\link[R.batch:unsink.Job]{unsink}} \tab Unsinks job output.\cr
 \tab \code{untouch} \tab  -\cr
 \tab \code{\link[R.batch:wasInterrupted.Job]{wasInterrupted}} \tab Checks if the job was interrupted.\cr
 \tab \code{\link[R.batch:wasSuccessful.Job]{wasSuccessful}} \tab Checks if the job was completed successfully.\cr
 \tab \code{\link[R.batch:writeToLog.Job]{writeToLog}} \tab Writes to log file.\cr
}


 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}


\keyword{classes}
\keyword{programming}
