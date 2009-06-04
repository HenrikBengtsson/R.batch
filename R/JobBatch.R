###########################################################################/**
# @RdocClass JobBatch
#
# @title "Class representing a batch job"
#
# \description{
#  @classhierarchy
#
#  @get "title".
#  A \code{JobBatch} has one or several @see "Job":s.
#  
#  To run batch jobs, most often this class the only one needed. 
#  The @see "Job" class is only to investigate details about a specific job.
# }
#
# @synopsis
#
# \arguments{
#  \item{root}{A name of a job root directory.}
#  \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods  
# }
#
# \details{
#  When a job is \emph{processed} then following happens in order:
#  \enumerate{
#   \item A non-locked job from the "todo" directory will be retrieved.
#   \item The job will be moved to the "running" directory.
#   \item The job will be locked (a lock file is created and opened).
#   \item If any of the above fails, @NULL is returned.
#   
#   \item The job is initiated; source code in the "src" directory
#         followed by the job directory is loaded. 
#         Here \code{onRun()} must be defined. All other \code{onNNN()}
#         functions maybe be redefined, otherwise default ones are used.
#         If there is syntax error in the source, the job is moved to 
#         the "erroneous" directory.
#   \item The working directory is set to the directory of the job.
#   \item If a stored image (typically from a previously interrupted
#         job) is detected, it is loaded into the current job and
#         onRestart() is called. 
#   \item The job is started and \code{onStart()} is called.
#   \item \code{onRun()} is called.
#   \item If sucessful, the job is moved to "finished" and is unlocked
#         (the lock file is removed).
#   \item The @see "Job" object that was processed is returned.
#  }
#
#  In addition, for step 7-9:
#  If an error occurs, \code{onError()} followed by \code{onFinally()} 
#  are called and the job is moved to the "failed" directory. 
#  If an interrupt occurs, \code{onInterrupt()} followed by 
#  \code{onFinally()} are called and the job is moved to the "interrupted" 
#  directory. By default, \code{onInterrupt()} save an image of the job,
#  by calling \code{saveImage(job)}.
#  In any case the job will be unlock and returned.
#
#  Note that, if the job directory is "locked" by another process, which can
#  happen if someone browser the job directory or similar, it cannot be moved.
#  If this happends when a job is moved to another directory, the move 
#  operation will be tried 10 times every 10 seconds. If the job was not moved
#  an error is generated (and the job remains in its current directory).
# }
#
# @examples "../incl/JobBatch.Rex"
#
# @author
#
# @keyword programming
#*/###########################################################################
setConstructorS3("JobBatch", function(root="jobs", ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (!is.null(root)) {
    # Expand Windows shortcuts, if they exists along the pathname.
    root <- filePath(root, expandLinks="any");
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Create object
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  extend(Object(), "JobBatch",
    # Paths
    .root = root
  )
})



########################################################################/**
# @RdocMethod as.character
#
# @title "Gets a character string representation of the job batch"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character string.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
# 
# @keyword programming
#**/#######################################################################
setMethodS3("as.character", "JobBatch", function(x, ...) {
  # To please R CMD check
  this <- x;

  s <- paste(class(this)[1], ": ", sep="");
  s <- paste(s, "Root path is '", this$.root, "'.", sep="");
  s;
})



########################################################################/**
# @RdocMethod print
#
# @title "Prints a summary of the jobs directory"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns nothing.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("print", "JobBatch", function(x, ...) {
  # To please R CMD check...
  this <- x;

  res <- getSummary(this);

  cat("Jobs root: ", getRoot(this), "\n", sep="")
  dir <- "todo";
  cat("Number of jobs to do       : ", res[[dir]]$nbrOfJobs, "\n", sep="")
  if (length(res[[dir]]$jobs) > 0)
    cat("(", paste(res[[dir]]$jobsStr, collapse=", "), ")\n", sep="");

  dir <- "running";
  cat("Number of running jobs     : ", res[[dir]]$nbrOfJobs, "\n", sep="")
  if (length(res[[dir]]$jobs) > 0)
    cat("(", paste(res[[dir]]$jobsStr, collapse=", "), ")\n", sep="");

  dir <- "finished";
  cat("Number of finished jobs    : ", res[[dir]]$nbrOfJobs, "\n", sep="")
  if (length(res[[dir]]$jobs) > 0)
    cat("(", paste(res[[dir]]$jobsStr, collapse=", "), ")\n", sep="");

  dir <- "interrupted";
  cat("Number of interrupted jobs : ", res[[dir]]$nbrOfJobs, "\n", sep="")
  if (length(res[[dir]]$jobs) > 0)
    cat("(", paste(res[[dir]]$jobsStr, collapse=", "), ")\n", sep="");

  dir <- "failed";
  cat("Number of failed jobs      : ", res[[dir]]$nbrOfJobs, "\n", sep="")
  if (length(res[[dir]]$jobs) > 0)
    cat("(", paste(res[[dir]]$jobsStr, collapse=", "), ")\n", sep="");

  dir <- "erroneous";
  cat("Number of erroneous jobs   : ", res[[dir]]$nbrOfJobs, "\n", sep="")
  if (length(res[[dir]]$jobs) > 0)
    cat("(", paste(res[[dir]]$jobsStr, collapse=", "), ")\n", sep="");

  dir <- "output";
  cat("Number of output files     : ", res[[dir]]$nbrOfFiles, "\n", sep="")

  if (length(res$duplicatedJobs) > 0) {
    cat("\nWARNING: Found duplicated jobs. These are marked with an asterisk above.\n");
  }

  invisible();
})


########################################################################/**
# @RdocMethod getRoot
# @aliasmethod "setRoot"
#
# @title "Gets the root path of the job batch"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character string.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
# 
# @keyword programming
#**/#######################################################################
setMethodS3("getRoot", "JobBatch", function(this, ...) {
  this$.root;
})


setMethodS3("setRoot", "JobBatch", function(this, ...) {
  throw("The job root must not be changed.");
})



########################################################################/**
# @RdocMethod getDirectory
# @aliasmethod "getTodoPath"
# @aliasmethod "getRunningPath"
# @aliasmethod "getFinishedPath"
# @aliasmethod "getFailedPath"
# @aliasmethod "getInterruptedPath"
# @aliasmethod "getErroneousPath"
# @aliasmethod "getSrcPath"
# @aliasmethod "getOutputPath"
# @aliasmethod "getInputPath"
#
# @title "Gets a subdirectory of the job batch"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{dir}{A name of a subdirectory.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character string.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
# 
# @keyword programming
#**/#######################################################################
setMethodS3("getDirectory", "JobBatch", function(this, dir=c("todo", "running", "finished", "failed", "interrupted", "erroneous", "src", "input", "output"), ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  dir <- match.arg(dir);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assure subdirectory exists
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Arguments$getReadablePath(dir, path=getRoot(this), mustExist=TRUE);
}, protected=TRUE)


setMethodS3("getTodoPath", "JobBatch", function(this, ...) {
  getDirectory(this, "todo");
})

setMethodS3("getRunningPath", "JobBatch", function(this, ...) {
  getDirectory(this, "running");
})

setMethodS3("getFinishedPath", "JobBatch", function(this, ...) {
  getDirectory(this, "finished");
})

setMethodS3("getFailedPath", "JobBatch", function(this, ...) {
  getDirectory(this, "failed");
})

setMethodS3("getErroneousPath", "JobBatch", function(this, ...) {
  getDirectory(this, "erroneous");
})

setMethodS3("getInterruptedPath", "JobBatch", function(this, ...) {
  getDirectory(this, "interrupted");
})

setMethodS3("getInputPath", "JobBatch", function(this, ...) {
  getDirectory(this, "input");
})

setMethodS3("getOutputPath", "JobBatch", function(this, ...) {
  getDirectory(this, "output");
})

setMethodS3("getSrcPath", "JobBatch", function(this, ...) {
  getDirectory(this, "src");
})




########################################################################/**
# @RdocMethod validate
#
# @title "Validates the job batch"
#
# \description{
#  @get "title" by asserting that all required subdirectories exist.
#  An exception is thrown if not valid.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns nothing.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
# 
# @keyword programming
#**/#######################################################################
setMethodS3("validate", "JobBatch", function(this, ...) {
  # Validate 'root' field. We cannot do it in the constructor because
  # it should always be possible to call JobBatch().
  if (is.null(this$.root))
    throw("Job root is NULL.");

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert that the job root exists.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  pathname <- getRoot(this);
  if (!isDirectory(pathname))
    throw("Job root does not exists or is not a directory: ", pathname);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert that all subdirectories exist
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  dirs <- c("todo", "running", "finished", "failed", "interrupted", 
                                    "erroneous", "src", "input", "output");
  for (dir in dirs)
    getDirectory(this, dir);

  invisible();
})




########################################################################/**
# @RdocMethod moveJobTo
#
# @title "Moves the job to another directory"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#  \item{dest}{A @character string specifying the destination directory
#    of the job.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns a @TRUE if successful, otherwise @FALSE.
# }
# 
# \details{
#  If job does not exist, is locked or destination does not exists, an
#  exception is thrown.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("moveJobTo", "JobBatch", function(this, job, dest=c("todo", "running", "finished", "failed", "interrupted", "erroneous"), ...) {
  log <- getLog(job);

  log && cat(log, "Trying to move job to '", dest, "'.");

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'job':
  if (!inherits(job, "Job"))
    throw("Argument 'job' is not a Job object: ", class(job)[1]);
  if (!isExisting(job))
    throw("Job does not exist: ", getPath(job));

  # Argument 'dest':
  dest <- match.arg(dest);
  destPath <- getDirectory(this, dest);

  # Check if source and destination is the same, then nothing to do.
  srcPath <- getPath(job);
  destPath <- filePath(destPath, basename(srcPath), expandLinks="any");
  if (srcPath == destPath) {
    log && cat(log, "Did not need to move because source and destination are the same: ", destPath);
    return(TRUE);
  }

  # Check if destination already exists.
  if (file.exists(destPath))
    throw("Cannot move job. Destination already exists: ", destPath);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert that it is ok to move job
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (isLocked(job))
    throw("Cannot move job. Job is locked.");

  res <- file.rename(srcPath, destPath);

  if (res) {
    # Update Job object 
    setPath(job, destPath);
    log && cat(log, "Job moved to '", dest, "': ", destPath);
  } else {
    log && cat(log, "Could not move job to '", dest, "': ", destPath);
  }
 
  res;
})



########################################################################/**
# @RdocMethod resetJobs
#
# @title "Resets Jobs in the job batch"
#
# \description{
#  @get "title" by unlocking each job and moving it back to the todo
#  directory.
# }
#
# @synopsis
#
# \arguments{
#  \item{jobs}{A @list of Jobs or a single Job to be reset.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns nothing.
# }
#
# \examples{\dontrun{
#   # Resets all jobs in job batch!
#   resetJobs(batch, findJobs(batch))
# }}
#
# \seealso{
#   @seeclass
# }
#
# @author
# 
# @keyword programming
#**/#######################################################################
setMethodS3("resetJobs", "JobBatch", function(this, jobs=NULL, ...) {
  if (is.null(jobs))
    return(invisible());

  if (!is.list(jobs))
    jobs <- list(jobs);

  for (job in jobs) {
    res <- moveJobTo(this, job, "todo");
    if (res) {
      resetToRun(job);
    } else {
      warning("Could not move job to 'todo/': ", getPath(job));
    }
  }
})


########################################################################/**
# @RdocMethod clean
#
# @title "Cleans up among jobs in this JobBatch"
#
# \description{
#   @get "title". 
#
#   Currently, cleaning up a job means that all of its output files
#   are moved to the directory of the job, i.e. \code{getPath(job)}.
# }
#
# \arguments{
#  \item{jobs}{A @list of Jobs or a single Job to be be clean up.
#      Default is to clean up all jobs in the JobBatch.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns nothing.
# }
#
# \seealso{
#   @seemethod "findJobs".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("clean", "JobBatch", function(this, jobs=findJobs(this), ...) {
  if (is.null(jobs))
    return(invisible());

  if (!is.list(jobs))
    jobs <- list(jobs);

  for (job in jobs) {
    moveOutputFilesTo(job, path=getPath(job));
  }
})



########################################################################/**
# @RdocMethod findJobs
#
# @title "Searches by name for Job:s in this JobBatch"
#
# \description{
#   @get "title". Jobs are searched for in the subfolders a valid Job,
#   that is, a job directory, can reside in.
#   The name of a Job is equal to the jobs directory name, e.g. 
#   \code{job01}.
# }
#
# \arguments{
#  \item{names}{A @vector of @character strings of job names to be found.} 
#  \item{regexpr}{If @TRUE, the \code{names} strings may be regular 
#    expression patterns, otherwise exact matching is required.}
#  \item{where}{A @vector of @character strings of directory names where to search for jobs.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a named list of Job:s with names equal to the \code{names} 
#   argument. If \code{regexpr} is @TRUE, several elements may have
#   the same name. Jobs not found are returned as @NULL.
# }
#
# \seealso{
#   @see "getName.Job".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("findJobs", "JobBatch", function(this, names="^job", regexpr=TRUE, where=c("todo", "running", "finished", "interrupted", "failed", "erroneous"), ...) {
  # Get summary for these directories
  summary <- getSummary(this)[where];

  # Extract 'jobs' fields
  dirs <- lapply(summary, FUN=function(dir) dir$jobs);
  dirs <- dirs[!unlist(lapply(dirs, FUN=is.null))];

  allJobs <- c();
  for (kk in seq(along=dirs)) {
    name <- names(dirs)[kk];
    tmp <- dirs[[kk]]; 
    names(tmp) <- rep(name, length(tmp));
    allJobs <- c(allJobs, tmp);
  }

  # Search for matching jobs...
  root <- getRoot(this);
  res <- list();
  for (kk in seq(along=names)) {
    name <- names[kk];
    if (regexpr) {
      pattern <- name;
    } else {
      pattern <- paste("^", name, "$", sep="");
    }

    jobs <- allJobs[regexpr(pattern, allJobs) != -1];
    nbrOfJobs <- length(jobs);
    if (nbrOfJobs > 0) {
      jobPaths <- names(jobs);
      for (kk in seq(along=jobs)) {
        jobPath <- filePath(root, jobPaths[kk], jobs[kk], expandLinks="any");
        job <- Job(jobPath=jobPath);
        tmp <- list(job);
        names(tmp) <- jobs[kk];
        res <- c(res, tmp);
      }
    } else {
      res <- c(res, list(NULL));
    }
  }

  res;
})



########################################################################/**
# @RdocMethod checkRequirements
#
# @title "Checks that requirements are fulfilled or not"
#
# \description{
#  @get "title".
#  
#  This method is called by @seemethod "getNextJob".
# }
#
# @synopsis
#
# \arguments{
#  \item{path}{A @character string of the path where to search for
#    requirement files. All subdirectories are searched too.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns NULL if all requirement files gives @TRUE, otherwise the
#  pathname (a @character string) of the first failed requirement file.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("checkRequirements", "JobBatch", function(static, path, ...) {
  srcPath <- filePath(path, expandLinks="any");
  reqFiles <- listDirectory(srcPath, pattern="^.Requirements.R$", 
                            allNames=TRUE, fullNames=TRUE, recursive=TRUE);
  if (length(reqFiles) == 0)
    return(NULL);

  for (reqFile in reqFiles) {
    res <- FALSE;
    tryCatch({
      pathname <- filePath(reqFile, expandLinks="any");
      res <- sourceTo(pathname)$value;
    }, error = function(ex) {
    })

    if (!identical(res, TRUE))
      return(reqFile);
  }

  NULL;
}, protected=TRUE, static=TRUE)


########################################################################/**
# @RdocMethod getNextJob
#
# @title "Gets next non-locked job"
#
# \description{
#  @get "title" and locks it.
#  
#  This method is called by @seemethod "getRunAndFinishJob".
# }
#
# @synopsis
#
# \arguments{
#  \item{which}{A @character string. The jobs are sorted in lexicographical
#    order by the directory names. 
#    If \code{"first"}, then the first non-locked job is retrieved.  
#    If \code{"last"}, then the last non-locked job is retrieved.  
#    If \code{"ranodm"}, a random non-locked job is retrieved.
#  }
#  \item{...}{Arguments, except \code{jobPath} and \code{verbose} passed
#    to the constructor of @see "Job".}
#  \item{verbose}{If @TRUE, extra information is displayed. The created
#    Job object gets this verbose level too.}
# }
#
# \value{
#  Returns a @see "Job" after first locking it. 
#  If no available job was found, @NULL is silently returned.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("getNextJob", "JobBatch", function(this, which=c("first", "last", "random"), ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert structure before starting
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  validate(this);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  which <- match.arg(which);

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Check for .Requirements.R files in src/. If any generates a non-TRUE
  # value, they will do this for all jobs. Hence, we can return from the
  # function already here.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  failedReq <- checkRequirements(this, getSrcPath(this));
  if (length(failedReq) > 0) {
    warning("Requirements not fulfilled. Nothing more to do: ", failedReq);
    return(NULL);
  }
  rm(failedReq);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Check for duplicated jobs. This may happen if the user copies files
  # back and forth. We do not want to start processing a duplicated job,
  # because the it will not be possible to move it to, say, 'finished'.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  dirs <- c("todo", "running", "finished", "failed", 
                                               "interrupted", "erroneous");
  paths <- unlist(lapply(dirs, FUN=function(p) getDirectory(this, p)));

  dups <- c();
  for (ii in 1:(length(paths)-1)) {
    jobsii <- list.files(path=paths[ii], pattern="^job");
    for (jj in (ii+1):length(paths)) {
      jobsjj <- list.files(path=paths[jj], pattern="^job");
      dups <- c(dups, intersect(jobsii, jobsjj));
    }
  }

  if (length(dups) > 0) {
    warning("Ignoring duplicated jobs: ", paste(sort(dups), collapse=", "));
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Get a (non-duplicated) job
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  path <- getDirectory(this, "todo");
  jobs <- list.files(path=path, pattern="^job", full.names=TRUE);
  verbose && cat(verbose, "Found ", length(jobs), " job(s) to be considered.\n");

  if (length(jobs) == 0) {
    warning("No jobs in todo directory: ", path);
    return(NULL);
  }

  nondups <- is.na(match(basename(jobs), dups));
  jobs <- jobs[nondups];

  if (length(jobs) == 0) {
    warning("Jobs found in todo directory, but they are all duplicated (remove these from 'finished', 'running' and so on): ", path);
    return(NULL);
  }

  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # While no job is found, try another one...
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  job <- NULL;
  while(is.null(job)) {
    njobs <- length(jobs);
    if (njobs == 0) {
      warning("Found no (unlocked) jobs: todo");
      return(NULL);
    }
  
    if(which == "first") {
      idx <- 1;
    } else if(which == "last") {
      idx <- njobs;
    } else if(which == "random") {
      idx <- sample(njobs, size=1);
    }
  
    verbose && enter(verbose, "Checking requirements");
    failedReq <- checkRequirements(this, jobs[idx]);
    if (length(failedReq) > 0) {
      warning("Requirements not fulfilled: ", failedReq);
      verbose && exit(verbose, suffix="...failed");
      jobs <- jobs[-idx];
      next;
    }
    rm(failedReq);
    verbose && exit(verbose);

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Create Job object
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # It might be that another process has stolen the job or it is locked...
    tryCatch({
      verbose && cat(verbose, "Trying to obtain job '", jobs[idx], "'...", newline=FALSE);
      job <- Job(jobPath=jobs[idx], verbose=verbose, ...)

      # A job was found, verify that we can run it.

      # 1. Are all dependencies finished and older than the job itself?
#      if (length(getDependencies(job)) > 0)
#        job <- NULL;

      if (!is.null(job)) {
        # 2. Try to move it to running, which will thrown an
        # exception if the job is locked.
        wasMoved <- moveJobTo(this, job, "running");
        if (wasMoved) {
          verbose && cat(verbose, "moved", newline=FALSE);
          if (lock(job)) {
            verbose && cat(verbose, " and locked. done."); 
          } else {
            verbose && cat(verbose, ", but failed to lock it. recovers.");
            # Moved the job, but failed to lock it. Move it back.
            moveJobTo(this, job, "todo");
          }
        } else {
          verbose && cat(verbose, "not moved. failed.");
          job <- NULL;
        }
      } # if (!is.null(job))
    }, error=function(error) {
      job <<- NULL;
      verbose && cat(verbose, " and failed.");
      verbose && cat(verbose, "Reason was:");
      verbose && print(verbose, error);
      warning(error);
    })

    if (is.null(job))
      jobs <- jobs[-idx];
  } # while (is.null(job))

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Return results
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  job;
}, protected=TRUE)




########################################################################/**
# @RdocMethod getRunAndFinishJob
#
# @title "Gets an non-locked job, process it and moves it to a final destination"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{reset}{If @TRUE, the job is reset before processed.}
#  \item{sink}{If @TRUE, all output is sinked to a file, otherwise not.}
#  \item{verbose}{If @TRUE, extra information is displayed. The created
#    Job object gets this verbose level too.}
#  \item{.cleanup}{A @function to be called after running job. For 
#    internal use only.}
#  \item{...}{Passed to @seemethod "getNextJob".}
# }
#
# \value{
#  Returns the @see "Job" object that was processed. If no unlocked job
#  was available, @NULL was returned.
# }
#
# \seealso{
#   @seemethod "run".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("getRunAndFinishJob", "JobBatch", function(this, sink=TRUE, reset=FALSE, clean=FALSE, verbose=FALSE, .cleanup=NULL, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'reset'
  reset <- Arguments$getLogical(reset);

  # Argument 'sink'
  sink <- Arguments$getLogical(sink);

  # Argument 'verbose'
  verbose <- Arguments$getVerbose(verbose);

  # Argument '.cleanup'
  if (!is.null(.cleanup) && !is.function(.cleanup))
    throw("Argument '.cleanup' is not a function: ", mode(.cleanup));

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert structure before starting
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  validate(this);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Retrieve a job
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  job <- getNextJob(this, verbose=verbose, ...);
  if (is.null(job))
    return(NULL);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Tries to run it
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  resetToRun(job);
  wasSuccessful <- run(job, reset=reset, sink=sink);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Call cleanup function, if given.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (!is.null(.cleanup))
    .cleanup();

  # ...then unlock it.
  unlock(job);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Move to final path
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (wasSuccessful) {
    finalDest <- "finished";
  } else {
    if (wasInterrupted(job)) {
      finalDest <- "interrupted";
    } else if (hasFailed(job)) {
      finalDest <- "failed";
    } else if (isErroneous(job)) {
      finalDest <- "erroneous";
    } else {
      throw("INTERNAL ERROR: Job was not successful, but neither it was interrupted nor did it fail. What happend? Job: ", as.character(job));
    }
  }

  # Tries to move the job to the final directory. Since the job
  # directory might be open once in a while by the user, and therefore
  # non-moveable, we should try several times before giving up.
  maxCount <- 10;
  count <- 0;
  while (TRUE) {
    gc();

    lastError <- NULL;
    tryCatch({
      if (moveJobTo(this, job, finalDest)) {
        verbose && cat(verbose,  "Moved job: ", finalDest);
        break;
      } else {
        unlock(job);
      }
    }, condition = function(ex) {
      lastError <<- ex;
      verbose && print(verbose, ex);
      unlock(job);
    })

    count <- count + 1;
    if (count == maxCount-1) {
      # As a last resort before giving up, close all open graphical
      # devices, because they have open files locking the job directory.
      # This may for instance happen when running non-interactively().
      # This should typically be done in onFinally().
      graphics.off();
    } else if (count > maxCount) {
      throw("Failed to move job to '", finalDest, "': ", as.character(job), ". Reason is: ", as.character(lastError));
    }

    verbose && cat(verbose, "Could not moved job to '", finalDest, "', but will try again soon.\nReason is: ", as.character(lastError));

    # Sleep for a while until trying again.
    Sys.sleep(10);
  } # while(TRUE)

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Return the job
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  gc();

  job;
})


########################################################################/**
# @RdocMethod run
#
# @title "Process some or all jobs available"
#
# \description{
#  @get "title". 
#
#  Note that if a job is \emph{interrupted} this method will detect that
#  and return quietly.  The rational for this is that it should always be
#  possible to interrupt the job batch too, without having to kill the
#  process.
# }
#
# @synopsis
#
# \arguments{
#  \item{maxJobs}{The maximum number of jobs to be processed.
#    If \code{-1}, all jobs available will be processed.
#    If @Inf, it will run forever until a job is interrupted.  When
#    no more jobs are detected, it will recheck for new jobs every
#    \code{sleepTime} seconds.
#  }
#  \item{sleepTime}{A @double specifying the number of seconds for \R
#    to sleep before checking for new jobs.}
#  \item{...}{Arguments passed to @seemethod "getRunAndFinishJob".}
# }
#
# \value{
#  Returns (invisibly) a @list of descriptions of jobs processed.
# }
#
# \seealso{
#   @seemethod "getRunAndFinishJob".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("run", "JobBatch", function(this, maxJobs=-1, sleepTime=15, clean=FALSE, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'maxJobs':
  maxJobs <- Arguments$getDouble(maxJobs, range=c(-1, Inf));

  # Argument 'sleepTime':
  sleepTime <- Arguments$getDouble(sleepTime, range=c(0, 24*60*60));

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);

  # Argument 'clean':
  clean <- Arguments$getLogical(clean);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Search for jobs
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  runForever <- is.infinite(maxJobs);

  res <- list();
  attr(res, "runForever") <- runForever;

  count <- 0;
  while (TRUE) {
    gc();

    job <- getRunAndFinishJob(this, ..., verbose=verbose);

    if (!is.null(job)) {
      if (clean)
        moveOutputFilesTo(job, path=getPath(job));
    } else {
      verbose && cat(verbose, "No more jobs available.");
      if (runForever) {
        verbose && cat(verbose, "Waiting ", sleepTime, " seconds before checking again.");
        Sys.sleep(sleepTime);
        next;
      } else {
        break;
      }
    }

    count <- count + 1;

    verbose && cat(verbose, "Completed job #", count, ":");
    verbose && print(verbose, job);

    if (!runForever)
      res[[count]] <- as.character(job);

    if (wasInterrupted(job)) {
      attr(res, "last") <- "interrupted";
      verbose && cat(verbose, "Job and job batch interrupted.");
      break;
    }

    if (runForever) {
      verbose && cat(verbose, "Running jobs until being interrupted.");
    } else {
      if (maxJobs > 0) {
        maxJobs <- maxJobs - 1;
        if (maxJobs <= 0) {
          verbose && cat(verbose, "Processed all jobs.");
          break;
        }
        verbose && cat(verbose, maxJobs, " jobs to go.");
      }
    }
  } # while(TRUE)

  invisible(res);
})




########################################################################/**
# @RdocMethod createStub
#
# @title "Creates a jobs directory structure stub"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character string.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("createStub", "JobBatch", function(this, ...) {
  stub <- JobBatch(system.file("jobs-stub", package="R.batch"))
  copyFrom(this, stub, ...);
})





########################################################################/**
# @RdocMethod copyFrom
#
# @title "Copies a job batch directory into this one"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#  \item{batch}{Another @see "JobBatch" object.}
#  \item{conflicts}{A @character string specifying how to handle already
#    existing files. If \code{"error"}, an error is thrown. 
#    If \code{"skip"}, the file is quietly skipped.
#    If \code{"overwrite"}, the file is quietly overwritten.}
#  \item{...}{Arguments passed to @seemethod "copyFrom".}
# }
#
# \value{
#  Returns a @character string.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("copyFrom", "JobBatch", function(this, batch, conflicts=c("skip", "overwrite", "error"), ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'batch'
  if (!inherits(batch, "JobBatch"))
    throw("Argument 'batch' must be a JobBatch object.");

  # Argument 'conflicts'
  conflicts <- match.arg(conflicts);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Get all files and subdirectories of the source batch
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  srcRoot <- getRoot(batch);
  srcfiles <- listDirectory(srcRoot, recursive=TRUE, allNames=TRUE);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Copy the to this batch
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  destRoot <- getRoot(this);
  for (file in srcfiles) {
    from <- filePath(srcRoot, file, expandLinks="any");
    to <- filePath(destRoot, file, expandLinks="any");

    if (isDirectory(from)) {
      # If 'from' is a directory, create the same at destination...
      mkdirs(to);
    } else {
      # ...else, copy the file and make sure all parent directories are
      # created first.
      parent <- getParent(to);
      if (!isDirectory(parent)) {
        if (!mkdirs(parent))
          throw("Failed to create directory: ", as.character(parent));
      }
  
      # Copy file
      if (file.exists(to)) {
        if (conflicts == "overwrite") {
          file.copy(from=from, to=to, overwrite=TRUE);
        } else if (conflicts == "error") {
          throw("File already exists: ", to);
        }
      } else {
        file.copy(from=from, to=to);
      }
    } # if (isDirectory(from))
  } # for (file...)
})



########################################################################/**
# @RdocMethod getSummary
#
# @title "Gets a summary of the jobs directory"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns a @list.
# }
#
# \seealso{
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("getSummary", "JobBatch", function(this, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert job root
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  validate(this);

  root <- getRoot(this);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Check for duplicated jobs. This may happen if the user copies files
  # back and forth. We do not want to start processing a duplicated job,
  # because the it will not be possible to move it to, say, 'finished'.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  dirs <- c("todo", "running", "finished", "failed", "interrupted", 
                                                    "erroneous", "output");
  paths <- unlist(lapply(dirs, FUN=function(p) getDirectory(this, p)));

  dups <- c();
  for (ii in 1:(length(paths)-1)) {
    jobsii <- list.files(path=paths[ii], pattern="^job");
    for (jj in (ii+1):length(paths)) {
      jobsjj <- list.files(path=paths[jj], pattern="^job");
      dups <- c(dups, intersect(jobsii, jobsjj));
    }
  }

  res <- list(root=as.character(root), duplicatedJobs=dups);
  for (kk in seq(paths)) {
    # Count the number of directories and the number of files
    files <- listDirectory(filePath(paths[kk], expandLinks="any"), fullNames=TRUE);
    if (is.null(files))
      next;

    isDir <- unlist(lapply(files, FUN=isDirectory));
    names <- unlist(lapply(files, FUN=basename));
    isJob <- isDir & (regexpr("^job", names) != -1);
    rm(files);

    jobs <- sort(names[isJob]);
    isDuplicated <- is.finite(match(jobs, dups));
    jobsStr <- jobs;
    jobsStr[isDuplicated] <- paste(jobsStr[isDuplicated], "*", sep="");

    key <- dirs[kk];
    res[[key]] <- list(
      nbrOfDirs = sum(isDir),
      dirs = sort(names[isDir]),
      nbrOfFiles = sum(!isDir),
      files = sort(names[!isDir]),
      nbrOfJobs = sum(isJob),
      jobs = jobs,
      isDuplicated = is.finite(match(jobs, dups)),
      jobsStr = jobsStr
    )
  }

  res;
}, protected=TRUE);



########################################################################/**
# @RdocMethod main
#
# @title "Static method to be called from the command line"
#
# \description{
#  @get "title". 
#  When called, this method will process one available job and return.\cr
#
#  The following \R command line options are recognized:
#  \itemize{
#   \item \code{--root=<path>} or \code{--root <path>} specifies the 
#         root path of the batch directory.
#   \item \code{--reset} specifies if each job should be reset before 
#         it is started. 
#   \item \code{--sink} specifies if each job output should be sinked 
#         to file. 
#   \item \code{--details} specifies if extra information should be
#         printed to the standard output. 
#   \item \code{--maxJobs} specifies the \emph{maximum} number of jobs 
#         this batch dispatcher should process before quiting. 
#         If \code{-1}, it runs until no more jobs are found.
#         If \code{Inf}, it runs forever until being interrupted.
#   \item \code{--sleepTime} specifies the number of seconds for \R
#         to sleep before checking for new jobs, when no more jobs are
#         available. Only effective if \code{--maxJobs=Inf}.
#   \item \code{--clean} specifies if all job specific created in the 
#         output path should be moved to the job path when job is done.
#  }
#
#  To avoid warning about unknown options when \R, add these options
#  at end after \code{--args}. See example below.\cr
#
#  To run this from the command line, see @see "1. Useful scripts".
# }
#
# @synopsis
#
# \arguments{
#  \item{root}{A @character string specifying the default value for the
#    command line option \code{--root}.}
#  \item{reset}{A @logical value specifying the default value for the
#    command line option \code{--reset}.}
#  \item{sink}{A @logical value specifying the default value for the
#    command line option \code{--sink}.}
#  \item{details}{A @logical value specifying the default value for the
#    command line option \code{--details}.}
#  \item{maxJobs}{A @integer (or the double @Inf) value specifying the
#    default value for the command line option \code{--maxjobs}.}
#  \item{sleepTime}{A @double value specifying the
#    default value for the command line option \code{--sleepTime}.}
#  \item{clean}{If @TRUE, job specific files in the output path are
#    moved to the job path after job is finished.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns (invisibly) the @see "Job" object processed, otherwise @NULL.
# }
#
# \examples{\dontrun{
#   RJobBatch --details --root=jobs-ex 
# }}
#
# \seealso{
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("main", "JobBatch", function(static, root="jobs", reset=FALSE, sink=TRUE, details=FALSE, maxJobs=1, sleepTime=15, clean=FALSE, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Clean up afterwards
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  on.exit({
    # Just in case it is locked, unlock job.
    if (exists("job")) {
      if (!is.null(job))
        unlock(job);
      job <- NULL; 
    }
    # Force all allocated jobs to finalize. 
    gc();
  }, add=TRUE);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Import arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Parse command line arguments (use commandArgs() in R.utils).
  args <- commandArgs(asValues=TRUE);

  # Override method arguments with command-line arguments, if specified.
  if (!is.null(args$root)) 
    root <- args$root;
  if (!is.null(args$reset))
    reset <- args$reset;
  if (!is.null(args$sink))
    sink <- args$sink;
  if (!is.null(args$details)) 
    details <- args$details;
  if (!is.null(args$maxJobs)) 
    maxJobs <- args$maxJobs;


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'root':
  root <- Arguments$getReadablePath(root, mustExist=TRUE);

  # Argument 'reset':
  reset <- Arguments$getLogical(reset);

  # Argument 'sink':
  sink <- Arguments$getLogical(sink);

  # Argument 'details':
  details <- as.character(details);
  if (toupper(details) %in% c("TRUE", "FALSE")) {
    details <- as.logical(details);
  } else {
    details <- as.integer(details);
  }
  verbose <- Arguments$getVerbose(details);

  # Argument 'maxJobs':
  maxJobs <- Arguments$getDouble(maxJobs, range=c(-1, Inf));

  # Argument 'sleepTime':
  sleepTime <- Arguments$getDouble(sleepTime, range=c(0, 24*60*60));

  # Argument 'clean':
  clean <- Arguments$getLogical(clean);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Creating job batch and starting it
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  batch <- JobBatch(root);

  verbose && print(verbose, batch);

  # Cleanup code to be called if called from command line.
  cleanup <- NULL;
  if (!interactive()) {
    cleanup <- function() graphics.off();
  }

  res <- run(batch, reset=reset, sink=sink, maxJobs=maxJobs, 
      sleepTime=sleepTime, .cleanup=cleanup, clean=clean, verbose=verbose);

  invisible(res);
}, static=TRUE) # main()



###########################################################################
# HISTORY: 
# 2006-09-19
# o BUG FIX: Argument 'details' must be converted explicitly to a character
#   before calling toupper().  This must be due to a change in R.
# 2006-01-31
# o Rdoc bug fix: 'example' instead of 'examples' tag.
# 2005-12-05
# o Replaced all writeToLog() with log Verbose object calls.
# 2005-12-02
# o Remove all *use* of code for dependencies and requirements.
# o Making use of the Verbose class.
# 2005-11-28
# o Making use of Arguments class a bit more.
# o Added arguments 'maxJobs' to main() allow more than one job to be 
#   processed.  Now main() is calling run().
# o Added argument 'verbose' to run(), which can be of class Verbose too.
#   All methods of this package should be rewritten to use the Verbose
#   class, but that is for the future.
# o BUG FIX: run() of class JobBatch used internal reference 'batch' 
#   instead of 'this'.
# 2005-05-23
# o Added static checkRequirements(), which source():s all .Requirements.R
#   files.
# 2005-05-21
# o Renamed resetJob() to resetJobs(). Now also a list of Job objects can
#   be reset.
# o Now resetToRun(job) is called before run(job) in getRunAndFinishJob().
#   This is to reset persistent fields.
# o Added findJobs().
# 2005-03-13
# o BUG FIX: Still used 'verbose' in run() although it has been removed
#   from the argument list.
# 2005-03-09
# o BUG FIX: getSummary() gave an error for empty directories.
# o BUG FIX: copyFrom() would not create empty directories.
# 2005-03-02
# o Now copyFrom() takes the argument 'conlflicts', which is more general
#   than former 'overwrite'.
# o Added argument 'sink' to getRunAndFinishJob() to sink output from
#   Job. Also added command line option --sink, which defaults to TRUE.
# 2005-03-01
# o main() now close all open devices when job is done, if called 
#   non-interactively, i.e. by R batch.
# o Added argument 'cleanup' to getRunAndFinishJob(). This will be used
#   by main().
# o If job can not be moved to its final destination, graphics.off() will
#   be called one iteration before giving up.
# o Renamed getSrc() to getSrcPath() and similar for the others. Before
#   getOutput() in JobBatch corresponded to getOutputPath() in Job.
# 2005-02-23
# o Added static main() methods.
# o main() method now making use of updated commandArgs() in R.basic.
# 2005-02-20
# o Added subdirectory "erroneous".
# 2005-02-19
# o Create the JobBatch() class.
# o Now also "private" directories and files are sourced, e.g. '.dir'.
# o Now summariesJobs() warns and highlight duplicated jobs.
# o Duplicated jobs are ignore with a warning.
# o Added static getSummary() and summariesJobs().
# o Phasing out the settings features. It is much easier to just do it
#   with plain R code.
# o Rename all .onNNN() functions to onNNN().
# o Added "job" functions getInputPath() and getOutputPath().
# 2005-02-18
# o Removed several 'verbose' arguments by making it a field of Job.
# o Added getDirectory() and assertSubdirectories().
# o Now the settings file should be names SETTINGS (captial letters), 
#   although on some file systems such as Windows, this is not required.
# o Added createJobsDirectoryStub() for conveniency.
# o BUG FIX: setup() only reads settings file, if it exists. Before, it
#   would generate an error.
# o Updated with Rdoc comments for all methods.
# o Added '...' to all methods to please R CMD check.
# 2004-08-13
# o Added the src/global directory which can contain source code that
#   should be source()'d into the global environment instead of the
#   local provided by the job. Note that this directory may also be a
#   Windows Shortcut link to an actual directory.
# o Now also Windows Shortcut files are sourced making it extremely 
#   convinient to reuse source code.
# 2004-08-12
# o Added require( R.lang ) to constructor.
# 2004-07-26
# o Moved getUsername() and getHostname() to R.lang::System.
# o Now run() checks for errors in setup() too, for instance if a parse
#   errors in source occurs, and sets the correct status such that, say,
#   getRunAndFinishJob() can recover correctly, i.e. move the job to
#   the "failed" directory and so on.
# o BUG FIX: Added a sep="" to stop().
# 2004-07-22
# o Added "is" to "...and is [not] locked" in as.character().
# 2004-07-21
# o Now getRunAndFinishJob() verifies the existance of all directories 
#   before starting.
# o Now getRunAndFinishJob() will also try to unlock the job if it can not
#   be moved to its final destination.
# o Added Rdoc comments for static getRunAndFinishJob().
# o BUG FIX: Used old 'job' instead of 'this' is getAsVector() etc.
# 2004-07-14
# o Added utility methods getAsIs(), getAsVector(), and getAsScalar().
# o Now detailed tracking information is written to the lock file.
# o Added wasSuccessful(), hasFailed(), wasInterrupted().
# 2004-07-13
# o Added static getRunAndFinishJob(), which does what it says.
# 2004-07-12
# o run() now returns result invisible().
# o Renamed getName() to getLabel(). If label is not set, that is, is NULL,
#   getLabel() returns "<username>@<host>_<object address>" in order to
#   be able to trace where the process is running if this is reported to
#   file.
# o Added verbose to static getJob().
# 2004-07-10
# o Added getHostname() and getUsername().
# o Added Rdoc comments for most methods.
# 2004-07-07
# o Added static getJob().
# 2004-06-29
# o Modified already existing readSettings() method. 
# o Re-created.
# 2004-06-09
# o First version of job function was created.
###########################################################################
