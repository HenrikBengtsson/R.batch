###########################################################################/**
# @RdocClass Job
#
# @title "Class representing a batch job"
#
# \description{
#  @classhierarchy
#
#  @get "title".
#
#  A \code{Job} belongs to a @see "JobBatch".
# }
#
# @synopsis
#
# \arguments{
#   \item{jobPath}{The job path.}
#   \item{label}{An optional @character string specifying the label (name) 
#     of the job.}
#   \item{verbose}{If @TRUE, detailed information will be given while 
#     working on the job.}
# }
#
# \section{Fields and Methods}{
#  @allmethods  
# }
#
# @author
#
# @keyword programming
#*/###########################################################################
setConstructorS3("Job", function(jobPath=NULL, label=NULL, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'jobPath':
  if (!is.null(jobPath)) {
    jobPath <- Arguments$getReadablePath(jobPath, mustExist=TRUE);
  }

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Create object
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  this <- extend(Object(), "Job",
    # Paths
    .jobPath = jobPath,

    # Verbose
    .verbose = verbose,

    # Status
    .status = "is not started",
  
    # Label
    .jobLabel = label,

    # Time stamps
    .jobStartTime = NA,
    .jobRunTime = NA,
    .jobInterruptTime = NA,
    .jobErrorTime = NA,
    .jobFinallyTime = NA,
    .jobStopTime = NA,

    # Log object
    log = NULL,

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Default on*() functions
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    onStart = function(job) {
      log <- getLog(job);
      log && cat(log, "In default onStart().");
    },

    onRun = function(job) {
      log <- getLog(job);
      log && cat(log, "onRun() not defined.");
      throw("onRun() not defined. Define onRun <- function(job) { ... } ",
          "in, say, onRun.R, in the src/ directory: ", job$.jobPath);
    },

    onInterrupt = function(job, interrupt) {
      log <- getLog(job);
      log && cat(log, "In default onInterrupt().");
      log && cat(log, "Interrupt message: ", as.character(interrupt));
      log && enter(log, "Saving Job object.");
      saveImage(job, label="interrupted");
      log && exit(log);
      print(interrupt);
    },

    onError = function(job, error) {
      log <- getLog(job);
      log && cat(log, "In default onError().");
      log && enter(log, "Saving Job object");
      saveImage(job, label="error");
      log && exit(log);
      log && cat(log, "Error message: ", as.character(error));
      print(error);
      traceback();
    },

    onFinally = function(job) {
      log <- getLog(job);
      log && cat(log, "In default onFinally().");
      graphics.off();
      log && cat(log, "Closed all open devices.");
    },

    onReset = function(job) {
      log <- getLog(job);
      log && cat(log, "In default onReset().");
      log && cat(log, "Resetting current log file without backup.");
      resetLog(job, backup=FALSE);

      removeStoredImage(job);
    },

    onRestart = function(job, fields=NULL) {
      log <- getLog(job);
      log && cat(log, "In default onRestart().");
    }
  )

  # Define persistent fields
  if (isExisting(this)) {
    addPersistentField(this, c(".status", ".jobLabel", ".jobStartTime", ".jobRunTime", ".jobInterruptTime", ".jobErrorTime", ".jobFinallyTime", ".jobStopTime"));
  }

  this;
}) # Job()


#############################################################################
# Methods to support persistent fields
#############################################################################
setMethodS3("addPersistentField", "Job", function(this, names, update=TRUE, ...) {
  names <- as.character(names);

  # 1. Add field names to list of persistent field names
  if (is.null(this$.persistentFields)) {
    this$.persistentFields <- names;
  } else {
    this$.persistentFields <- unique(c(this$.persistentFields, names));
  }

  # Update all persistent fields both to memory and on persistant storage.
  if (update) {
    path <- getPersistentPath(this);
    for (name in names) {
      pathname <- filePath(path, name, expandLinks="any");
      if (file.exists(pathname)) {
        value <- NULL; rm(value); # To please R CMD check
        load(pathname);  # Loads object named 'value'
        this[[name]] <- value;
      } else {
        value <- this[[name]];
        save(value, file=pathname);
      }
    }
  }
}, protected=TRUE)



setMethodS3("moveOutputFilesTo", "Job", function(this, names, path=getPath(this), ...) {
  fromPath <- getOutputPath(this);
  pattern <- paste("^", getLabel(this), sep="");
  files <- list.files(pattern=pattern, path=fromPath);
  for (file in files) {
    from <- file.path(fromPath, file);
    dest <- file.path(path, file);
    file.rename(from, dest);
  }
})


setMethodS3("isPersistentField", "Job", function(this, names, ...) {
  if (is.null(getPath(this)))
    return(FALSE);

  names <- as.character(names);
  (names %in% this$.persistentFields);
}, protected=TRUE)



setMethodS3("getPersistentPath", "Job", function(this, ...) {
  path <- filePath(getPath(this), ".persistent", expandLinks="any");
  mkdirs(path);
  path;
}, protected=TRUE)



setMethodS3("setField", "Job", function(this, name, value, ...) {
  this[[name]] <- value;
  if (isPersistentField(this, name)) {
    pathname <- file.path(getPersistentPath(this), name);
    save(value, file=pathname);
  }
}, protected=TRUE)



setMethodS3("getField", "Job", function(this, name, ...) {
  if (isPersistentField(this, name)) {
    pathname <- file.path(getPersistentPath(this), name);
    if (file.exists(pathname)) {
      value <- NULL; rm(value); # To please R CMD check
      load(pathname);  # Loads object named 'value'
      this[[name]] <- value;
      value;
    } else {
      this[[name]];
    }
  } else {
    this[[name]];
  }
}, protected=TRUE)



setMethodS3("touch", "Job", function(this, ...) {
  file <- file.path(getPersistentPath(this), ".lastModified");
  cat(as.character(Sys.time()), file=file);
})



setMethodS3("untouch", "Job", function(this, ...) {
  file <- file.path(getPersistentPath(this), ".lastModified");
  if (file.exists(file))
    file.remove(file);
})



setMethodS3("lastModified", "Job", function(this, ...) {
  file <- file.path(getPersistentPath(this), ".lastModified");
  if (!file.exists(file))
    return(NA);
  lastModified(file);
})


setMethodS3("isNewer", "Job", function(this, other, ...) {
  if (!is.na(other) && !inherits(other, "Job"))
    throw("Argument 'other' is not a Job object: ", class(other));

  # A Job cannot be newer than itself.
  if (equals(this, other))
    return(FALSE);

  t1 <- lastModified(this);
  t2 <- lastModified(other);

  # If either Job has not been process, return NA.
  if (is.na(t1) || is.na(t2))
    return(NA);

  (t1 > t2);   
})


setMethodS3("getDependenciesRaw", "Job", function(this, ...) {
  pathname <- file.path(getPath(this), ".Dependencies");
  if (!file.exists(pathname))
    return(NULL);

  lines <- readLines(pathname);

  # Remove comments
  lines <- gsub("#.*$", "", lines);

  # Trim lines
  lines <- gsub("^ ", "", lines);
  lines <- gsub(" $", "", lines);

  # Remove empty lines
  lines <- lines[nchar(lines) > 0];

  # Split in to two columns; batch name and job name.
  res <- NULL;
  for (kk in seq(along=lines)) {
    line <- lines[kk];
    pos <- regexpr("/[^/]*$", line);
    if (pos == -1) {
      batch <- NA;
      job <- line;
    } else {
      batch <- substring(line, first=1, last=pos-1);
      job <- substring(line, first=pos+1);
    }
    row <- list(batch=batch, job=job);
    res <- rbind(res, row);
  }
  rownames(res) <- seq(length=nrow(res));

  res;
}, protected=TRUE)


setMethodS3("getDependencies", "Job", function(this, criteria=function(job) { !isFinished(job) || is.na(lastModified(job)) || (!is.na(lastModified(this)) && isNewer(job, this)) }, ...) {
  # Argument 'criteria':
  if (is.function(criteria)) {
  } else if (!is.null(criteria)) {
    throw("Argument 'criteria' must be a function or NULL: ", mode(criteria));
  }

  dep <- getDependenciesRaw(this);
  if (is.null(dep))
    return(list());

  thisBatchPath <- getRoot(this);

  jobs <- list();
  for (kk in seq(length=nrow(dep))) {
    batchPath <- unlist(dep[kk, "batch"]);
    if (is.na(batchPath))
      batchPath <- NULL;

    jobName <- unlist(dep[kk, "job"]);

    # FIX: Absolute paths
    batchPath <- filePath(thisBatchPath, batchPath, expandLinks="any");

    job <- list(NULL);
    if (file.exists(batchPath)) {
      batch <- JobBatch(batchPath);
      tryCatch({
        job <- findJobs(batch, jobName, regexpr=FALSE)[[1]];
      }, error=function(ex) {
      })
    }

    if (equals(this, job))
      throw("A job can not dependend on itself: ", as.character(job));

    if (is.function(criteria)) {
      if (!criteria(job))
        job <- NULL;
    }

    if (!is.null(job))
      jobs <- c(jobs, list(job));
  }

  jobs;
})


#########################################################################/**
# @RdocMethod equals
#
# @title "Checks if this job equals another"
# 
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{other}{Another Job.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns @TRUE if the jobs are equal, otherwise @FALSE.
# }
#
# \details{
#  If the other object is not a Job object, this method returns @FALSE.
#  Otherwise, @seemethod "getPath" is called on both objects and these are
#  compared with @see "base::identical".
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/#########################################################################  
setMethodS3("equals", "Job", function(this, other, ...) {
  if (!inherits(other, "Job"))
    return(FALSE);
  if (identical(getPath(this), getPath(other)))
    return(TRUE);
  FALSE;
})



#########################################################################/**
# @RdocMethod showWarnings
#
# @title "Displays warning generated while running this job"
# 
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{reset}{If @TRUE, warnings are removed after being viewed, 
#    otherwise not.}
#  \item{show}{If @TRUE, warnings are displayed, otherwise not.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns nothing.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/#########################################################################  
setMethodS3("showWarnings", "Job", function(this, reset=TRUE, show=TRUE, ...) {
  if (!exists("last.warning", envir=.GlobalEnv))
    return(invisible());

  last.warning <- get("last.warning", envir=.GlobalEnv);
  nbrOfWarnings <- length(last.warning);

  # Print and reset warnings 
  if (nbrOfWarnings > 0) {
    log <- getLog(this);
    log && cat(log, "Warnings detected: ", nbrOfWarnings);

    if (show)
      print(warnings());

    if (reset)
      rm("last.warning", envir=.GlobalEnv);
  }

  invisible();
}, private=TRUE)


########################################################################/**
# @RdocMethod saveImage
#
# @title "Save an image of the job"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{label}{A @character string to be used in the filename.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns nothing.
# }
#
# \seealso{
#   @seemethod "hasStoredImage".
#   @seemethod "loadStoredImage".
#   @seemethod "removeStoredImage".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("saveImage", "Job", function(this, label=getLabel(this), ...) {
  label <- as.character(label);
  save(this, file=paste(label, ".Job", sep=""));
})


########################################################################/**
# @RdocMethod hasStoredImage
#
# @title "Checks if a stored job image exists"
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
#  Returns @TRUE if a stored image exists, otherwise @FALSE.
# }
#
# \seealso{
#   @seemethod "saveImage".
#   @seemethod "loadStoredImage".
#   @seemethod "removeStoredImage".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("hasStoredImage", "Job", function(this, ...) {
  file.exists("interrupted.Job");
})


########################################################################/**
# @RdocMethod removeStoredImage
#
# @title "Removes stored job image"
#
# \description{
#  @get "title", if exists.
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
#   @seemethod "saveImage".
#   @seemethod "hasStoredImage".
#   @seemethod "loadStoredImage".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("removeStoredImage", "Job", function(this, ...) {
  if (hasStoredImage(this)) {
    log <- getLog(this);
    file.remove("interrupted.Job");
    log && cat(log, "Removed stored Job image file.");
  }
})



########################################################################/**
# @RdocMethod loadStoredImage
#
# @title "Reload a stored job image"
#
# \description{
#  @get "title", if exists.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character @vector with the names of the reloaded fields.
# }
#
# \seealso{
#   @seemethod "hasStoredImage".
#   @seemethod "removeStoredImage".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("loadStoredImage", "Job", function(this, cleanup=TRUE, ...) {
  if (!hasStoredImage(this))
    return(c());

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Setting up logging
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  log <- getLog(this);
  log && pushState(log);
  on.exit(popState(log), add=TRUE);

  log && cat(log, "Loading Job object from image file.");
  oldJob <- Object$load(file="interrupted.Job");
  log && cat(log, "Job object image file successfully read.");

  log && cat(log, "Restoring fields from old Job object.");

  # Fields in stored Job object
  fields <- as.character(ll(oldJob, private=FALSE)$member);

  # Do not overwrite current fields
  excl <- as.character(ll(this, private=TRUE)$member);
  fields <- setdiff(fields, excl);
  log && cat(log, "Restoring ", length(fields), " fields: ", 
                                       paste(fields, collapse=", "));
  # Copy field by field
  for (field in fields) {
    this[[field]] <- oldJob[[field]];
    oldJob[[field]] <- NULL;       # Save memory as soon as possible.
  }

  rm(oldJob);
  gc();

  if (cleanup) {
    file.remove("interrupted.Job");
    log && cat(log, "Removed old Job image file.");
  }

  log && cat(log, "Previous Job object restored.");

  fields;
})


########################################################################/**
# @RdocMethod finalize
#
# @title "Finalizes job"
#
# \description{
#  @get "title" by unlocking it, if locked.
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
setMethodS3("finalize", "Job", function(this, ...) {
  log <- getLog(this);

  if (isSinked(this))
    unsink(this);

  # In case the file connection for the stderr sink is still open...
  file <- this$.sink[["file"]];
  if (inherits(file, "connection") && isOpen(file))
    close(file);
  this$.sink[["file"]] <- NULL;

#  log && cat(log, "Finalizing Job object.");
  if (!is.null(getPath(this))) {
    if (isLocked(this))
      unlock(this);
#    log && cat(log, "Job object finalized. Bye bye ;)");
  }
})



########################################################################/**
# @RdocMethod as.character
#
# @title "Gets a character string representation of the job"
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
setMethodS3("as.character", "Job", function(x, ...) {
  # To please R CMD check
  this <- x;

  s <- paste(class(this)[1], ": ", getLabel(this), ":", sep="");
  s <- paste(s, " Job ", getStatus(this), " and is", sep="");
  if (!isLocked(this))
    s <- paste(s, " not", sep="");
  s <- paste(s, " locked.", sep="");
  if (isDone(this)) {
    startTime <- getField(this, ".jobStartTime");
    stopTime <- getField(this, ".jobStopTime");
    s <- paste(s, " It was started on ", as.character(startTime),
               " and stopped on ", as.character(stopTime),
               " (in total ", as.character(stopTime - startTime),
               " seconds).", sep="");
  }
  s <- paste(s, " Current job path is '", getPath(this), "'.", sep="");
  if (!isExisting(this)) {
    s <- paste(s, " The job does not exist.", sep="");
  }
  s;
})




########################################################################/**
# @RdocMethod getPath
# @aliasmethod setPath
#
# @title "Gets the path to the job"
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
setMethodS3("getPath", "Job", function(this, ...) {
  path <- this$.jobPath;
  if (is.null(path))
    return(NULL);
  getAbsolutePath(path);
})


setMethodS3("setPath", "Job", function(this, path, ...) {
  path <- Arguments$getReadablePath(path, mustExist=TRUE);
  path <- getAbsolutePath(path);
  this$.jobPath <- path;
}, private=TRUE)


setMethodS3("getRoot", "Job", function(this, ...) {
  filePath(getPath(this), "..", "..", expandLinks="any");
})



########################################################################/**
# @RdocMethod getOutputPath
#
# @title "Gets the output path of the job"
#
# \description{
#  @get "title".
#  It is recommended to write logs, store results etc. to this directory.
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
setMethodS3("getOutputPath", "Job", function(this, ...) {
  path <- this$outputPath;
  if (is.null(path))
    path <- filePath(getRoot(this), "output", expandLinks="any");
  getAbsolutePath(path);
})


setMethodS3("setOutputPath", "Job", function(this, path=NULL, ...) {
  oldPath <- this$outputPath;
  this$outputPath <- path;
  invisible(oldPath);
})

setMethodS3("setFigurePath", "Job", function(this, path=NULL, ...) {
  oldPath <- this$figurePath;
  this$figurePath <- path;
  invisible(oldPath);
})


setMethodS3("getFigurePath", "Job", function(this, ...) {
  path <- this$figurePath;
  if (is.null(path))
    path <- getOutputPath(this);
  if (!isAbsolutePath(path))
    path <- filePath(getPath(this), path, expandLinks="any");
  path <- filePath(path, expandLinks="any");
  getAbsolutePath(path);
})


setMethodS3("getResultPath", "Job", function(this, ...) {
  path <- this$resultPath;
  if (is.null(path))
    path <- getOutputPath(this);
  if (!isAbsolutePath(path))
    path <- filePath(getPath(this), path, expandLinks="any");
  path <- filePath(path, expandLinks="any");
  getAbsolutePath(path);
})


setMethodS3("getLogPath", "Job", function(this, ...) {
  path <- this$logPath;
  if (is.null(path))
    path <- getOutputPath(this);
  if (!isAbsolutePath(path))
    path <- filePath(getPath(this), path, expandLinks="any");
  path <- filePath(path, expandLinks="any");
  getAbsolutePath(path);
})


########################################################################/**
# @RdocMethod getInputPath
#
# @title "Gets the input path of the job"
#
# \description{
#  @get "title".
#  It is recommended to put data files here, which should be read
#  by the job. 
#
#  The input path should be named \code{"input/"}. In order to read data 
#  from two different directories, additional input paths may be
#  \code{"input2/"}, \code{"input3/"} etc. These are specified using the
#  \code{index} argument. This makes it possible to "import" data from
#  the output of various other batch jobs via links.
# }
#
# @synopsis
#
# \arguments{
#  \item{index}{An @integer specifying which input path to query.}
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
setMethodS3("getInputPath", "Job", function(this, index=1, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'index'
  if (!is.numeric(index))
    throw("Argument 'index' must be numeric: ", mode(index));
  if (length(index) != 1)
    throw("Argument 'index' must be of length one: ", length(index));
  if (index < 1)
    throw("Argument 'index' must be a postive integer: ", index);

  path <- "input";
  if (index > 1)
    path <- paste(path, index, sep="");

  filePath(getRoot(this), path, expandLinks="any");
})



########################################################################/**
# @RdocMethod getName
#
# @title "Gets the name of the job"
#
# \description{
#  @get "title".
#
#  The name of a job is by definition equal to the name of the basename of
#  the job directory, that is, identical to \code{basename(getPath(job))}.
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
#   @see "basename".
#   @seemethod "getLabel".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("getName", "Job", function(this, ...) {
  basename(getPath(this));
})



########################################################################/**
# @RdocMethod getLabel
#
# @title "Gets the label of the job"
#
# \description{
#  @get "title".
#
#  If the label is not set, a default string of format
#  \code{<jobname>_<user>@<host>} will be returned.
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
#   @seemethod "getName".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("getLabel", "Job", function(this, ...) {
  label <- getField(this, ".jobLabel");
  if (is.null(label)) {
    label <- getName(this);
    label <- paste(label, "_", System$getUsername(), sep="");
    label <- paste(label, "@", System$getHostname(), sep="");
#    label <- paste(label, "_", getInternalAddress(this), sep="");
  }
  label;
})



########################################################################/**
# @RdocMethod setLabel
#
# @title "Sets the label of the job"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{label}{A @character string. If @NULL, label is reset.}
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
setMethodS3("setLabel", "Job", function(this, label=NULL, ...) {
  setField(this, ".jobLabel", as.character(label));
})





########################################################################/**
# @RdocMethod isExisting
#
# @title "Checks if the job exists"
#
# \description{
#  @get "title". A job exists if the job directory exists.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns @TRUE if the job exists, otherwise @FALSE.
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
setMethodS3("isExisting", "Job", function(this, ...) {
  isDirectory(getPath(this));
})



########################################################################/**
# @RdocMethod isStarted
#
# @title "Checks if the job is started or not"
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
#  Returns @TRUE if the job is started, otherwise @FALSE.
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
setMethodS3("isStarted", "Job", function(this, ...) {
  (getStatus(this) != "is not started")
})




########################################################################/**
# @RdocMethod wasSuccessful
#
# @title "Checks if the job was completed successfully"
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
#  Returns @TRUE if the job was successful, otherwise @FALSE.
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
setMethodS3("wasSuccessful", "Job", function(this, ...) {
  (getStatus(this) == "has finished");
})



########################################################################/**
# @RdocMethod hasFailed
#
# @title "Checks if the job failed"
#
# \description{
#  @get "title" either during its main loop or during it finalization.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns @TRUE if the job failed, otherwise @FALSE.
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
setMethodS3("hasFailed", "Job", function(this, ...) {
  (getStatus(this) %in% c("has failed", "failed while finalizing"));
})



########################################################################/**
# @RdocMethod isErroneous
#
# @title "Checks if the job is erroneous"
#
# \description{
#  @get "title". A job that fails during setup is erroneous. The most
#  common reason is that the source code in the src/ directory or the
#  job directory contains errors.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns @TRUE if the job is erroneous, otherwise @FALSE.
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
setMethodS3("isErroneous", "Job", function(this, ...) {
  (getStatus(this) == "is erroneous");
})




########################################################################/**
# @RdocMethod wasInterrupted
#
# @title "Checks if the job was interrupted"
#
# \description{
#  @get "title" either during its main loop or during it finalization.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns @TRUE if the job was interrupted, otherwise @FALSE.
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
setMethodS3("wasInterrupted", "Job", function(this, ...) {
  (getStatus(this) %in% c("was interrupt", 
                                      "was interrupted while finalizing"));
})




########################################################################/**
# @RdocMethod isDone
#
# @title "Checks if the job is done"
#
# \description{
#  @get "title". A job is done if it was sucessfully finished, or if an
#  error or interrupt was detected.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns @TRUE if the job is done, otherwise @FALSE.
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
setMethodS3("isDone", "Job", function(this, ...) {
  isStarted(this) && (wasSuccessful(this) || wasInterrupted(this) || 
                                                          hasFailed(this));
})


setMethodS3("isFinished", "Job", function(this, ...) {
  (isDone(this) && wasSuccessful(this));
})


########################################################################/**
# @RdocMethod getStatus
#
# @title "Gets the status of the job"
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
#   @seemethod "setStatus".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################s
setMethodS3("getStatus", "Job", function(this, ...) {
  getField(this, ".status");
}, protected=TRUE)



########################################################################/**
# @RdocMethod setStatus
#
# @title "Sets the status of the job"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{status}{A @character string.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns invisibly the previous status as a @character string.
# }
#
# \seealso{
#   @seemethod "getStatus".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################s
setMethodS3("setStatus", "Job", function(this, status, ...) {
  setField(this, ".status", status);
}, protected=TRUE)





########################################################################/**
# @RdocMethod lock
#
# @title "Locks the job"
#
# \description{
#  @get "title". When a job is locked, no other \code{Job} objects can 
#  lock and run the same job code.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns @TRUE if job is locked afterwards, otherwise @FALSE.
#  If job does not exist or is already locked, an exception is thrown.
# }
#
# \seealso{
#   @seemethod "isLocked" and @seemethod "unlock".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("lock", "Job", function(this, ...) {
  log <- getLog(this);

  log && cat(log, "Trying to lock job.");

  if (isLocked(this))
    throw("Job is already locked.");

  if (!isExisting(this))
    throw("Job does not exist: ", getPath(this));

  # Create and open lock file without closing it in order
  # to prevent another process from deleting it
  lockFile <- file.path(getPath(this), ".lock");
  this$.lock <- file(lockFile, open="w");
  s <- paste("This job was locked at ", as.character(date()), 
     " by process ", getLabel(this), 
     " running on host ", System$getHostname(), 
     " by user ", System$getUsername(), ".", sep="");
  s <- paste(s, " Software: ", base::R.version.string, ".", sep="");
  write(file=this$.lock, s);           # Write tracking info for debugging

  log && cat(log, "Job successfully locked.");

  TRUE;
})



########################################################################/**
# @RdocMethod isLocked
#
# @title "Checks if a job is locked"
#
# \description{
#   @get "title" either by this \code{Job} object or another.
#   A job is defined to be locked if an \emph{unremovable} lock file 
#   (\code{.lock}) exists. This function will try to delete the lock file
#   before checking in order to avoid forgotten or false lock files.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns @TRUE if job is locked, otherwise @FALSE.
# }
#
# \seealso{
#   @seemethod "lock" and @seemethod "unlock".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("isLocked", "Job", function(this, ...) {
  # If and only if lock file exists then the job is locked
  lockFile <- file.path(getPath(this), ".lock");
  if (!file.exists(lockFile))
    return(FALSE);

  # Lock file found, but try first to remove it...
  file.remove(lockFile);

  # ...and recheck.
  file.exists(lockFile);
})



########################################################################/**
# @RdocMethod unlock
#
# @title "Unlocks the job"
#
# \description{
#  @get "title".
#
#  This method is called by various methods. All methods that locks a
#  job will also unlock it. If a \code{Job} object is delete, it will
#  also be unlocked when deallocated (by the garbage collector), if locked.
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns @TRUE if job is unlocked afterwards, otherwise @FALSE.
#  If job does not exist, an exception is thrown.
# }
#
# \seealso{
#   @seemethod "isLocked" and @seemethod "lock".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("unlock", "Job", function(this, ...) {
  log <- getLog(this);

  log && cat(log, "Trying to unlock job.");
  if (!isExisting(this))
    throw("Job does not exist: ", getPath(this));

  # First, check if locked
  if (!isLocked(this)) {
    log && cat(log, "Job was not locked.");
    return(FALSE);
  }

  # Second, we own the lock file and it is open, close it first
  if (inherits(this$.lock, "connection") && isOpen(this$.lock)) {
    close(this$.lock);
    this$.lock <- NULL;
    force <- TRUE;
  }

  # Finally, try to remove the lock file. This will be accepted either if
  # we owned it and just closed it, or if the process that created it was
  # killed and failed to remove it, then another job can remove the file.
  lockFile <- file.path(getPath(this), ".lock");
  res <- file.remove(lockFile);
  if (res) {
    log && cat(log, "Job successfully unlocked.");
  } else {
    log && cat(log, "Job not unlocked.");
  }
  res;
})




########################################################################/**
# @RdocMethod sourceHotCode
#
# @title "Sources and removes code in the hot/ and src/hot/ directories"
#
# \description{
#  @get "title", that is, directories named hot/ in both the current
#  job directory as well as the common src/ directory, making it possible
#  either patch specific jobs or all jobs. If not put in a directory
#  named 'global', the code will be sourced into the calling environment, 
#  which typically is "inside" the \code{onRun()} function.
#
#  \emph{WARNING: By default, hot plugin files that were sourced, are 
#  removed afterward!}
#
#  By calling this method repetably in \code{onRun(job)}, 
#  say, in the main iteration loop, it is possible to update code while
#  a job is running. 
#  One situation where this has been found to be useful is when it takes
#  literally days to process a job and when almost done you have been
#  told that there will be a power-shut before you job finishes. By 
#  plugging in new code this way, you can save you current session and
#  shut it down nicely to be continued later. Note, there is currently
#  now methods provided by \code{R.batch} that does this for you; you
#  have to implement reloading etc. yourself.
#
#  Errors that occurs while sourcing the hot code, for instance syntax
#  errors but also evaluation errors, are \emph{skipped} with a warning
#  and recorded in the log file. This way you will not killa process by
#  misstake that have been running for days.
#
#  Note that code in hot/ and src/hot/ will also be source by
#  @seemethod "setup" on startup.
# }
#
# @synopsis
#
# \arguments{
#  \item{remove}{If @TRUE, hot patch files will be removed after being
#        read, otherwise not.}
#  \item{envir}{The @environment where the source code should be stored.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns (invisibly) a @vector of the files sourced.
# }
#
# \details{
#  All scripts files are evaluated with \code{source()} to the 
#  \emph{local} working environment of this job. That is, no global 
#  objects will be overwritten.
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
setMethodS3("sourceHotCode", "Job", function(this, remove=TRUE, envir=this$.env, ...) {
  # Source the code in the src/hot/ directory.
  hotPath <- filePath(getRoot(this), "src", "hot", expandLinks="any");
  files1 <- sourceDirectoryWithPreprocessor(this, 
                            path=hotPath, envir=envir, onError="warning");
  # Source the code in the job's hot/ directory.
  files2 <- sourceDirectoryWithPreprocessor(this, 
                              path="hot", envir=envir, onError="warning");

  files <- c(files1, files2);

  if (remove) {
    tryCatch({
      file.remove(files);
    }, error = function(error) {
    })
  }

  invisible(files);
})


setMethodS3("sourceDirectoryWithPreprocessor", "Job", function(this, ...) {
  # Workaround: sourceDirectory() calls sourceTo() calls source() that call 
  # capabilities("iconv"), which may generate (non-catchable) message
  # 'Xlib: connection to "<host>:0.0" refused by server'; we do not 
  # need "iconv" here.
  # Not needed anymore.  See HISTORY.  /HB 2006-09-12
##  orgCapabilities <- base::capabilities;
##  assign("capabilities", function(...) FALSE, 
##                                    pos=which(search() == "package:base")); 
##  # Remove above workaround afterward
##  on.exit({
##    assign("capabilities", orgCapabilities, 
##                                    pos=which(search() == "package:base")); 
##  }, add=TRUE);

  log <- getLog(this);
  files <- NULL;
  tryCatch({
    # Set preprocessing hooks
    oldHooks <- getHook("sourceTo/onPreprocess");
    setHook("sourceTo/onPreprocess", function(lines, ...) { 
      tryCatch({
        lines <- LComments$compile(lines=lines);
        if (log) {
          cat(log, level=-80, "Source after pre-processing:");
          code <- displayCode(code=lines, pager="none");
          log$asGString <- FALSE;
          cat(log, level=-80, code);
          log$asGString <- TRUE;
        }
      }, error = function(ex) {
        log && cat(log, "Error when pre-compiling source code:"):
        log && print(log, ex);
        print(ex);
      })
      lines;
    }, action="replace")

    output <- capture.output({
      files <- sourceDirectory(..., verbose=log);
    });

    if (length(output) > 0 && any(nchar(output) > 0)) {
      log && cat(log, "Output detected while loading plugin source:");
      log && cat(log, paste(output, collapse="\n", sep="\n"));
    }
  }, finally = {
    # Reset hooks
    setHook("sourceTo/onPreprocess", oldHooks, action="replace");
  }) 

  invisible(files);
}, protected=TRUE)


########################################################################/**
# @RdocMethod setup
#
# @title "Initiates the job"
#
# \description{
#  @get "title" by evaluating script files in the source directory and
#  the job directory. 
#
#  This method is called by @seemethod "run".
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns (invisibly) a @vector of the files sourced.
# }
#
# \details{
#  All scripts files are evaluated with \code{source()} to the 
#  \emph{local} working environment of this job. That is, no global 
#  objects will be overwritten.
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
setMethodS3("setup", "Job", function(this, ...) {
  # Hmmm, here I actually do a hack and use this$.env as if it is
  # public and well document, but it isn't. Maybe, there should
  # be a getEnvironment() for the Object class? /HB 2005-02-18
  envir <- this$.env;


  touch(this);

  log <- getLog(this);
  log && header(log, "Setting up job"); 

  # Source the code in the src/ directory
  srcPath <- filePath(getRoot(this), "src", expandLinks="any");

  files1 <- sourceDirectoryWithPreprocessor(this, path=srcPath, envir=envir);

  # Source the code in the job directory
  files2 <- sourceDirectoryWithPreprocessor(this, path=getPath(this), envir=envir);

  log && cat(log, "Job was successfully setup.");

  files <- c(files1, files2);

  touch(this);

  invisible(files);
})


setMethodS3("resetToRun", "Job", function(this, ...) {
  setField(this, ".status", "is not started");
  setField(this, ".jobStartTime", NA);
  setField(this, ".jobRunTime", NA);
  setField(this, ".jobInterruptTime", NA);
  setField(this, ".jobErrorTime", NA);
  setField(this, ".jobFinallyTime", NA);
  setField(this, ".jobStopTime", NA);
  untouch(this);
}, protected=TRUE)



########################################################################/**
# @RdocMethod run
#
# @title "Runs the job"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#  \item{reset}{If @TRUE, job is first reset, otherwise not.}
#  \item{sink}{If @TRUE, all output is sinked to a file, otherwise not.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns a @TRUE if job finished successfully, otherwise @FALSE.
# }
# 
# \details{
#  First the job is locked. Then @seemethod "setup" is called, and the 
#  current working directory is set to the job path, that is, any script
#  or functions calls are evaluated within the job directory.
#  Then the handle method \code{onStart()} followed by \code{onRun()} are 
#  evaluated with this object as the first argument. 
#  If an error occurs while evaluating these, it is caught and
#  \code{onError()} is evaluated.
#  Similar, if an interrupt, that is Ctrl-C (\code{SIGINT}), occurs it is
#  caught and \code{onInterrupt()} is evaluated.
#  Finally, handle method \code{onFinally()} is (always) evaluated.
#  Errors or interrupts occuring while evaluating this latter method, will
#  \emph{not} call \code{onInterrupt()} and \code{onError()}, 
#  respectively.
#
#  Note also, that if errors or additional interrupts occurs while 
#  evaluating \code{onInterrupt()} or \code{onError()} these will 
#  \emph{not} be caught. This can be an issue if for instance the user
#  holds down Ctrl-C. Unfortunately, there is no solution to the problem
#  at the moment [1].  
# }
#
# \references{
#   [1] H. Bengtsson, \emph{Exception handling in R}, 2004.
#       \url{http://www.maths.lth.se/help/R/}
# }
#
# \seealso{
#   @seemethod "lock".
#   @seemethod "setup".
#   @seemethod "isExisting".
#   @seemethod "isStarted".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("run", "Job", function(this, reset=FALSE, sink=TRUE, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'reset'
  if (!is.logical(reset))
    throw("Argument 'reset' is not logical: ", mode(reset));

  # Argument 'sink'
  if (!is.logical(sink))
    throw("Argument 'sink' is not logical: ", mode(sink));


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Setting up logging
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  log <- getLog(this);
  if (log) {
    enter(log, "Called run(). Running job"); 
    on.exit(exit(log), add=TRUE);
  }

  log && pushState(log);
  on.exit(popState(log), add=TRUE);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert that the job exists
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (!isDirectory(getPath(this))) {
    msg <- paste("Job directory does not exist:", as.character(getPath(this))):
    log && enter(log, msg);
    throw(msg);
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert that job is not running
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (isStarted(this)) {
    msg <- "Cannot run job. Job is already started.";
    log && enter(log, msg);
    throw(msg);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Lock job
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (!isLocked(this)) {
    lock(this);
    on.exit(unlock(this), add=TRUE);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert that all dependencies are fulfilled and processed before this Job.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#  log && cat(log, "Checking dependencies.");
#  deps <- getDependencies(this);
#  if (length(deps) > 0) {
#    throw("Cannot run job. Found unfinished or jobs that have be updated after this job, which need to be rerun: ", paste(unlist(lapply(deps, FUN=getName)), collapse=", "));
#  }
#  rm(deps);

  log && cat(log, "Touching job (updating last modified date).");
  touch(this);

  # Set job status
  setStatus(this, "is not started");


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Record the current directory to return to at the end and 
  # set working directory to job directory.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  log && cat(log, "Set working directory to job directory: ", getPath(this));
  opwd <- getwd();
  on.exit(setwd(opwd), add=TRUE);
  setwd(getPath(this));
  log && cat(log, "Current working directory: ", getwd());


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Read all source and settings files
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  log && cat(log, "Sinking output.");
  # Sink? (Cannot be done from within tryCatch()! /HB 2005-03-02).
  if (sink && sink(this)) {
    on.exit(if (isSinked(this)) unsink(this), add=TRUE);
  }

  touch(this);
  tryCatch({
    setField(this, ".jobSetupTime", Sys.time());

    # Reset all warnings
    showWarnings(this, show=FALSE);

    # Print and reset warnings 
    showWarnings(this);

    setup(this);

    # Print and reset warnings 
    showWarnings(this);

  }, error=function(error) {
    log && cat(log, "Failed to setup job. ", as.character(error));
    print(error);
   setStatus(this, "is erroneous");
    setField(this, ".jobErrorTime", Sys.time());
    setField(this, ".jobStopTime", Sys.time());
    this$job <- NULL;
    gc();
    # NOTE: It is not possible to return() from a tryCatch()!!!!
  })

  touch(this);
  if (isErroneous(this))
    return(FALSE);

  # Add itself ('this') as a variable 'job' available to all job scripts.
  this$job <- this;

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Reset job
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  touch(this);
  tryCatch({
    if (reset) {
      setField(this, ".jobResetTime", Sys.time());
      log && header(log, "onReset()"); 
      log && enter(log, level=-20, "Calling onReset().");

      # Assure that the correct state of the Verbose object is 
      # retained afterwards.
      log && pushState(log); 

      # Call onReset() from within the local job environment
      eval(substitute(
        onReset(this), 
        list(this=this))
      , envir=this$.env);

      log && popState(log);
      log && warnings(log);
      log && exit(log); 

      # Print and reset warnings 
      showWarnings(this);
    }
  }, error=function(ex) {
    log && popState(log);
    log && print(log, ex);
    log && warnings(log);
    log && exit(log, suffix="...failed"); 
    print(ex);
    setStatus(this, "has failed");
    setField(this, ".jobErrorTime", Sys.time());
    setField(this, ".jobStopTime", Sys.time());
    this$job <- NULL;
    gc();
    # NOTE: It is not possible to return() from a tryCatch()!!!!
  })

  touch(this);
  if (hasFailed(this))
    return(FALSE);


  touch(this);
  tryCatch({
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # onRestart()
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    gc();                                 # Free as much memory as possible
    if (hasStoredImage(this)) {
      fields <- loadStoredImage(this);
      log && header(log, "onRestart()"); 
      log && enter(log, level=-20, "Calling onRestart().");

      # Assure that the correct state of the Verbose object is 
      # retained afterwards.
      log && pushState(log); 

      # Call onRestart() from within the local job environment
      eval(substitute(
        onRestart(this, fields=fields), 
        list(this=this))
      , envir=this$.env);

      log && popState(log);
      log && warnings(log);
      log && exit(log); 

      # Print and reset warnings 
      showWarnings(this);
    }

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # onStart()
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    gc();                                 # Free as much memory as possible

    setStatus(this, "is started");
    setField(this, ".jobStartTime", Sys.time());
    log && header(log, "onStart()"); 
    log && enter(log, level=-20, "Calling onStart().");

    # Assure that the correct state of the Verbose object is 
    # retained afterwards.
    log && pushState(log); 

    # Call onStart() from within the local job environment
    eval(substitute(
      onStart(this), 
      list(this=this))
    , envir=this$.env);

    log && popState(log);
    log && warnings(log);
    log && exit(log); 

    # Print and reset warnings 
    showWarnings(this);

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # onRun()
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    gc();                                 # Free as much memory as possible
    setStatus(this, "is running");
    setField(this, ".jobRunTime", Sys.time());
    log && header(log, "onRun()"); 
    log && enter(log, level=-20, "Calling onRun().");

    # Assure that the correct state of the Verbose object is 
    # retained afterwards.
    log && pushState(log); 

    # Call onRun() from within the local job environment
    eval(substitute(
      onRun(this), 
      list(this=this))
    , envir=this$.env);

    log && popState(log);
    log && warnings(log);
    log && exit(log); 

    setStatus(this, "has finished");

    # Print and reset warnings 
    showWarnings(this);
  }, interrupt=function(interrupt) {
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # onInterrupt()
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    gc();                                 # Free as much memory as possible
    setStatus(this, "was interrupt");
    setField(this, ".jobInterruptTime", Sys.time());
    this$onInterrupt(this, interrupt);
    log && cat(log, "Job was interrupted.");
    log && header(log, "onInterrupt()"); 
    log && enter(log, level=-20, "Calling onInterrupt().");

    # Assure that the correct state of the Verbose object is 
    # retained afterwards.
    log && pushState(log); 

    # Call onInterrupt() from within the local job environment
    eval(substitute(
      onInterrupt(this, interrupt), 
      list(this=this, interrupt=interrupt))
    , envir=this$.env);

    log && popState(log);
    log && warnings(log);
    log && exit(log); 

    log && cat(log, "onInterrupt() done.");

    # Print and reset warnings 
    showWarnings(this);
  }, error=function(ex) {
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # onError()
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    gc();                                 # Free as much memory as possible
    setStatus(this, "has failed");
    setField(this, ".jobErrorTime", Sys.time());
    log && cat(log, "An error occured while processing job.");

    log && popState(log);
    log && print(log, ex);
    log && warnings(log);
    log && exit(log, suffix="...failed"); 
    print(ex);

    log && header(log, "onError()"); 
    log && enter(log, level=-20, "Calling onError().");

    # Assure that the correct state of the Verbose object is 
    # retained afterwards.
    log && pushState(log); 

    # Call onError() from within the local job environment
    eval(substitute(
      onError(this, error), 
      list(this=this, error=ex))
    , envir=this$.env);

    log && popState(log);
    log && warnings(log);
    log && exit(log); 

    # Print and reset warnings 
    showWarnings(this);
  }, finally={
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # onFinally()
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    gc();                                 # Free as much memory as possible
    setField(this, ".jobFinallyTime", Sys.time());
    log && header(log, "onFinally()"); 
    tryCatch({
      log && enter(log, level=-20, "Calling onFinally().");

      # Assure that the correct state of the Verbose object is 
      # retained afterwards.
      log && pushState(log); 

      # Call onFinally() from within the local job environment
      eval(substitute(
        onFinally(this), 
        list(this=this))
      , envir=this$.env);

      log && popState(log);
      log && warnings(log);
      log && exit(log); 
    }, interrupt=function(interrupt) {
      setStatus(this, "was interrupted while finalizing");
    }, error=function(ex) {
      log && popState(log);
      log && print(log, ex);
      log && warnings(log);
      log && exit(log, suffix="...failed"); 
      print(ex);
    })

    setField(this, ".jobStopTime", Sys.time());

    this$job <- NULL;
  })

  touch(this);

  gc();                                   # Free as much memory as possible
  res <- wasSuccessful(this);
  if (log) {
    if (res) {
      cat(log, "Job ran successfully.");
    } else {
      cat(log, "Job did not run successfully.");
    }
    dt <- getField(this, ".jobStopTime") - getField(this, ".jobStartTime");
    cat(log, "Total process time: ", as.character(dt), " seconds.");
  }

  res;
})





########################################################################/**
# @RdocMethod resetLog
#
# @title "Reset log by removing log file"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#  \item{backup}{If @TRUE, log file is backuped before deleted.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns @TRUE if log file was reset, otherwise @FALSE.
# }
#
# \seealso{
#   @seemethod "writeToLog".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("resetLog", "Job", function(this, backup=TRUE, ...) {
  filename <- paste(getLabel(this), ".log", sep="");
  filename <- file.path(getOutputPath(this), filename);

  # Nothing to do?
  if (!isFile(filename))
    return(FALSE);

  # Create a backup?
  if (backup) {
    backupLog(this);
  } else {
    file.remove(filename);
  }

  TRUE;
}, protected=TRUE)


########################################################################/**
# @RdocMethod writeToLog
#
# @title "Writes to log file"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Arguments passed to the @see "R.utils::cat.Verbose" method.}
#  \item{collapse}{A @character string to concatenate objects in a list.}
# }
#
# \value{
#  Returns nothing.
# }
#
# \seealso{
#   @seemethod "resetLog".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("writeToLog", "Job", function(this, ..., collapse=" ") {
  # Write to log file
  log <- getLog(this);
  log && cat(log, ..., collapse=collapse);

  # Write to verbose object too.
  verbose <- this$.verbose;
  verbose && cat(verbose, ..., collapse=collapse);
}, protected=TRUE)


setMethodS3("backupLog", "Job", function(this, ...) {
  filename <- paste(getLabel(this), ".log", sep="");
  path <- getOutputPath(this);
  filename <- file.path(path, filename);

  # Nothing to do?
  if (!isFile(filename))
    return(FALSE);

  pattern <- paste("^", filename, sep="");
  logs <- list.files(pattern=pattern, path=path);
  count <- gsub(pattern, "", logs);
  count <- gsub("[.]", "", count);
  if (identical(count, "")) {
    count <- 0;
  } else {
    count <- max(as.integer(count), na.rm=TRUE);
  }
  backup <- sprintf("%s.%03d", filename, as.integer(count+1));
  from <- file.path(path, filename);
  to <- file.path(path, backup);
  file.rename(from, to);

  TRUE;
}, protected=TRUE)


setMethodS3("getLog", "Job", function(this, ...) {
  log <- this$log;
  if (is.null(log)) {
    # If there is already a log file, back it up.
    backupLog(this);

    filename <- paste(getLabel(this), ".log", sep="");
    filename <- file.path(getLogPath(this), filename);
    threshold <- -1;
    if (inherits(this$.verbose, "Verbose"))
      threshold <- getThreshold(this$.verbose);
    log <- Verbose(filename, removeFile=TRUE, timestamp=TRUE, threshold=threshold);
    this$log <- log;
  }

  log;
})


setMethodS3("listDir", "Job", function(this, ...) {
  list.files(getPath(this), ...);
})





########################################################################/**
# @RdocMethod isSinked
#
# @title "Checks if job output is sinked to file"
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
#  Returns @TRUE if either output or message is sinked, otherwise @FALSE.
# }
#
# \seealso{
#   @seemethod "sink" and @seemethod "unsink".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("isSinked", "Job", function(this, ...) {
  if (is.null(this$.sink))
    this$.sink <- list(output=0, message=0, file=NULL);
  
  if (this$.sink[["output"]] > 0)
    return(TRUE);

  if (this$.sink[["message"]] > 0)
    return(TRUE);

  FALSE;
})


########################################################################/**
# @RdocMethod sink
#
# @title "Sinks job output"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#  \item{output}{If @TRUE, standard output is sinked, otherwise not.}
#  \item{message}{If @TRUE, standard error is sinked, otherwise not.
#                \emph{Currently ignored!}}
#  \item{split}{If @TRUE, output will be sent to the sink and to the
#              current output stream. The standard error cannot be split.}
#  \item{path}{Path (directory) for the sink file. If @NULL, the current
#              (job) directory is used.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns nothing.
# }
#
# \seealso{
#   @seemethod "isSinked" and @seemethod "unsink".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("sink", "Job", function(this, output=TRUE, message=TRUE, split=TRUE, path=getOutputPath(this), ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'output'
  if (!is.logical(output))
    throw("Argument 'output' is not logical: ", mode(output));

  # Argument 'message'
  if (!is.logical(message))
    throw("Argument 'message' is not logical: ", mode(message));

  # Argument 'split'
  if (!is.logical(split))
    throw("Argument 'split' is not logical: ", mode(split));

  # Create internal sink record, if missing.
  if (is.null(this$.sink))
    this$.sink <- list(output=0, message=0);

  # Only sink, if not already done
  output <- (output && (this$.sink[["output"]] == 0));
  message <- (message && (this$.sink[["message"]] == 0));

  if (!output && !message) {
    # Nothing to do.
    return(FALSE);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert that everything is ok, before trying to sink anything.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  log <- getLog(this);
  if (output) {
    log && cat(log, "Trying to sink output.");
  }

  if (message) {
    log && cat(log, "Trying to sink message.");
  }

  file <- this$.sink[["file"]];
  if (is.null(file)) {
    # Create a sink file. Overwrite existing.
    filename <- paste(getLabel(this), ".out", sep="");
    filename <- file.path(path, filename);
    file <- file(filename, open="wt");
    this$.sink[["file"]] <- file;
  }

  if (!inherits(file, "connection") || !isOpen(file)) {
     throw("Sink 'file' is not a connection or is not opened.");
  }

  # Sink output
  if (output) {
    # Sink to file
    sink(file, append=TRUE, type="output", split=split);

    # Record the number of output diversions. If not the same
    # when trying to unsink(), then it is an error.
    this$.sink[["output"]] <- sink.number(type="output");

    log && cat(log, "Sinked output to file: ", filename);
  }

  # Sink message
  if (message) {
    # Sink to an already open file connection. See ?sink.
    sink(file, append=TRUE, type="message");

    # Record the connection number for the message sink.
    this$.sink[["message"]] <- sink.number(type="message");

    log && cat(log, "Sinked message to file: ", filename);
  }

  invisible(isSinked(this));
})


########################################################################/**
# @RdocMethod unsink
#
# @title "Unsinks job output"
#
# \description{
#  @get "title". 
# }
#
# @synopsis
#
# \arguments{
#  \item{output}{If @TRUE, standard output is unsinked, otherwise not.}
#  \item{message}{If @TRUE, standard error is unsinked, otherwise not.}
#  \item{...}{Not used.}
# }
#
# \value{
#  Returns nothing.
# }
#
# \seealso{
#   @seemethod "isSinked" and @seemethod "sink".
#   @seeclass
# }
#
# @author
#
# @keyword programming
#**/#######################################################################
setMethodS3("unsink", "Job", function(this, output=TRUE, message=TRUE, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'output'
  if (!is.logical(output))
    throw("Argument 'output' is not logical: ", mode(output));

  # Argument 'message'
  if (!is.logical(message))
    throw("Argument 'message' is not logical: ", mode(message));

  # Only unsink, if sinked...
  output <- (output && (this$.sink[["output"]] > 0));
  message <- (message && (this$.sink[["message"]] > 0));

  if (!output && !message) {
    # Nothing to do.
    return(FALSE);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Assert that everything is ok, before trying to sink anything.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  log <- getLog(this);
  if (message) {
    log && cat(log, "Trying to unsink message.");

    if (sink.number(type="message") == 0) {
      msg <- "Excepted message sink to be open, but it is not.";
      log && cat(log, msg);
      warning(msg);
      message <- FALSE;
    }
  }

  if (output) { 
    log && cat(log, "Trying to unsink output.");

    # Assert existance of output sinks
    if (sink.number(type="output") == 0) {
      msg <- "Cannot close Job sink. No output sink diversions exist.";
      log && cat(log, msg);
      throw(msg);
    }

    # Has "someone else" closed the sink before "us"?
    if (this$.sink[["output"]] > sink.number(type="output")) {
      msg <- paste("The Job sink was inappropriately closed by a call to sink(): ", this$.sink[["output"]], " > ", sink.number(type="output"), sep="");
      log && cat(log, msg);
      warning(msg);
    }

    # Assert that the same sink is closed as the one opened (recorded)
    if (this$.sink[["output"]] < sink.number(type="output")) {
      msg <- paste("Trying to unsink Job when succeeding sink is active: ", 
          this$.sink[["output"]], " > ", sink.number(type="output"), sep="");
      log && cat(log, msg);
      warning(msg);
    }
  }

  # Close sink output
  if (output) {
    # Close sink
    sink(type="output");

    this$.sink[["output"]] <- 0;

    log && cat(log, "Unsinked output.");
  }

  # Close sink message
  if (message) {
    sink(type="message");

    # Send messages to standard error.
    sink(file=stderr(), type="message");

    this$.sink[["message"]] <- 0; 

    log && cat(log, "Unsinked message.");
  }


  # If not sinked, close file connection...
  if (!isSinked(this)) {
    file <- this$.sink[["file"]];
    if (inherits(file, "connection") && isOpen(file))
      close(file);
    this$.sink[["file"]] <- NULL;
  }

  invisible(isSinked(this));
})


###########################################################################
# HISTORY: 
# 2009-06-06
# o BUG FIX: "Unnamed" argument 'list' in all substitute(..., list=...). 
# 2006-09-12
# o Removed the workaround that redefined capabilities() temporarily in
#   sourceDirectoryWithPreprocessor().  This was due to a temporary DNS
#   failure at my test site and should normally not be necessary.  The
#   reason for not leaving it in there is that it has become harder to
#   patch that function inside the namespace-protected base environment.
#   For more details on the problem I experienced, see my R-devel thread
#   "[Rd] capabilities() and non-catchable messages" on June 20-28, 2005.
# 2005-12-05
# o Starting to make more use of the log Verbose object; writeToLog()
#   should not be needed anymore.
# 2005-12-02
# o Remove all *use* of code for dependencies and requirements.
# o Now the log file is using path according to getLogPath().
# o Moving toward using the Verbose class for logging too.
# o Making more use of Arguments class.
# o Using throw() all over instead of stop().
# 2005-10-20
# o BUG FIX: Job() would generate an error. This prevented the Rdoc 
#   compiler to run etc.
# 2005-07-18
# o Now using sourceDirectory() of R.utils. Removed same method of Job.
# 2005-05-23
# o Added argument 'criteria' to getDependencies(). This makes it possible
#   to see if a Job dependends on other Job:s that have been updated so
#   that the Job has to be (re-)run.
# o Added isNewer().
# 2005-05-21
# o Added equals().
# o Added lastModified() and touch().
# o Added protected resetToRun().
# o Now getStatus() and setStatus() is persistent fields.
# o Added setStatus().
# o Added semi-generic support for persistent fields via protected methods
#   getField() and setField(), addPersistentField(), isPersistentField(),
#   and getPersistentPath().
# o Added getName().
# 2005-05-03
# o Added '...' to setLabel().
# 2005-03-14
# o Added optional argument 'index' to getInputPath().
# 2005-03-11
# o Added getFigurePath(), getResultPath(), and getLogPath().
# o Added setLabel().
# o Now an image is save also when an error occurs.
# o sourceHotCode() is using environment 'job' now and not the parent one.
# 2005-03-10
# o Added argument 'remove=TRUE' to sourceHotCode() to remove source code
#   files afterwards.
# o Now sourceDirectory() returns the files that were sourced.
# o Now only sourced hot code is written to log, otherwise the log file
#   would grow unnecessarily large when calling sourceHotCode() often
#   and when no hot code existed. Updated sourceDirectory() too.
# 2005-03-09
# o Added sourceHotCode() to provide hot-plugin of source code.
# o Added protected sourceDirectory(), which earlier for a local function
#   in setup().
# 2005-03-02
# o Removed resetLog() in run(), because it is called in getNextJob() in
#   the JobBatch class, and should not be called twice.
# o Now log files and sinks are sent to output/ by default. This will
#   make it much easier to get an overview of the status of all jobs.
#   This will lower the need for the user to open the job directory,
#   which in turn will lower the risk for the directory to be locked
#   when trying to move it.
# o Added private showWarnings() to dump and reset warnings when running.
# o Added sink() and unsink(). The methods checks for mistakes such as
#   if a sink is already, or a sink has been open but not closed.
# o Removed "internal address" from the defaults in getLabel().
# 2005-03-01
# o BUG FIX: saveImage() used a non-existing reference variable.
# o Added methods getOutputPath() and getInputPath() and removed 
#   "internal" ditto.
# o Added "basename" to getLabel().
# 2005-02-24
# o Added saveImage().
# 2005-02-20
# o Added writeToLog().
# o Added onReset() and onRestart().
# 2005-02-19
# o Now also "private" directories and files are sourced, e.g. '.dir'.
# o Now summariesJobs() warns and highlight duplicated jobs.
# o Duplicated jobs are ignore with a warning.
# o Added static getJobsSummary() and summariesJobs().
# o Phasing out the settings features. It is much easier to just do it
#   with plain R code.
# o Rename all .onNNN() functions to onNNN().
# o Added "job" functions getInputPath() and getOutputPath().
# 2005-02-18
# o Removed several 'verbose' arguments by making it a field of Job.
# o Added getSubDirectory() and validateSubdirectories().
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
