library(R.oo)
library(R.basic)
source("~/braju.com.R/patches/R.jobs/Job.R")
source("~/braju.com.R/patches/R.basic/sourceTo.R")
source("~/braju.com.R/patches/patchAll.R")

main <- function() {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  getOption <- function(name, defaultValue=NULL) {
    args <- commandArgs();
    args <- args[-1];
    key <- paste("--", name, sep="");
    pos <- which(args == key);
    if (length(pos) == 0)
      return(defaultValue);
    # If the option is specified more than once, use the last value
    pos <- pos[length(pos)];
    if (length(args) < pos+1)
      stop(paste("Missing value for option: ", key, sep=""));
    value <- args[pos+1];
    cat(key, "==", value, "\n");
    value;
  } # getOption()
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Parse command line arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  jobsPath <- getOption("jobsPath", "jobs");
  todoPath <- getOption("todoPath", "todo");
  runningPath <- getOption("runningPath", "running");
  finishedPath <- getOption("finishedPath", "finished");
  failedPath <- getOption("failedPath", "failed");
  interruptedPath <- getOption("interruptedPath", "todo");
  
  logJobs <- as.logical(getOption("logJobs", TRUE));
  verbose <- as.logical(getOption("verbose", TRUE));
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Verify existance of "jobs" directory
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (!file.exists(jobsPath) || !file.info(jobsPath)$isdir)
    stop("Jobs path is not a directory: ", jobsPath);
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Move to "jobs" directory first...
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  on.exit({
    # Force all allocated jobs to finalize (release lock files etc). 
    job <- NULL; 
    gc();
  }, add=TRUE);
  opwd <- getwd(); 
  on.exit(setwd(opwd), add=TRUE);
  setwd(jobsPath);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # ...then start a new job
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  job <- Job$getRunAndFinishJob(
    todoPath=todoPath, 
    runningPath=runningPath, 
    finishedPath=finishedPath, 
    interruptedPath=interruptedPath, 
    failedPath=failedPath, 
    verbose=verbose
  );

  if (is.null(job)) {
    warning("No jobs found.");
    invisible(FALSE);
  }
} # main()

main();

###########################################################################
# HISTORY:
# 2004-07-21
# o Now making use of static getRunAndFinishJob() of the Job class.
# o BUG FIX: Did not query the correct names of the command line arguments.
# o BUG FIX: Forgot option 'finishedPath'.
# o Added HISTORY.
###########################################################################
