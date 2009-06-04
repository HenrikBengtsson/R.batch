# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# This code will be evaluated whenever sourceHotCode() is called by the Job.
# Note that the code will be evaluated in the calling environment, which
# in this example is onRun().
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

cat("In patch.R...\n");

if (!exists("patchCount"))
  patchCount <- 0;
patchCount <- patchCount + 1;

# Change the title
title <- sprintf("Mandelbrot w/ patch #%d", as.integer(patchCount));

cat("In patch.R...done\n");
