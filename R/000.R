##############################################################################
# This code has to come first in a library. To do this make sure this file
# is named "000.R" (zeros).
##############################################################################

if (R.Version()$major < 2) {
  require(R.oo)    || stop("Could not load package: R.oo");
  require(R.utils) || throw("Package not found: R.utils");
  options(dontWarnPkgs=unique(c("R.oo", "R.utils", 
                                "base", "datasets", "graphics", "grDevices", 
       "methods", "stats", "utils", "Autoloads", getOption("dontWarnPkgs"))))
} else {
  # Is autoload() allowed in R v2.0.0 or higher? According to the help one
  # should not use require(). These methods are need to load the package.
  autoload("appendVarArgs", package="R.oo")
  autoload("hasVarArgs", package="R.oo")
  autoload("setMethodS3", package="R.oo")
  autoload("setConstructorS3", package="R.oo")
}
