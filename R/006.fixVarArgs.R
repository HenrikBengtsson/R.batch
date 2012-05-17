# Added '...' to some base functions. These will later be
# turned into default functions by setMethodS3().

# USED TO DO: sink <- appendVarArgs(sink)
sink <- function(...) UseMethod("sink");
setMethodS3("sink", "default", function(...) {
  base::sink(...);
})


############################################################################
# HISTORY:
# 2012-05-17
# o Replaced all appendVarArgs() for 'base' functions that do .Internal()
#   calls, because they would then appear as local functions of this
#   package and hence not be accepted by CRAN according to their new
#   policies.  Instead we now create "default" functions that are
#   wrappers to the corresponding functions in the 'base' package.
# 2005-03-02
# o Created to please R CMD check.
############################################################################
