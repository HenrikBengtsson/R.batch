####################################################################
# Files named .Requirements.R are special. If they exists, they are
# sourced before any other *.R files. For a Job to be run, all such
# files must produce the value TRUE. All other values and errors
# are equivalent to FALSE.
#
# If one in the common source directory src/ gives FALSE, then
# run() in JobBatch returns list() immediately, since no other
# Job will pass the test either.
#################################################################### 
getRversion() >= "2.0.0" &&
require(R.utils)
