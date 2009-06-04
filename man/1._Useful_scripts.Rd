%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  901.UsefulScripts.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{1. Useful scripts}
\alias{1. Useful scripts}
\title{1. Useful scripts}


\description{
 Below are some useful scripts for command line processing.
}

\section{RJobBatch.bat - A Windows command prompt scripts}{
 Given that this script is in the \code{PATH} (or in the current
 directory), a job can be process by calling\cr

 \code{RJobBatch --root=jobs-ex}

 from the command prompt.\cr

 The script can be found by

 \code{system.file("misc", "RJobBatch.bat", package="R.batch")} and
 here is what it looks like:
 \preformatted{
 
echo off
rem ##################################################################
rem # RJobBatch
rem #
rem # Options: See help(main.JobBatch) for details.
rem #
rem # Windows script to run R one batch job utilizing R.batch.
rem #
rem # Author: Henrik Bengtsson, 2005
rem ##################################################################

rem # Create R script to start JobBatch$main() in the R.batch package.
echo library(R.batch); JobBatch$main(); > "\%TEMP\%\JobBatchMain.R"

rem # Start the R script and pass command line options
Rterm --vanilla --args \%1 \%2 \%3 \%4 \%5 \%6 \%7 \%8 \%9 < "\%TEMP\%\JobBatchMain.R"

rem # Clean up
del "\%TEMP\%\JobBatchMain.R"

 }\emph{}
}

\section{RJobBatch - A shell scripts}{
 Given that this script is in the \code{PATH} (or in the current
 directory), and has execute rights, a job can be process by calling\cr

 \code{RJobBatch --root=jobs-ex}

 from the command prompt.\cr

 The script can be found by

 \code{system.file("misc", "RJobBatch", package="R.batch")} and
 here is what it looks like:
 \preformatted{
 
#!/usr/bin/env sh
##################################################################
# RJobBatch
#
# Options: See help(main.JobBatch) for details.
#
# Shell script to run R one batch job utilizing R.batch.
#
# Author: Henrik Bengtsson, 2005
##################################################################

# Create R script to start JobBatch$main() in the R.batch package.
echo 'library(R.batch); JobBatch$main();' > /tmp/JobBatchMain.R

# Start the R script and pass command line options
R --vanilla --args $* < /tmp/JobBatchMain.R

# Clean up
rm /tmp/JobBatchMain.R

 }\emph{}
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}
\keyword{documentation}
