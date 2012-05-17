
echo off
rem ##################################################################
rem # RJobBatchServer
rem # 
rem # Options: See help("main.JobBatch") for details.
rem # 
rem # Windows script to run R one batch job utilizing R.batch.
rem #
rem # Author: Henrik Bengtsson, 2005-12
rem ##################################################################

rem # Create R script to start JobBatch$main() in the R.batch package.
echo R.batch::JobBatch$main(root="~/jobs/", maxJobs=Inf, details=-50, clean=TRUE) > "%TEMP%\JobBatchMain.R"

rem # Start the R script and pass command line options
Rterm --vanilla --args %1 %2 %3 %4 %5 %6 %7 %8 %9 < "%TEMP%\JobBatchMain.R"

rem # Clean up
del "%TEMP%\JobBatchMain.R"
