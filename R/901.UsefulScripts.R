#########################################################################/**
# @RdocDocumentation "1. Useful scripts"
#
# \description{
#  Below are some useful scripts for command line processing.
# }
#
# \section{RJobBatch.bat - A Windows command prompt scripts}{
#  Given that this script is in the \code{PATH} (or in the current 
#  directory), a job can be process by calling\cr
#
#  \code{RJobBatch --root=jobs-mandelbroot}
#
#  from the command prompt.\cr
#
#  The script can be found by 
#
#  \code{system.file("bin", "RJobBatch.bat", package="R.batch")} and
#  here is what it looks like:
#  \preformatted{
#   @include "../inst/bin/RJobBatch.bat"
#  }\emph{}
#
#  To add this automatically to the Windows search PATH, you can 
#  call the following in your Windows Command line startup script:
#  \preformatted{
#     Rterm --slave -e "cat(system.file('bin', package='R.batch'))" > %TEMP%\DIR.tmp
#     for /F "delims=" %%G in (%TEMP%\DIR.tmp) do set dir=%%~sG
#     IF EXIST "%dir%" (
#       echo Detected R.batch package - added to the search path.
#       path %PATH%;%dir%
#     )
#   }
# }
#
# \section{RJobBatch - A Unix shell scripts}{
#  Given that this script is in the \code{PATH} (or in the current 
#  directory), and has execute rights, a job can be process by calling\cr
#
#  \code{RJobBatch --root=jobs-mandelbroot}
#
#  from the command prompt.\cr
#
#  The script can be found by 
#
#  \code{system.file("bin", "RJobBatch", package="R.batch")} and
#  here is what it looks like:
#  \preformatted{
#   @include "../inst/bin/RJobBatch"
#  }\emph{}
#
#  To add this automatically to the Unix search PATH, you can 
#  call the following in your shell startup script:
#  \preformatted{
#     dir=`R --slave -e "cat(system.file('bin', package='R.batch'))"`
#     if test -d "${dir}" (
#       echo Detected R.batch package - added to the search path.
#       setenv path=${path};${dir}
#     )
#   }
# }
#
# @author
#*/#########################################################################  

