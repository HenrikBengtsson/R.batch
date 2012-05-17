%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  999.package.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{R.batch-package}
\alias{R.batch-package}
\alias{R.batch}
\docType{package}

\title{Package R.batch}


\description{
  The package provides methods to batch run R scripts in a straightforward manner. A job batch is defined by a root directory containing subdirectories todo/, running/, finished/, failed/, src/ among others. A job to be processed is a directory placed in todo/. A job is processed by moving it to running/, then sourcing the R files in src/ (containing common code) and then the ones in the job directory (contain job specific code), in which a function onRun() has to be defined, which is then called. Succesfully jobs end up in finished/ whereas failed jobs in failed/.

  This package should be considered to be in "beta" version.
}

\section{Installation}{
  To install this package, see \url{https://r-forge.r-project.org/R/?group_id=428}.
  Required packages are installed from CRAN.
}

\section{To get started}{
  To get started, see:
  \enumerate{
    \item \code{\link{JobBatch}}.
    \item \code{\link[=main.JobBatch]{JobBatch$main()}} - to start a job from the command line.
  }
}

\section{How to cite this package}{
  To cite this package, please use:
\preformatted{
@Manual{,
  title = {R.batch: Methods to run batch jobs in R},
  author = {Henrik Bengtsson},
  year = {2012},
  note = {R package version 0.6.1},
  url = {http://www.braju.com/R/},
}
}
}

\author{
 Henrik Bengtsson.
}

\section{License}{
 LGPL (>= 2.1).
}
\keyword{package}
